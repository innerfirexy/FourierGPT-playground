#!/usr/bin/env python3
"""
跨域RNN分类器训练脚本

用于训练特定domain和模型版本的RNN分类器，支持小数据集优化。
"""

import torch
import torch.nn as nn
import torch.optim as optim
import numpy as np
import os
import sys
import json
import argparse
from datetime import datetime
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, precision_recall_fscore_support, roc_auc_score
from tqdm import tqdm

# 添加当前目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from model import BiLSTMClassifier
from data_loader import NLLDataset, create_data_loaders
from trainer import Trainer

class DomainClassifierTrainer:
    """域分类器训练器，针对小数据集优化"""
    
    def __init__(self, config):
        """
        初始化训练器
        
        Args:
            config: 训练配置字典
        """
        self.config = config
        self.device = torch.device(config.get('device', 'cpu'))
        
        # 创建输出目录
        os.makedirs(config['output_dir'], exist_ok=True)
        
        # 保存配置
        with open(os.path.join(config['output_dir'], 'config.json'), 'w') as f:
            json.dump(config, f, indent=2)
    
    def create_model(self):
        """创建模型"""
        model = BiLSTMClassifier(
            input_size=self.config['model']['input_size'],
            hidden_size=self.config['model']['hidden_size'],
            num_layers=self.config['model']['num_layers'],
            num_classes=self.config['model']['num_classes'],
            dropout=self.config['model']['dropout'],
            bidirectional=self.config['model']['bidirectional']
        )
        return model
    
    def train_single_fold(self, train_loader, val_loader, fold_idx):
        """
        训练单个fold
        
        Args:
            train_loader: 训练数据加载器
            val_loader: 验证数据加载器
            fold_idx: fold索引
            
        Returns:
            best_model: 最佳模型
            training_history: 训练历史
        """
        # 创建模型和训练器
        model = self.create_model()
        trainer = Trainer(model, self.device)
        
        # 优化器和调度器
        optimizer = optim.Adam(
            model.parameters(), 
            lr=self.config['training']['learning_rate'],
            weight_decay=self.config['training'].get('weight_decay', 1e-5)
        )
        
        scheduler = optim.lr_scheduler.ReduceLROnPlateau(
            optimizer, 
            mode='max', 
            factor=0.5, 
            patience=5
        )
        
        # 早停
        best_val_acc = 0
        patience_counter = 0
        best_model_state = None
        
        print(f"开始训练 Fold {fold_idx + 1}")
        
        for epoch in tqdm(range(self.config['training']['num_epochs']), 
                         desc=f"Fold {fold_idx + 1} training", 
                         leave=False):
            # 训练
            train_loss, train_acc = trainer.train_epoch(train_loader, optimizer)
            
            # 验证
            val_loss, val_acc, _, _ = trainer.validate(val_loader)
            
            # 学习率调度
            scheduler.step(val_acc)
            
            # 早停检查
            if val_acc > best_val_acc:
                best_val_acc = val_acc
                patience_counter = 0
                best_model_state = model.state_dict().copy()
            else:
                patience_counter += 1
            
            if epoch % 10 == 0:
                print(f"Epoch {epoch}: Train Loss={train_loss:.4f}, Train Acc={train_acc:.4f}, "
                      f"Val Loss={val_loss:.4f}, Val Acc={val_acc:.4f}")
            
            # 早停
            if patience_counter >= self.config['training']['early_stopping_patience']:
                print(f"早停在 epoch {epoch}, 最佳验证准确率: {best_val_acc:.4f}")
                break
        
        # 恢复最佳模型
        if best_model_state is not None:
            model.load_state_dict(best_model_state)
        
        return model, trainer.train_losses, trainer.val_losses, trainer.train_accuracies, trainer.val_accuracies
    
    def cross_validate(self, human_file, model_file):
        """
        执行交叉验证训练
        
        Args:
            human_file: 人类数据文件路径
            model_file: 模型数据文件路径
            
        Returns:
            results: 交叉验证结果
        """
        print(f"开始交叉验证训练")
        print(f"人类数据: {human_file}")
        print(f"模型数据: {model_file}")
        
        # 加载完整数据集
        dataset = NLLDataset(
            human_file, 
            model_file, 
            max_length=self.config['data'].get('max_length')
        )
        
        print(f"数据集大小: {len(dataset)}")
        
        # 准备交叉验证
        n_splits = self.config['training']['cross_validation_folds']
        skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=42)
        
        # 获取所有标签用于分层
        all_labels = []
        for i in range(len(dataset)):
            _, label = dataset[i]
            all_labels.append(label.item())
        all_labels = np.array(all_labels)
        
        fold_results = []
        
        for fold_idx, (train_indices, val_indices) in enumerate(tqdm(skf.split(range(len(dataset)), all_labels), 
                                                                   total=n_splits, 
                                                                   desc="Cross-validation folds")):
            print(f"\n=== Fold {fold_idx + 1}/{n_splits} ===")
            
            # 创建fold数据集
            train_dataset = torch.utils.data.Subset(dataset, train_indices)
            val_dataset = torch.utils.data.Subset(dataset, val_indices)
            
            # 创建数据加载器
            from data_loader import collate_fn
            
            train_loader = torch.utils.data.DataLoader(
                train_dataset,
                batch_size=self.config['training']['batch_size'],
                shuffle=True,
                collate_fn=collate_fn,
                num_workers=0
            )
            
            val_loader = torch.utils.data.DataLoader(
                val_dataset,
                batch_size=self.config['training']['batch_size'],
                shuffle=False,
                collate_fn=collate_fn,
                num_workers=0
            )
            
            # 训练fold
            model, train_losses, val_losses, train_accs, val_accs = self.train_single_fold(
                train_loader, val_loader, fold_idx
            )
            
            # 评估fold
            model.eval()
            val_predictions = []
            val_true_labels = []
            val_probabilities = []
            
            with torch.no_grad():
                for sequences, lengths, labels in val_loader:
                    sequences = sequences.to(self.device)
                    lengths = lengths.to(self.device)
                    labels = labels.to(self.device)
                    
                    outputs = model(sequences, lengths)
                    probabilities = torch.softmax(outputs, dim=1)
                    predictions = torch.argmax(outputs, dim=1)
                    
                    val_predictions.extend(predictions.cpu().numpy())
                    val_true_labels.extend(labels.cpu().numpy())
                    val_probabilities.extend(probabilities.cpu().numpy())
            
            # 计算指标
            val_predictions = np.array(val_predictions)
            val_true_labels = np.array(val_true_labels)
            val_probabilities = np.array(val_probabilities)
            
            accuracy = accuracy_score(val_true_labels, val_predictions)
            precision, recall, f1, _ = precision_recall_fscore_support(
                val_true_labels, val_predictions, average='weighted'
            )
            
            try:
                auc = roc_auc_score(val_true_labels, val_probabilities[:, 1])
            except:
                auc = 0.5  # 如果计算失败，使用默认值
            
            fold_result = {
                'fold': fold_idx + 1,
                'accuracy': accuracy,
                'precision': precision,
                'recall': recall,
                'f1': f1,
                'auc': auc,
                'train_losses': train_losses,
                'val_losses': val_losses,
                'train_accuracies': train_accs,
                'val_accuracies': val_accs
            }
            
            fold_results.append(fold_result)
            
            print(f"Fold {fold_idx + 1} 结果:")
            print(f"  准确率: {accuracy:.4f}")
            print(f"  精确率: {precision:.4f}")
            print(f"  召回率: {recall:.4f}")
            print(f"  F1分数: {f1:.4f}")
            print(f"  AUC: {auc:.4f}")
            
            # 保存fold模型
            torch.save({
                'model_state_dict': model.state_dict(),
                'config': self.config,
                'fold': fold_idx + 1,
                'metrics': fold_result
            }, os.path.join(self.config['output_dir'], f'fold_{fold_idx + 1}_model.pth'))
        
        # 计算平均结果
        avg_results = {
            'mean_accuracy': np.mean([r['accuracy'] for r in fold_results]),
            'std_accuracy': np.std([r['accuracy'] for r in fold_results]),
            'mean_precision': np.mean([r['precision'] for r in fold_results]),
            'std_precision': np.std([r['precision'] for r in fold_results]),
            'mean_recall': np.mean([r['recall'] for r in fold_results]),
            'std_recall': np.std([r['recall'] for r in fold_results]),
            'mean_f1': np.mean([r['f1'] for r in fold_results]),
            'std_f1': np.std([r['f1'] for r in fold_results]),
            'mean_auc': np.mean([r['auc'] for r in fold_results]),
            'std_auc': np.std([r['auc'] for r in fold_results]),
            'fold_results': fold_results
        }
        
        print(f"\n=== 交叉验证总结 ===")
        print(f"平均准确率: {avg_results['mean_accuracy']:.4f} ± {avg_results['std_accuracy']:.4f}")
        print(f"平均精确率: {avg_results['mean_precision']:.4f} ± {avg_results['std_precision']:.4f}")
        print(f"平均召回率: {avg_results['mean_recall']:.4f} ± {avg_results['std_recall']:.4f}")
        print(f"平均F1分数: {avg_results['mean_f1']:.4f} ± {avg_results['std_f1']:.4f}")
        print(f"平均AUC: {avg_results['mean_auc']:.4f} ± {avg_results['std_auc']:.4f}")
        
        # 保存结果
        with open(os.path.join(self.config['output_dir'], 'cv_results.json'), 'w') as f:
            json.dump(avg_results, f, indent=2)
        
        return avg_results

def create_config(args):
    """根据命令行参数创建配置"""
    config = {
        'domain': args.domain,
        'model_version': args.model_version,
        'output_dir': args.output_dir,
        'device': args.device,
        'model': {
            'input_size': 1,
            'hidden_size': args.hidden_size,
            'num_layers': args.num_layers,
            'num_classes': 2,
            'dropout': args.dropout,
            'bidirectional': True
        },
        'training': {
            'batch_size': args.batch_size,
            'learning_rate': args.learning_rate,
            'num_epochs': args.num_epochs,
            'early_stopping_patience': args.early_stopping_patience,
            'weight_decay': args.weight_decay,
            'cross_validation_folds': args.cv_folds
        },
        'data': {
            'max_length': args.max_length
        },
        'timestamp': datetime.now().isoformat()
    }
    return config

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='训练域分类器')
    parser.add_argument('--domain', type=str, required=True,
                       choices=['harmful', 'peerread', 'pubmed', 'writing', 'xsum'],
                       help='数据域')
    parser.add_argument('--model_version', type=str, required=True,
                       help='模型版本 (如: claude-3-haiku-20240307)')
    parser.add_argument('--file_model_version', type=str, required=True,
                       help='文件名中的模型版本 (如: claude-3-haiku-20240307)')
    parser.add_argument('--output_dir', type=str, required=True,
                       help='输出目录')
    parser.add_argument('--data_dir', type=str, default="../data/Claude/Claude-Haiku",
                       help='数据目录路径')
    
    # 模型参数
    parser.add_argument('--hidden_size', type=int, default=64,
                       help='LSTM隐藏层大小')
    parser.add_argument('--num_layers', type=int, default=1,
                       help='LSTM层数')
    parser.add_argument('--dropout', type=float, default=0.3,
                       help='Dropout概率')
    
    # 训练参数
    parser.add_argument('--batch_size', type=int, default=16,
                       help='批处理大小')
    parser.add_argument('--learning_rate', type=float, default=0.001,
                       help='学习率')
    parser.add_argument('--num_epochs', type=int, default=50,
                       help='训练轮数')
    parser.add_argument('--early_stopping_patience', type=int, default=10,
                       help='早停patience')
    parser.add_argument('--weight_decay', type=float, default=1e-5,
                       help='权重衰减')
    parser.add_argument('--cv_folds', type=int, default=5,
                       help='交叉验证折数')
    
    # 数据参数
    parser.add_argument('--max_length', type=int, default=None,
                       help='最大序列长度')
    parser.add_argument('--device', type=str, default='cpu',
                       help='训练设备')
    
    args = parser.parse_args()
    
    # 构建数据文件路径
    data_dir = args.data_dir
    human_file = os.path.join(data_dir, f"{args.domain}_{args.file_model_version}_human.txt")
    model_file = os.path.join(data_dir, f"{args.domain}_{args.file_model_version}_model.txt")
    
    # 检查文件是否存在
    if not os.path.exists(human_file):
        print(f"错误: 人类数据文件不存在 - {human_file}")
        return
    
    if not os.path.exists(model_file):
        print(f"错误: 模型数据文件不存在 - {model_file}")
        return
    
    print(f"域分类器训练")
    print(f"域: {args.domain}")
    print(f"模型版本: {args.model_version}")
    print(f"输出目录: {args.output_dir}")
    
    # 创建配置并训练
    config = create_config(args)
    trainer = DomainClassifierTrainer(config)
    
    try:
        results = trainer.cross_validate(human_file, model_file)
        print(f"\n训练完成! 结果保存在: {args.output_dir}")
        
    except Exception as e:
        print(f"训练过程中出现错误: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
