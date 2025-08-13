#!/usr/bin/env python3
"""
模型性能评估脚本

用于评估训练好的RNN模型在测试数据上的性能，计算精确率、召回率和F1分数。
使用BasePredictor基类，提供全面的评估功能。
"""

import os
import sys
import json
import argparse
import numpy as np
from sklearn.metrics import precision_score, recall_score, f1_score, confusion_matrix, classification_report
from sklearn.metrics import accuracy_score

# 添加当前目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from base_predictor import BasePredictor
from data_loader import NLLDataset

class ModelEvaluator:
    """模型评估器，基于BasePredictor实现评估功能"""
    
    def __init__(self, model_path, device='cpu'):
        """
        初始化评估器
        
        Args:
            model_path: 模型权重文件路径
            device: 推理设备
        """
        self.predictor = BasePredictor(model_path, device)
        self.device = device
        self.model_path = model_path
    
    def load_test_data(self, human_file, model_file, max_length=None):
        """
        加载测试数据
        
        Args:
            human_file: 人类文本的NLL分数文件
            model_file: 模型生成文本的NLL分数文件
            max_length: 最大序列长度
            
        Returns:
            sequences: 输入序列列表
            labels: 标签列表 (0: 人类, 1: 模型)
            lengths: 序列长度列表
        """
        # 使用NLLDataset加载数据
        dataset = NLLDataset(human_file, model_file, max_length=max_length)
        
        sequences = []
        labels = []
        
        # 提取数据
        for i in range(len(dataset)):
            seq_tensor, label_tensor = dataset[i]
            sequences.append(seq_tensor.numpy().tolist())
            labels.append(label_tensor.item())
        
        lengths = [len(seq) for seq in sequences]
        
        return sequences, labels, lengths
    
    def compute_metrics(self, true_labels, predictions):
        """
        计算评估指标
        
        Args:
            true_labels: 真实标签
            predictions: 预测标签
            
        Returns:
            metrics: 评估指标字典
        """
        # 基本指标
        accuracy = accuracy_score(true_labels, predictions)
        precision = precision_score(true_labels, predictions, average='weighted')
        recall = recall_score(true_labels, predictions, average='weighted')
        f1 = f1_score(true_labels, predictions, average='weighted')
        
        # 计算每个类别的指标
        precision_per_class = precision_score(true_labels, predictions, average=None)
        recall_per_class = recall_score(true_labels, predictions, average=None)
        f1_per_class = f1_score(true_labels, predictions, average=None)
        
        # 混淆矩阵
        cm = confusion_matrix(true_labels, predictions)
        
        # 分类报告
        report = classification_report(
            true_labels, predictions, 
            target_names=['Human', 'Model'],
            output_dict=True
        )
        
        metrics = {
            'accuracy': accuracy,
            'precision_weighted': precision,
            'recall_weighted': recall,
            'f1_weighted': f1,
            'precision_per_class': precision_per_class.tolist(),
            'recall_per_class': recall_per_class.tolist(),
            'f1_per_class': f1_per_class.tolist(),
            'confusion_matrix': cm.tolist(),
            'classification_report': report,
            'sample_counts': {
                'total': len(true_labels),
                'human': sum(1 for label in true_labels if label == 0),
                'model': sum(1 for label in true_labels if label == 1)
            }
        }
        
        return metrics
    
    def evaluate(self, human_file, model_file, max_length=None, batch_size=32):
        """
        评估模型性能
        
        Args:
            human_file: 人类文本的NLL分数文件
            model_file: 模型生成文本的NLL分数文件
            max_length: 最大序列长度
            batch_size: 批处理大小
            
        Returns:
            metrics: 评估指标字典
            true_labels: 真实标签
            predictions: 预测标签
            probabilities: 预测概率
        """
        print(f"加载测试数据...")
        print(f"  人类数据: {human_file}")
        print(f"  模型数据: {model_file}")
        
        # 加载测试数据
        sequences, true_labels, lengths = self.load_test_data(
            human_file, model_file, max_length
        )
        
        print(f"  总样本数: {len(sequences)}")
        print(f"  人类样本: {sum(1 for label in true_labels if label == 0)}")
        print(f"  模型样本: {sum(1 for label in true_labels if label == 1)}")
        
        # 进行预测
        print(f"进行预测...")
        predictions, probabilities = self.predictor.predict_batch(
            sequences, lengths=lengths, batch_size=batch_size, max_length=max_length
        )
        
        # 计算评估指标
        metrics = self.compute_metrics(true_labels, predictions)
        
        return metrics, true_labels, predictions, probabilities
    
    def analyze_errors(self, true_labels, predictions, probabilities, sequences=None, top_k=10):
        """
        分析预测错误
        
        Args:
            true_labels: 真实标签
            predictions: 预测标签
            probabilities: 预测概率
            sequences: 原始序列（可选）
            top_k: 显示前k个错误案例
            
        Returns:
            error_analysis: 错误分析结果
        """
        errors = []
        
        for i, (true_label, pred_label, prob) in enumerate(zip(true_labels, predictions, probabilities)):
            if true_label != pred_label:
                confidence = prob[pred_label]
                error_info = {
                    'index': i,
                    'true_label': true_label,
                    'predicted_label': pred_label,
                    'confidence': confidence,
                    'true_class': 'Human' if true_label == 0 else 'Model',
                    'predicted_class': 'Human' if pred_label == 0 else 'Model'
                }
                
                if sequences:
                    error_info['sequence_length'] = len(sequences[i])
                    error_info['sequence_stats'] = {
                        'mean': np.mean(sequences[i]),
                        'std': np.std(sequences[i]),
                        'min': np.min(sequences[i]),
                        'max': np.max(sequences[i])
                    }
                
                errors.append(error_info)
        
        # 按置信度排序（高置信度的错误更值得关注）
        errors.sort(key=lambda x: x['confidence'], reverse=True)
        
        error_analysis = {
            'total_errors': len(errors),
            'error_rate': len(errors) / len(true_labels),
            'high_confidence_errors': errors[:top_k],
            'error_by_class': {
                'human_as_model': sum(1 for e in errors if e['true_label'] == 0),
                'model_as_human': sum(1 for e in errors if e['true_label'] == 1)
            }
        }
        
        return error_analysis

def print_results(metrics, show_detailed=True):
    """打印评估结果"""
    print("\n" + "=" * 80)
    print("模型性能评估结果")
    print("=" * 80)
    
    # 总体指标
    print(f"准确率 (Accuracy): {metrics['accuracy']:.4f} ({metrics['accuracy']*100:.2f}%)")
    print(f"精确率 (Precision): {metrics['precision_weighted']:.4f} ({metrics['precision_weighted']*100:.2f}%)")
    print(f"召回率 (Recall): {metrics['recall_weighted']:.4f} ({metrics['recall_weighted']*100:.2f}%)")
    print(f"F1分数 (F1-Score): {metrics['f1_weighted']:.4f} ({metrics['f1_weighted']*100:.2f}%)")
    
    if show_detailed:
        print("\n" + "-" * 50)
        print("各类别详细指标:")
        print("-" * 50)
        
        classes = ['Human', 'Model']
        for i, class_name in enumerate(classes):
            print(f"{class_name}:")
            print(f"  精确率: {metrics['precision_per_class'][i]:.4f} ({metrics['precision_per_class'][i]*100:.2f}%)")
            print(f"  召回率: {metrics['recall_per_class'][i]:.4f} ({metrics['recall_per_class'][i]*100:.2f}%)")
            print(f"  F1分数: {metrics['f1_per_class'][i]:.4f} ({metrics['f1_per_class'][i]*100:.2f}%)")
        
        print("\n" + "-" * 50)
        print("混淆矩阵:")
        print("-" * 50)
        cm = np.array(metrics['confusion_matrix'])
        print("         预测")
        print("       Human  Model")
        print(f"真实 Human  {cm[0,0]:4d}  {cm[0,1]:4d}")
        print(f"     Model  {cm[1,0]:4d}  {cm[1,1]:4d}")
    
    print("\n" + "-" * 50)
    print("样本统计:")
    print("-" * 50)
    print(f"总样本数: {metrics['sample_counts']['total']}")
    print(f"人类样本: {metrics['sample_counts']['human']}")
    print(f"模型样本: {metrics['sample_counts']['model']}")

def print_error_analysis(error_analysis, show_details=True):
    """打印错误分析结果"""
    print("\n" + "=" * 80)
    print("错误分析")
    print("=" * 80)
    
    print(f"总错误数: {error_analysis['total_errors']}")
    print(f"错误率: {error_analysis['error_rate']:.4f} ({error_analysis['error_rate']*100:.2f}%)")
    
    print(f"\n错误分布:")
    print(f"  人类被误判为模型: {error_analysis['error_by_class']['human_as_model']}")
    print(f"  模型被误判为人类: {error_analysis['error_by_class']['model_as_human']}")
    
    if show_details and error_analysis['high_confidence_errors']:
        print(f"\n高置信度错误案例 (前{len(error_analysis['high_confidence_errors'])}个):")
        print("-" * 70)
        
        for i, error in enumerate(error_analysis['high_confidence_errors']):
            print(f"错误 {i+1}:")
            print(f"  索引: {error['index']}")
            print(f"  真实类别: {error['true_class']}")
            print(f"  预测类别: {error['predicted_class']}")
            print(f"  置信度: {error['confidence']:.4f}")
            
            if 'sequence_stats' in error:
                stats = error['sequence_stats']
                print(f"  序列长度: {error['sequence_length']}")
                print(f"  序列统计: 均值={stats['mean']:.2f}, 标准差={stats['std']:.2f}, "
                      f"最小值={stats['min']:.2f}, 最大值={stats['max']:.2f}")
            print()

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='评估RNN模型性能')
    parser.add_argument('--model_path', type=str, required=True,
                       help='模型权重文件路径')
    parser.add_argument('--human_file', type=str, required=True,
                       help='人类文本的NLL分数文件')
    parser.add_argument('--model_file', type=str, required=True,
                       help='模型生成文本的NLL分数文件')
    parser.add_argument('--max_length', type=int, default=None,
                       help='最大序列长度')
    parser.add_argument('--batch_size', type=int, default=32,
                       help='批处理大小')
    parser.add_argument('--device', type=str, default='cpu',
                       help='推理设备')
    parser.add_argument('--output', type=str, default=None,
                       help='输出结果文件路径')
    parser.add_argument('--analyze_errors', action='store_true',
                       help='进行错误分析')
    parser.add_argument('--show_detailed', action='store_true', default=True,
                       help='显示详细结果')
    
    args = parser.parse_args()
    
    # 检查文件是否存在
    if not os.path.exists(args.model_path):
        print(f"错误: 模型文件不存在 - {args.model_path}")
        return
    
    if not os.path.exists(args.human_file):
        print(f"错误: 人类数据文件不存在 - {args.human_file}")
        return
    
    if not os.path.exists(args.model_file):
        print(f"错误: 模型数据文件不存在 - {args.model_file}")
        return
    
    print("RNN模型性能评估")
    print("=" * 50)
    print(f"模型路径: {args.model_path}")
    print(f"人类数据: {args.human_file}")
    print(f"模型数据: {args.model_file}")
    print(f"设备: {args.device}")
    if args.max_length:
        print(f"最大序列长度: {args.max_length}")
    print()
    
    # 创建评估器
    try:
        evaluator = ModelEvaluator(args.model_path, args.device)
        
        # 评估模型
        metrics, true_labels, predictions, probabilities = evaluator.evaluate(
            args.human_file, args.model_file, args.max_length, args.batch_size
        )
        
        # 打印结果
        print_results(metrics, args.show_detailed)
        
        # 错误分析
        if args.analyze_errors:
            sequences, _, _ = evaluator.load_test_data(
                args.human_file, args.model_file, args.max_length
            )
            error_analysis = evaluator.analyze_errors(
                true_labels, predictions, probabilities, sequences
            )
            print_error_analysis(error_analysis)
        
        # 保存结果
        if args.output:
            # 转换numpy类型为Python原生类型
            def convert_numpy_types(obj):
                if isinstance(obj, np.integer):
                    return int(obj)
                elif isinstance(obj, np.floating):
                    return float(obj)
                elif isinstance(obj, np.ndarray):
                    return obj.tolist()
                elif isinstance(obj, dict):
                    return {key: convert_numpy_types(value) for key, value in obj.items()}
                elif isinstance(obj, list):
                    return [convert_numpy_types(item) for item in obj]
                else:
                    return obj
            
            output_data = {
                'model_path': args.model_path,
                'human_file': args.human_file,
                'model_file': args.model_file,
                'metrics': convert_numpy_types(metrics),
                'args': vars(args)
            }
            
            if args.analyze_errors:
                output_data['error_analysis'] = convert_numpy_types(error_analysis)
            
            with open(args.output, 'w', encoding='utf-8') as f:
                json.dump(output_data, f, indent=2, ensure_ascii=False)
            
            print(f"\n结果已保存到: {args.output}")
        
    except Exception as e:
        print(f"评估过程中出现错误: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
