#!/usr/bin/env python3
"""
训练Claude-Sonnet数据的LSTM分类器
使用2024-02-29的数据训练，测试在后续时间点的表现
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader
import numpy as np
import os
import sys
import time
from datetime import datetime
import json
import matplotlib.pyplot as plt

# 添加当前目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from data_loader import NLLDataset, collate_fn, create_data_loaders
from model import BiLSTMClassifier, SimpleLSTMClassifier

def train_model(model, train_loader, val_loader, criterion, optimizer, 
                num_epochs, device, save_dir):
    """训练模型"""
    
    train_losses = []
    val_losses = []
    train_accuracies = []
    val_accuracies = []
    
    best_val_accuracy = 0.0
    best_model_path = None
    
    print(f"开始训练，共 {num_epochs} 个epoch...")
    
    for epoch in range(num_epochs):
        # 训练阶段
        model.train()
        train_loss = 0.0
        train_correct = 0
        train_total = 0
        
        for batch_idx, (sequences, lengths, labels) in enumerate(train_loader):
            sequences = sequences.to(device)
            labels = labels.to(device)
            
            optimizer.zero_grad()
            outputs = model(sequences, lengths)
            loss = criterion(outputs, labels)
            loss.backward()
            optimizer.step()
            
            train_loss += loss.item()
            _, predicted = torch.max(outputs.data, 1)
            train_total += labels.size(0)
            train_correct += (predicted == labels).sum().item()
            
            if batch_idx % 10 == 0:
                print(f'Epoch [{epoch+1}/{num_epochs}], Batch [{batch_idx}/{len(train_loader)}], '
                      f'Loss: {loss.item():.4f}')
        
        train_accuracy = 100 * train_correct / train_total
        train_losses.append(train_loss / len(train_loader))
        train_accuracies.append(train_accuracy)
        
        # 验证阶段
        model.eval()
        val_loss = 0.0
        val_correct = 0
        val_total = 0
        
        with torch.no_grad():
            for sequences, lengths, labels in val_loader:
                sequences = sequences.to(device)
                labels = labels.to(device)
                
                outputs = model(sequences, lengths)
                loss = criterion(outputs, labels)
                
                val_loss += loss.item()
                _, predicted = torch.max(outputs.data, 1)
                val_total += labels.size(0)
                val_correct += (predicted == labels).sum().item()
        
        val_accuracy = 100 * val_correct / val_total
        val_losses.append(val_loss / len(val_loader))
        val_accuracies.append(val_accuracy)
        
        print(f'Epoch [{epoch+1}/{num_epochs}], '
              f'Train Loss: {train_losses[-1]:.4f}, Train Acc: {train_accuracy:.2f}%, '
              f'Val Loss: {val_losses[-1]:.4f}, Val Acc: {val_accuracy:.2f}%')
        
        # 保存最佳模型
        if val_accuracy > best_val_accuracy:
            best_val_accuracy = val_accuracy
            best_model_path = os.path.join(save_dir, f'best_model_epoch_{epoch+1}.pth')
            
            torch.save({
                'epoch': epoch + 1,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),
                'train_loss': train_losses[-1],
                'val_loss': val_losses[-1],
                'train_accuracy': train_accuracy,
                'val_accuracy': val_accuracy,
                'best_val_accuracy': best_val_accuracy
            }, best_model_path)
            
            print(f'保存最佳模型: {best_model_path}')
    
    return train_losses, val_losses, train_accuracies, val_accuracies, best_model_path

def plot_training_curves(train_losses, val_losses, train_accuracies, val_accuracies, save_dir):
    """绘制训练曲线"""
    
    epochs = range(1, len(train_losses) + 1)
    
    plt.figure(figsize=(12, 5))
    
    # 损失曲线
    plt.subplot(1, 2, 1)
    plt.plot(epochs, train_losses, 'b-', label='Training Loss')
    plt.plot(epochs, val_losses, 'r-', label='Validation Loss')
    plt.title('Training and Validation Loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.legend()
    plt.grid(True)
    
    # 准确率曲线
    plt.subplot(1, 2, 2)
    plt.plot(epochs, train_accuracies, 'b-', label='Training Accuracy')
    plt.plot(epochs, val_accuracies, 'r-', label='Validation Accuracy')
    plt.title('Training and Validation Accuracy')
    plt.xlabel('Epoch')
    plt.ylabel('Accuracy (%)')
    plt.legend()
    plt.grid(True)
    
    plt.tight_layout()
    plt.savefig(os.path.join(save_dir, 'training_curves.png'), dpi=300, bbox_inches='tight')
    plt.savefig(os.path.join(save_dir, 'training_curves.pdf'), bbox_inches='tight')
    plt.close()

def main():
    """主函数"""
    
    # 配置参数
    config = {
        'model_type': 'bilstm',  # 'bilstm' 或 'simple'
        'hidden_size': 128,
        'num_layers': 2,
        'dropout': 0.5,
        'bidirectional': True,
        'batch_size': 32,
        'learning_rate': 0.001,
        'num_epochs': 15,
        'train_split': 0.8,
        'val_split': 0.1,
        'max_length': None,  # None表示使用所有数据
        'device': 'cpu'
    }
    
    # 数据路径
    train_human_file = "claude_sonnet_training/train_human.txt"
    train_model_file = "claude_sonnet_training/train_model.txt"
    
    # 创建输出目录
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = f"outputs/claude_sonnet_run_{timestamp}"
    os.makedirs(output_dir, exist_ok=True)
    
    # 保存配置
    with open(os.path.join(output_dir, 'config.json'), 'w') as f:
        json.dump(config, f, indent=2)
    
    print(f"输出目录: {output_dir}")
    print(f"配置: {config}")
    
    # 设置设备
    device = torch.device(config['device'])
    print(f"使用设备: {device}")
    
    # 创建数据加载器
    print("创建数据加载器...")
    train_loader, val_loader, test_loader = create_data_loaders(
        human_file=train_human_file,
        chatgpt_file=train_model_file,
        batch_size=config['batch_size'],
        train_split=config['train_split'],
        val_split=config['val_split'],
        max_length=config['max_length'],
        shuffle=True,
        num_workers=0
    )
    
    print(f"训练集: {len(train_loader.dataset)} 样本")
    print(f"验证集: {len(val_loader.dataset)} 样本")
    print(f"测试集: {len(test_loader.dataset)} 样本")
    
    # 创建模型
    print("创建模型...")
    if config['model_type'] == 'bilstm':
        model = BiLSTMClassifier(
            input_size=1,
            hidden_size=config['hidden_size'],
            num_layers=config['num_layers'],
            num_classes=2,
            dropout=config['dropout'],
            bidirectional=config['bidirectional']
        )
    else:
        model = SimpleLSTMClassifier(
            input_size=1,
            hidden_size=config['hidden_size'],
            num_classes=2,
            dropout=config['dropout']
        )
    
    model.to(device)
    
    # 定义损失函数和优化器
    criterion = nn.CrossEntropyLoss()
    optimizer = optim.Adam(model.parameters(), lr=config['learning_rate'])
    
    # 训练模型
    print("开始训练...")
    train_losses, val_losses, train_accuracies, val_accuracies, best_model_path = train_model(
        model=model,
        train_loader=train_loader,
        val_loader=val_loader,
        criterion=criterion,
        optimizer=optimizer,
        num_epochs=config['num_epochs'],
        device=device,
        save_dir=output_dir
    )
    
    # 绘制训练曲线
    print("绘制训练曲线...")
    plot_training_curves(train_losses, val_losses, train_accuracies, val_accuracies, output_dir)
    
    # 在测试集上评估
    print("在测试集上评估...")
    model.load_state_dict(torch.load(best_model_path, map_location=device)['model_state_dict'])
    model.eval()
    
    test_correct = 0
    test_total = 0
    
    with torch.no_grad():
        for sequences, lengths, labels in test_loader:
            sequences = sequences.to(device)
            labels = labels.to(device)
            
            outputs = model(sequences, lengths)
            _, predicted = torch.max(outputs.data, 1)
            test_total += labels.size(0)
            test_correct += (predicted == labels).sum().item()
    
    test_accuracy = 100 * test_correct / test_total
    print(f"测试集准确率: {test_accuracy:.2f}%")
    
    # 保存结果
    results = {
        'best_model_path': best_model_path,
        'test_accuracy': test_accuracy,
        'final_train_accuracy': train_accuracies[-1],
        'final_val_accuracy': val_accuracies[-1],
        'best_val_accuracy': max(val_accuracies)
    }
    
    with open(os.path.join(output_dir, 'results.json'), 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"训练完成！")
    print(f"最佳模型: {best_model_path}")
    print(f"测试集准确率: {test_accuracy:.2f}%")
    print(f"最佳验证准确率: {max(val_accuracies):.2f}%")

if __name__ == "__main__":
    main()
