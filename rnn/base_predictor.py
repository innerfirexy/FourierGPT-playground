#!/usr/bin/env python3
"""
基础预测器类

包含模型加载和预测的公共功能，被inference.py和evaluate_model.py继承使用。
"""

import torch
import numpy as np
import os
import sys

# 添加当前目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from model import BiLSTMClassifier, SimpleLSTMClassifier

class BasePredictor:
    """基础预测器类，包含公共功能"""
    
    def __init__(self, model_path, device='cpu'):
        """
        初始化预测器
        
        Args:
            model_path: 模型权重文件路径
            device: 推理设备
        """
        self.device = device
        self.model_path = model_path
        
        # 加载模型
        self.model = self._load_model()
        self.model.eval()
    
    def _load_model(self):
        """
        加载模型
        
        Returns:
            model: 加载的模型
        """
        # 加载checkpoint
        checkpoint = torch.load(self.model_path, map_location=self.device)
        
        # 从配置文件或checkpoint中获取模型参数
        if 'config' in checkpoint:
            config = checkpoint['config']
        else:
            # 使用默认配置（与训练时一致）
            config = {
                'input_size': 1,
                'hidden_size': 128,
                'num_layers': 2,
                'num_classes': 2,
                'dropout': 0.5,
                'bidirectional': True
            }
        
        # 创建模型
        model = BiLSTMClassifier(
            input_size=config.get('input_size', 1),
            hidden_size=config.get('hidden_size', 128),
            num_layers=config.get('num_layers', 2),
            num_classes=config.get('num_classes', 2),
            dropout=config.get('dropout', 0.5),
            bidirectional=config.get('bidirectional', True)
        )
        
        # 加载权重
        model.load_state_dict(checkpoint['model_state_dict'])
        model.to(self.device)
        
        return model
    
    def _preprocess_sequences(self, sequences, max_length=None):
        """
        预处理序列数据
        
        Args:
            sequences: 输入序列列表
            max_length: 最大序列长度
            
        Returns:
            processed_sequences: 预处理后的序列列表
        """
        processed_sequences = []
        
        for seq in sequences:
            # 转换为numpy数组（如果还不是）
            if isinstance(seq, list):
                seq = np.array(seq, dtype=np.float32)
            elif isinstance(seq, torch.Tensor):
                seq = seq.numpy().astype(np.float32)
            
            # 限制长度
            if max_length and len(seq) > max_length:
                seq = seq[:max_length]
            
            processed_sequences.append(seq.tolist())
        
        return processed_sequences
    
    def _prepare_batch_tensors(self, sequences, lengths=None):
        """
        准备批量tensor数据
        
        Args:
            sequences: 序列列表
            lengths: 序列长度列表
            
        Returns:
            batch_tensor: 批量tensor
            batch_lengths_tensor: 批量长度tensor
        """
        batch_tensors = []
        batch_lens = []
        
        for seq in sequences:
            seq_tensor = torch.tensor(seq, dtype=torch.float32)
            batch_tensors.append(seq_tensor)
            batch_lens.append(len(seq))
        
        # 填充序列到相同长度
        max_len = max(batch_lens)
        padded_sequences = []
        
        for seq_tensor in batch_tensors:
            if len(seq_tensor) < max_len:
                # 填充零
                padded = torch.cat([seq_tensor, torch.zeros(max_len - len(seq_tensor))])
                padded_sequences.append(padded)
            else:
                padded_sequences.append(seq_tensor)
        
        # 转换为批量tensor
        batch_tensor = torch.stack(padded_sequences).to(self.device)
        batch_lengths_tensor = torch.tensor(batch_lens, dtype=torch.long).to(self.device)
        
        return batch_tensor, batch_lengths_tensor
    
    def predict_batch(self, sequences, lengths=None, batch_size=32, max_length=None):
        """
        批量预测
        
        Args:
            sequences: 输入序列列表
            lengths: 序列长度列表
            batch_size: 批处理大小
            max_length: 最大序列长度
            
        Returns:
            predictions: 预测标签列表
            probabilities: 预测概率列表
        """
        # 预处理序列
        sequences = self._preprocess_sequences(sequences, max_length)
        
        predictions = []
        probabilities = []
        
        # 分批处理
        for i in range(0, len(sequences), batch_size):
            batch_sequences = sequences[i:i + batch_size]
            
            # 准备批量数据
            batch_tensor, batch_lengths_tensor = self._prepare_batch_tensors(batch_sequences)
            
            # 推理
            with torch.no_grad():
                outputs = self.model(batch_tensor, batch_lengths_tensor)
                probs = torch.softmax(outputs, dim=1)
                preds = torch.argmax(outputs, dim=1)
                
                predictions.extend(preds.cpu().numpy())
                probabilities.extend(probs.cpu().numpy())
        
        return predictions, probabilities
    
    def predict_single(self, scores, max_length=None):
        """
        预测单个序列
        
        Args:
            scores: 负对数似然分数列表或numpy数组
            max_length: 最大序列长度
            
        Returns:
            prediction: 预测标签 (0: 人类, 1: 模型)
            probability: 预测概率
        """
        predictions, probabilities = self.predict_batch([scores], max_length=max_length, batch_size=1)
        prediction = predictions[0]
        probability = probabilities[0][prediction]
        
        return prediction, probability
    
    def load_scores_from_file(self, file_path, max_length=None):
        """
        从文件加载负对数似然分数
        
        Args:
            file_path: 文件路径
            max_length: 最大序列长度
            
        Returns:
            scores_list: 分数序列列表
        """
        scores_list = []
        
        with open(file_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    scores = [float(x) for x in line.split()]
                    if max_length:
                        scores = scores[:max_length]
                    scores_list.append(scores)
        
        return scores_list

class SimpleLSTMPredictor(BasePredictor):
    """简化版LSTM预测器"""
    
    def _load_model(self):
        """加载SimpleLSTM模型"""
        checkpoint = torch.load(self.model_path, map_location=self.device)
        
        # 创建简化模型
        model = SimpleLSTMClassifier(
            input_size=1,
            hidden_size=64,
            num_classes=2,
            dropout=0.3
        )
        
        # 加载权重
        model.load_state_dict(checkpoint['model_state_dict'])
        model.to(self.device)
        
        return model

def create_predictor(model_path, model_type='bilstm', device='cpu'):
    """
    工厂函数，创建对应类型的预测器
    
    Args:
        model_path: 模型权重文件路径
        model_type: 模型类型 ('bilstm' 或 'simple')
        device: 推理设备
        
    Returns:
        predictor: 预测器实例
    """
    if model_type == 'simple':
        return SimpleLSTMPredictor(model_path, device)
    else:
        return BasePredictor(model_path, device)

if __name__ == "__main__":
    # 测试基础预测器
    import argparse
    
    parser = argparse.ArgumentParser(description='测试基础预测器')
    parser.add_argument('--model_path', type=str, required=True,
                       help='模型权重文件路径')
    parser.add_argument('--test_file', type=str,
                       help='测试文件路径')
    parser.add_argument('--device', type=str, default='cpu',
                       help='推理设备')
    
    args = parser.parse_args()
    
    # 创建预测器
    predictor = BasePredictor(args.model_path, args.device)
    
    if args.test_file:
        # 从文件加载测试数据
        scores_list = predictor.load_scores_from_file(args.test_file)
        print(f"加载了 {len(scores_list)} 个序列")
        
        # 批量预测
        predictions, probabilities = predictor.predict_batch(scores_list[:5])  # 只测试前5个
        
        for i, (pred, prob) in enumerate(zip(predictions, probabilities)):
            label = "人类" if pred == 0 else "模型"
            print(f"序列 {i+1}: {pred} ({label}) - 概率: {prob[pred]:.4f}")
    else:
        # 使用示例数据
        example_scores = [
            [11.13528, 3.72001, 10.88363, 10.93882, 0.16693, 0.18061, 5.67179, 2.59403, 0.70261, 2.07792],
            [9.04648, 5.01506, 2.77325, 0.00815, 8.66432, 9.81500, 4.27468, 1.97760, 3.49640, 5.45108]
        ]
        
        predictions, probabilities = predictor.predict_batch(example_scores)
        
        for i, (pred, prob) in enumerate(zip(predictions, probabilities)):
            label = "人类" if pred == 0 else "模型"
            print(f"示例 {i+1}: {pred} ({label}) - 概率: {prob[pred]:.4f}")
