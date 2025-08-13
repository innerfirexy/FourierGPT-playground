#!/usr/bin/env python3
"""
RNN模型推理脚本

用于加载训练好的模型并对新的负对数似然分数序列进行预测。
使用BasePredictor基类，提供灵活的推理功能。
"""

import argparse
import os
import sys

# 添加当前目录到路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from base_predictor import create_predictor

class NLLInferenceEngine:
    """推理引擎，基于BasePredictor实现推理功能"""
    
    def __init__(self, model_path, model_type='bilstm', device='cpu'):
        """
        初始化推理引擎
        
        Args:
            model_path: 模型权重文件路径
            model_type: 模型类型 ('bilstm' 或 'simple')
            device: 推理设备
        """
        self.predictor = create_predictor(model_path, model_type, device)
        self.model_type = model_type
        self.device = device
    
    def infer_from_file(self, input_file, max_length=None):
        """
        从文件推理
        
        Args:
            input_file: 输入文件路径
            max_length: 最大序列长度
            
        Returns:
            results: 推理结果字典
        """
        print(f"从文件加载数据: {input_file}")
        scores_list = self.predictor.load_scores_from_file(input_file, max_length)
        
        print(f"加载了 {len(scores_list)} 个序列")
        
        # 批量预测
        predictions, probabilities = self.predictor.predict_batch(scores_list, max_length=max_length)
        
        # 统计结果
        human_count = sum(1 for pred in predictions if pred == 0)
        model_count = len(predictions) - human_count
        
        results = {
            'predictions': predictions,
            'probabilities': probabilities,
            'human_count': human_count,
            'model_count': model_count,
            'total': len(predictions),
            'human_percent': human_count / len(predictions) * 100,
            'model_percent': model_count / len(predictions) * 100
        }
        
        return results
    
    def infer_from_scores(self, scores, max_length=None):
        """
        从分数列表推理
        
        Args:
            scores: 分数列表
            max_length: 最大序列长度
            
        Returns:
            prediction: 预测标签
            probability: 预测概率
        """
        if max_length and len(scores) > max_length:
            scores = scores[:max_length]
        
        prediction, probability = self.predictor.predict_single(scores, max_length)
        
        return prediction, probability
    
    def print_detailed_results(self, results, show_individual=False):
        """
        打印详细结果
        
        Args:
            results: 推理结果字典
            show_individual: 是否显示每个序列的结果
        """
        print("\n预测结果:")
        print("=" * 60)
        
        if show_individual and len(results['predictions']) <= 50:  # 只在序列数量较少时显示详细结果
            print("序列ID\t预测\t概率\t\t类别")
            print("-" * 40)
            
            for i, (pred, prob) in enumerate(zip(results['predictions'], results['probabilities'])):
                label = "人类" if pred == 0 else "模型"
                confidence = prob[pred]
                print(f"{i+1}\t{pred}\t{confidence:.4f}\t\t{label}")
            
            print("-" * 40)
        
        # 汇总统计
        print(f"总序列数: {results['total']}")
        print(f"人类: {results['human_count']} ({results['human_percent']:.1f}%)")
        print(f"模型: {results['model_count']} ({results['model_percent']:.1f}%)")
        
        # 置信度统计
        confidences = [prob[pred] for pred, prob in zip(results['predictions'], results['probabilities'])]
        avg_confidence = sum(confidences) / len(confidences)
        min_confidence = min(confidences)
        max_confidence = max(confidences)
        
        print(f"\n置信度统计:")
        print(f"平均置信度: {avg_confidence:.4f}")
        print(f"最低置信度: {min_confidence:.4f}")
        print(f"最高置信度: {max_confidence:.4f}")
    
    def run_demo_mode(self, max_length=None):
        """运行演示模式"""
        print("演示模式 - 使用示例数据")
        
        # 创建一些示例数据
        example_scores = [
            [11.13528, 3.72001, 10.88363, 10.93882, 0.16693, 0.18061, 5.67179, 2.59403, 0.70261, 2.07792],
            [9.04648, 5.01506, 2.77325, 0.00815, 8.66432, 9.81500, 4.27468, 1.97760, 3.49640, 5.45108],
            [10.76282, 9.75742, 6.83630, 8.46655, 5.38614, 1.13510, 0.98469, 2.61917, 0.22812, 3.53722]
        ]
        
        print(f"示例序列数量: {len(example_scores)}")
        
        # 批量预测
        predictions, probabilities = self.predictor.predict_batch(example_scores, max_length=max_length)
        
        # 构造结果字典
        human_count = sum(1 for pred in predictions if pred == 0)
        model_count = len(predictions) - human_count
        
        results = {
            'predictions': predictions,
            'probabilities': probabilities,
            'human_count': human_count,
            'model_count': model_count,
            'total': len(predictions),
            'human_percent': human_count / len(predictions) * 100,
            'model_percent': model_count / len(predictions) * 100
        }
        
        self.print_detailed_results(results, show_individual=True)

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='RNN模型推理')
    parser.add_argument('--model_path', type=str, required=True,
                       help='模型权重文件路径')
    parser.add_argument('--model_type', type=str, default='bilstm',
                       choices=['bilstm', 'simple'],
                       help='模型类型')
    parser.add_argument('--input_file', type=str,
                       help='输入文件路径（可选）')
    parser.add_argument('--scores', type=str,
                       help='负对数似然分数，用空格分隔（可选）')
    parser.add_argument('--max_length', type=int, default=None,
                       help='最大序列长度')
    parser.add_argument('--device', type=str, default='cpu',
                       help='推理设备')
    parser.add_argument('--show_individual', action='store_true',
                       help='显示每个序列的详细结果')
    
    args = parser.parse_args()
    
    # 检查模型文件是否存在
    if not os.path.exists(args.model_path):
        print(f"错误: 模型文件不存在 - {args.model_path}")
        return
    
    print("RNN模型推理")
    print("=" * 50)
    print(f"模型路径: {args.model_path}")
    print(f"模型类型: {args.model_type}")
    print(f"推理设备: {args.device}")
    if args.max_length:
        print(f"最大序列长度: {args.max_length}")
    print()
    
    # 创建推理引擎
    try:
        engine = NLLInferenceEngine(
            model_path=args.model_path,
            model_type=args.model_type,
            device=args.device
        )
        
        print(f"模型加载成功!")
        print("-" * 50)
        
        if args.input_file:
            # 从文件推理
            if not os.path.exists(args.input_file):
                print(f"错误: 输入文件不存在 - {args.input_file}")
                return
            
            results = engine.infer_from_file(args.input_file, args.max_length)
            engine.print_detailed_results(results, args.show_individual)
            
        elif args.scores:
            # 从命令行参数推理
            scores = [float(x) for x in args.scores.split()]
            print(f"输入序列长度: {len(scores)}")
            
            prediction, probability = engine.infer_from_scores(scores, args.max_length)
            
            label = "人类" if prediction == 0 else "模型"
            print(f"\n预测结果:")
            print(f"预测标签: {prediction} ({label})")
            print(f"预测概率: {probability:.4f}")
            
        else:
            # 演示模式
            engine.run_demo_mode(args.max_length)
            
    except Exception as e:
        print(f"推理过程中出现错误: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
