#!/usr/bin/env python3
"""
测试结果汇总报告

汇总所有测试数据集上的模型性能结果
"""

import json
import os
from datetime import datetime

def load_results(file_path):
    """加载测试结果"""
    if os.path.exists(file_path):
        with open(file_path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return None

def print_detailed_report():
    """打印详细的测试报告"""
    
    # 测试结果文件
    result_files = [
        {
            'name': 'Claude-3.5-Sonnet (2024-06-20)',
            'file': 'test_results_20240620.json'
        },
        {
            'name': 'Claude-3.5-Sonnet (2024-10-22)', 
            'file': 'test_results_20241022.json'
        }
    ]
    
    print("=" * 100)
    print("RNN模型性能评估综合报告")
    print("=" * 100)
    print(f"生成时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"模型: outputs/claude_sonnet_run_20250811_025519/best_model_epoch_13.pth")
    print()
    
    all_results = []
    
    for test_info in result_files:
        print(f"测试数据集: {test_info['name']}")
        print("-" * 80)
        
        results = load_results(test_info['file'])
        if results is None:
            print(f"  错误: 无法加载结果文件 {test_info['file']}")
            print()
            continue
            
        metrics = results['metrics']
        all_results.append({
            'name': test_info['name'],
            'metrics': metrics
        })
        
        # 基本信息
        print(f"  样本总数: {metrics['sample_counts']['total']}")
        print(f"  人类样本: {metrics['sample_counts']['human']}")
        print(f"  模型样本: {metrics['sample_counts']['model']}")
        print()
        
        # 总体指标
        print("  总体性能指标:")
        print(f"    准确率 (Accuracy):  {metrics['accuracy']:.4f} ({metrics['accuracy']*100:.2f}%)")
        print(f"    精确率 (Precision): {metrics['precision_weighted']:.4f} ({metrics['precision_weighted']*100:.2f}%)")
        print(f"    召回率 (Recall):    {metrics['recall_weighted']:.4f} ({metrics['recall_weighted']*100:.2f}%)")
        print(f"    F1分数 (F1-Score):  {metrics['f1_weighted']:.4f} ({metrics['f1_weighted']*100:.2f}%)")
        print()
        
        # 各类别指标
        print("  各类别详细指标:")
        classes = ['Human', 'Model']
        for i, class_name in enumerate(classes):
            print(f"    {class_name}:")
            print(f"      精确率: {metrics['precision_per_class'][i]:.4f} ({metrics['precision_per_class'][i]*100:.2f}%)")
            print(f"      召回率: {metrics['recall_per_class'][i]:.4f} ({metrics['recall_per_class'][i]*100:.2f}%)")
            print(f"      F1分数: {metrics['f1_per_class'][i]:.4f} ({metrics['f1_per_class'][i]*100:.2f}%)")
        print()
        
        # 混淆矩阵
        cm = metrics['confusion_matrix']
        print("  混淆矩阵:")
        print("           预测")
        print("         Human  Model")
        print(f"  真实 Human  {cm[0][0]:4d}  {cm[0][1]:4d}")
        print(f"       Model  {cm[1][0]:4d}  {cm[1][1]:4d}")
        print()
        print("=" * 100)
        print()
    
    # 汇总比较
    if len(all_results) > 1:
        print("测试数据集性能比较")
        print("=" * 100)
        
        # 表头
        print(f"{'数据集':<30} {'准确率':<10} {'精确率':<10} {'召回率':<10} {'F1分数':<10}")
        print("-" * 70)
        
        # 每个数据集的结果
        for result in all_results:
            metrics = result['metrics']
            print(f"{result['name']:<30} "
                  f"{metrics['accuracy']:.4f}    "
                  f"{metrics['precision_weighted']:.4f}    "
                  f"{metrics['recall_weighted']:.4f}    "
                  f"{metrics['f1_weighted']:.4f}")
        
        print()
        
        # 平均性能
        avg_accuracy = sum(r['metrics']['accuracy'] for r in all_results) / len(all_results)
        avg_precision = sum(r['metrics']['precision_weighted'] for r in all_results) / len(all_results)
        avg_recall = sum(r['metrics']['recall_weighted'] for r in all_results) / len(all_results)
        avg_f1 = sum(r['metrics']['f1_weighted'] for r in all_results) / len(all_results)
        
        print(f"{'平均性能':<30} "
              f"{avg_accuracy:.4f}    "
              f"{avg_precision:.4f}    "
              f"{avg_recall:.4f}    "
              f"{avg_f1:.4f}")
        
        print()
        print("性能分析:")
        print("-" * 50)
        
        # 分析各类别性能
        human_precisions = [r['metrics']['precision_per_class'][0] for r in all_results]
        human_recalls = [r['metrics']['recall_per_class'][0] for r in all_results]
        human_f1s = [r['metrics']['f1_per_class'][0] for r in all_results]
        
        model_precisions = [r['metrics']['precision_per_class'][1] for r in all_results]
        model_recalls = [r['metrics']['recall_per_class'][1] for r in all_results]
        model_f1s = [r['metrics']['f1_per_class'][1] for r in all_results]
        
        print(f"人类文本检测:")
        print(f"  平均精确率: {sum(human_precisions)/len(human_precisions):.4f}")
        print(f"  平均召回率: {sum(human_recalls)/len(human_recalls):.4f}")
        print(f"  平均F1分数: {sum(human_f1s)/len(human_f1s):.4f}")
        print()
        
        print(f"模型文本检测:")
        print(f"  平均精确率: {sum(model_precisions)/len(model_precisions):.4f}")
        print(f"  平均召回率: {sum(model_recalls)/len(model_recalls):.4f}")
        print(f"  平均F1分数: {sum(model_f1s)/len(model_f1s):.4f}")
        print()
        
        # 性能观察
        print("关键观察:")
        print("-" * 30)
        
        best_accuracy_idx = max(range(len(all_results)), key=lambda i: all_results[i]['metrics']['accuracy'])
        worst_accuracy_idx = min(range(len(all_results)), key=lambda i: all_results[i]['metrics']['accuracy'])
        
        print(f"• 最佳性能数据集: {all_results[best_accuracy_idx]['name']} (准确率: {all_results[best_accuracy_idx]['metrics']['accuracy']:.4f})")
        print(f"• 最差性能数据集: {all_results[worst_accuracy_idx]['name']} (准确率: {all_results[worst_accuracy_idx]['metrics']['accuracy']:.4f})")
        
        # 检查是否有性能下降趋势
        if len(all_results) == 2:
            performance_change = all_results[1]['metrics']['accuracy'] - all_results[0]['metrics']['accuracy']
            if performance_change < -0.05:
                print(f"• 注意: 较新数据集上性能显著下降 ({performance_change:.4f})")
            elif performance_change > 0.05:
                print(f"• 较新数据集上性能有所提升 ({performance_change:.4f})")
            else:
                print(f"• 不同数据集上性能相对稳定 ({performance_change:.4f})")

def main():
    """主函数"""
    print_detailed_report()

if __name__ == "__main__":
    main()
