#!/usr/bin/env python3
"""
使用示例脚本：演示如何使用多类SVM分类器
"""

import os
import sys

def print_usage():
    """打印使用说明"""
    print("="*80)
    print("多类AI模型分类器使用示例")
    print("="*80)
    print()
    
    print("1. 为每个领域分别训练分类器（推荐）：")
    print("   python multiclass/train_svm_ovo.py --skip_pairwise")
    print()
    
    print("2. 只训练特定领域的分类器：")
    print("   python multiclass/train_svm_ovo.py --domains writing xsum --skip_pairwise")
    print()
    
    print("3. 训练使用所有领域数据的单个分类器：")
    print("   python multiclass/train_svm_ovo.py --train_all_domains --skip_pairwise")
    print()
    
    print("4. 训练包含成对分类器的完整分析（较慢）：")
    print("   python multiclass/train_svm_ovo.py --domains harmful")
    print()
    
    print("参数说明：")
    print("  --domains: 指定要训练的领域 [harmful, peerread, pubmed, writing, xsum]")
    print("  --skip_pairwise: 跳过成对分类器训练以加快速度")
    print("  --train_all_domains: 训练使用所有领域数据的单个分类器")
    print("  --output_dir: 指定输出目录（默认: multiclass）")
    print("  --test_size: 测试集比例（默认: 0.2）")
    print()
    
    print("输出文件：")
    print("  multiclass_ovo_classifier_<domain>.pkl: 主分类器")
    print("  pairwise_classifiers_<domain>.pkl: 成对分类器")
    print("  training_results_<domain>.pkl: 详细训练结果")
    print()

def check_data_availability():
    """检查数据文件是否存在"""
    print("检查数据文件可用性...")
    print("-" * 40)
    
    data_root = "data"
    models = {
        'Claude-Haiku': 'Claude/Claude-Haiku/harmful_claude-3-5-haiku-20241022_model.txt',
        'Claude-Sonnet': 'Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_model.txt',
        'GPT4': 'GPT4/harmful_gpt-4-turbo-2024-04-09_model.txt',
        'GPT4o': 'GPT4o/harmful_chatgpt-4o-latest_model.txt'
    }
    
    all_available = True
    for model_name, file_path in models.items():
        full_path = os.path.join(data_root, file_path)
        if os.path.exists(full_path):
            print(f"✓ {model_name}: {file_path}")
        else:
            print(f"✗ {model_name}: {file_path} (不存在)")
            all_available = False
    
    print()
    if all_available:
        print("✓ 所有模型的数据文件都可用")
    else:
        print("✗ 部分数据文件缺失，可能影响训练")
    
    print()
    
    # 检查不同领域的数据
    domains = ['harmful', 'peerread', 'pubmed', 'writing', 'xsum']
    print("各领域数据可用性：")
    for domain in domains:
        available_models = []
        for model_name, _ in models.items():
            if model_name == 'Claude-Haiku':
                file_pattern = f"Claude/Claude-Haiku/{domain}_claude-3-5-haiku-20241022_model.txt"
            elif model_name == 'Claude-Sonnet':
                file_pattern = f"Claude/Claude-Sonnet/{domain}_claude-3-5-sonnet-20241022_model.txt"
            elif model_name == 'GPT4':
                file_pattern = f"GPT4/{domain}_gpt-4-turbo-2024-04-09_model.txt"
            elif model_name == 'GPT4o':
                file_pattern = f"GPT4o/{domain}_chatgpt-4o-latest_model.txt"
            
            if os.path.exists(os.path.join(data_root, file_pattern)):
                available_models.append(model_name)
        
        print(f"  {domain}: {len(available_models)}/4 个模型 ({', '.join(available_models)})")
    
    return all_available

def run_example():
    """运行示例训练"""
    print("运行示例训练...")
    print("-" * 40)
    
    # 运行一个快速示例（只使用harmful数据，跳过成对分类器）
    cmd = "python multiclass/train_svm_ovo.py --domains harmful --skip_pairwise --output_dir multiclass/example_output"
    
    print(f"执行命令: {cmd}")
    print("注意：这可能需要几分钟时间...")
    print()
    
    # 这里不实际执行，只是展示
    print("如要运行示例，请复制上述命令到终端执行")

def main():
    print_usage()
    
    # 检查数据可用性
    data_available = check_data_availability()
    
    if data_available:
        print("推荐的快速开始命令：")
        print("python multiclass/train_svm_ovo.py --domains harmful --skip_pairwise")
        print()
        print("这将训练一个harmful领域的分类器，用于区分四个AI模型")
    else:
        print("请确保数据文件存在后再运行训练脚本")

if __name__ == '__main__':
    main()
