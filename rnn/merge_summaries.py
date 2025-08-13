#!/usr/bin/env python3
"""
合并harmful domain结果到all_domains_training_summary.json
"""

import json
import os

def merge_summaries():
    # 读取harmful summary
    harmful_file = "outputs/domain_experiments/harmful_summary.json"
    with open(harmful_file, 'r') as f:
        harmful_results = json.load(f)
    
    # 读取all domains summary
    all_domains_file = "all_domains_training_summary.json"
    with open(all_domains_file, 'r') as f:
        all_domains_summary = json.load(f)
    
    # 将harmful结果转换为与all_domains格式一致的配置
    harmful_configs = []
    for result in harmful_results:
        model_version = result['model']
        # 标准化模型版本名称
        if model_version == "opus_20240229":
            model_version = "3-opus-20240229"
        elif model_version == "haiku_20240307":
            model_version = "3-haiku-20240307"
        elif model_version == "haiku35_20241022":
            model_version = "3-5-haiku-20241022"
        
        config = {
            "domain": "harmful",
            "model_version": model_version,
            "output_dir": f"outputs/domain_experiments/harmful_{model_version.replace('-', '_').replace('.', '')}",
            "hidden_size": 64,
            "num_layers": 1,
            "dropout": 0.3,
            "batch_size": 16,
            "learning_rate": 0.001,
            "num_epochs": 50,
            "cv_folds": 5,
            "device": "cpu",
            "results": {
                "accuracy": result['accuracy'],
                "std_accuracy": result['std_accuracy'],
                "f1": result['f1'],
                "std_f1": result['std_f1'],
                "auc": result['auc'],
                "std_auc": result['std_auc']
            }
        }
        harmful_configs.append(config)
    
    # 将harmful配置添加到all_domains_summary
    all_domains_summary['successful_configs'].extend(harmful_configs)
    all_domains_summary['total_configs'] = len(all_domains_summary['successful_configs'])
    all_domains_summary['successful'] = all_domains_summary['total_configs']
    
    # 保存合并后的结果
    output_file = "all_domains_training_summary_merged.json"
    with open(output_file, 'w') as f:
        json.dump(all_domains_summary, f, indent=2)
    
    print(f"Successfully merged harmful results into {output_file}")
    print(f"Total configurations: {all_domains_summary['total_configs']}")
    print(f"Successful: {all_domains_summary['successful']}")
    print(f"Failed: {all_domains_summary['failed']}")

if __name__ == "__main__":
    merge_summaries()
