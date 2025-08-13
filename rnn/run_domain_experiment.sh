#!/bin/bash

# RNN域分类实验脚本
# 仿照 ../sup_cls/claude-all_sup_cls.sh 的实验设计

echo "=== RNN域分类实验 ==="
echo "开始时间: $(date)"

# 创建输出目录
mkdir -p outputs/domain_experiments

# 阶段1: 单域实验 (Harmful domain)
echo ""
echo "=== 阶段1: 单域实验 (Harmful) ==="

echo "训练 Claude-3-Opus (2024-02-29) - Harmful"
python train_domain_classifier.py \
    --domain harmful \
    --model_version claude-3-opus-20240229 \
    --output_dir outputs/domain_experiments/harmful_opus_20240229 \
    --hidden_size 64 \
    --num_layers 1 \
    --dropout 0.3 \
    --batch_size 16 \
    --learning_rate 0.001 \
    --num_epochs 50 \
    --cv_folds 5 \
    --device cpu

echo ""
echo "训练 Claude-3-Haiku (2024-03-07) - Harmful"
python train_domain_classifier.py \
    --domain harmful \
    --model_version claude-3-haiku-20240307 \
    --output_dir outputs/domain_experiments/harmful_haiku_20240307 \
    --hidden_size 64 \
    --num_layers 1 \
    --dropout 0.3 \
    --batch_size 16 \
    --learning_rate 0.001 \
    --num_epochs 50 \
    --cv_folds 5 \
    --device cpu

echo ""
echo "训练 Claude-3.5-Haiku (2024-10-22) - Harmful"
python train_domain_classifier.py \
    --domain harmful \
    --model_version claude-3-5-haiku-20241022 \
    --output_dir outputs/domain_experiments/harmful_haiku35_20241022 \
    --hidden_size 64 \
    --num_layers 1 \
    --dropout 0.3 \
    --batch_size 16 \
    --learning_rate 0.001 \
    --num_epochs 50 \
    --cv_folds 5 \
    --device cpu

echo ""
echo "=== 阶段1完成 ==="
echo "结束时间: $(date)"

# 生成初步报告
echo ""
echo "=== 生成阶段1报告 ==="
python -c "
import json
import os
import numpy as np

print('\\n=== Harmful Domain 实验结果 ===')
models = ['opus_20240229', 'haiku_20240307', 'haiku35_20241022']
results = []

for model in models:
    result_file = f'outputs/domain_experiments/harmful_{model}/cv_results.json'
    if os.path.exists(result_file):
        with open(result_file, 'r') as f:
            data = json.load(f)
        results.append({
            'model': model,
            'accuracy': data['mean_accuracy'],
            'std_accuracy': data['std_accuracy'],
            'f1': data['mean_f1'],
            'std_f1': data['std_f1'],
            'auc': data['mean_auc'],
            'std_auc': data['std_auc']
        })
        print(f'{model:20} | Acc: {data[\"mean_accuracy\"]:.4f}±{data[\"std_accuracy\"]:.4f} | F1: {data[\"mean_f1\"]:.4f}±{data[\"std_f1\"]:.4f} | AUC: {data[\"mean_auc\"]:.4f}±{data[\"std_auc\"]:.4f}')
    else:
        print(f'{model:20} | 结果文件不存在')

# 保存汇总结果
if results:
    with open('outputs/domain_experiments/harmful_summary.json', 'w') as f:
        json.dump(results, f, indent=2)
    print('\\n汇总结果已保存到: outputs/domain_experiments/harmful_summary.json')
"

echo ""
echo "=== 单域实验完成! ==="
echo "请检查 outputs/domain_experiments/ 目录下的结果"
echo ""
echo "如果结果良好，可以继续运行多域实验:"
echo "bash run_multi_domain_experiment.sh"
