#!/usr/bin/env python3
"""
Claude模型AUROC时间线实验
对比训练好的分类器在不同时间点Claude版本上的表现
"""

import sys
sys.path.append('..')
from run_fft import FFTProcessor
import numpy as np
import pandas as pd
import argparse
import os
import pickle
import subprocess
import matplotlib.pyplot as plt
from typing import Union

from sklearn.metrics import roc_auc_score, accuracy_score


# Preprocessing functions (same as test_sup_cls.py)
def circular(input: list, n: int = None, include_self: bool = True):
    if n is None:
        n = len(input) - 1
    output = []
    if include_self:
        output.append(input)
    for i in range(n):
        out = input[i+1:] + input[:i+1]
        output.append(out)
    return output

def get_circular_mean(input_file: str, require_sid=True):
    fft_processor = FFTProcessor(method='fft', preprocess='logzs', value='norm', require_sid=False)
    nlls = fft_processor._read_data(data_file=input_file)
    freqs, powers, sids = [], [], []
    for i, nll in enumerate(nlls):
        nll_circle = circular(nll)
        data = fft_processor._preprocess(nll_circle)
        freq, power, _ = fft_processor._fft_batch(data, verbose=False)
        power_mean = np.mean(power, axis=0)
        freqs.append(freq[0])
        powers.append(power_mean)
        sids.append(np.repeat(i, len(power_mean)))
    if require_sid:
        df = pd.DataFrame.from_dict({'freq': np.concatenate(freqs),
                                     'power': np.concatenate(powers),
                                     'sid': np.concatenate(sids)})
    else:
        df = pd.DataFrame.from_dict({'freq': np.concatenate(freqs),
                                'power': np.concatenate(powers)})
    return df

def get_features(spectrum_data: Union[str, pd.DataFrame], interp_len: int = 500):
    if isinstance(spectrum_data, str):
        df = pd.read_csv(spectrum_data)
    else:
        df = spectrum_data

    if 'sid' not in df.columns:
        df['sdiff']  = df['freq'] < df['freq'].shift(1, fill_value=0)
        df['sdiff'] = df['sdiff'].astype(int)
        df['sid'] = df['sdiff'].cumsum()

    features_interp = []
    for _, group in df.groupby('sid'):
        freqs = group['freq'].values
        features = group['power'].values
        new_freq = np.linspace(0, 0.5, interp_len)
        new_feat = np.interp(new_freq, freqs, features)
        features_interp.append(new_feat)

    return np.array(features_interp)

def test_classifier_auroc(classifier_path: str, test_human_file: str, test_model_file: str):
    """
    Test classifier and return AUROC score
    """
    # Load the trained classifier
    with open(classifier_path, 'rb') as f:
        classifier = pickle.load(f)
    
    # Process test data
    human_circlemean = get_circular_mean(test_human_file)
    model_circlemean = get_circular_mean(test_model_file)
    
    x_human = get_features(human_circlemean)
    y_human = np.zeros(x_human.shape[0])
    x_model = get_features(model_circlemean)
    y_model = np.ones(x_model.shape[0])
    
    x_test = np.concatenate([x_human, x_model], axis=0)
    y_test = np.concatenate([y_human, y_model], axis=0)
    
    # Make predictions
    y_pred_proba = classifier.predict_proba(x_test)[:, 1]
    
    # Calculate AUROC
    auroc = roc_auc_score(y_test, y_pred_proba)
    
    return auroc


def run_timeline_experiment():
    """
    运行时间线实验
    """
    # 定义域和对应的训练AUROC（从之前的训练结果）
    domains = {
        'writing': 0.9371,
        'pubmed': 0.9053,
        'xsum': 0.8918,
        'peerread': 0.8522,
        'harmful': 0.6976
    }
    
    # 定义时间点
    timepoints = ['2024-02-29\n(opus)', '2024-03-07\n(haiku)', '2024-10-22\n(haiku-3.5)']
    
    # 存储结果
    results = {}
    
    print("开始时间线实验...")
    
    for domain in domains:
        print(f"\n处理 {domain} domain...")
        
        # 初始化结果列表
        auroc_scores = []
        
        # 1. 训练数据上的交叉验证AUROC（已知结果）
        train_auroc = domains[domain]
        auroc_scores.append(train_auroc)
        print(f"  训练AUROC: {train_auroc:.4f}")
        
        # 2. test_data1 上的测试
        classifier_path = f"best_claude_{domain}_classifier.pkl"
        test1_human = f"./test_data1/{domain}_claude-3-haiku-20240307_human.txt"
        test1_model = f"./test_data1/{domain}_claude-3-haiku-20240307_model.txt"
        
        if os.path.exists(test1_human) and os.path.exists(test1_model):
            test1_auroc = test_classifier_auroc(classifier_path, test1_human, test1_model)
            auroc_scores.append(test1_auroc)
            print(f"  Test1 AUROC: {test1_auroc:.4f}")
        else:
            print(f"  Test1 数据不存在，跳过")
            auroc_scores.append(np.nan)
        
        # 3. test_data2 上的测试
        test2_human = f"./test_data2/{domain}_claude-3-5-haiku-20241022_human.txt"
        test2_model = f"./test_data2/{domain}_claude-3-5-haiku-20241022_model.txt"
        
        if os.path.exists(test2_human) and os.path.exists(test2_model):
            test2_auroc = test_classifier_auroc(classifier_path, test2_human, test2_model)
            auroc_scores.append(test2_auroc)
            print(f"  Test2 AUROC: {test2_auroc:.4f}")
        else:
            print(f"  Test2 数据不存在，跳过")
            auroc_scores.append(np.nan)
        
        results[domain] = auroc_scores
    
    return results, timepoints


def plot_timeline(results, timepoints, save_path=None):
    """
    绘制时间线图
    """
    plt.figure(figsize=(12, 8))
    
    # 定义颜色
    colors = {
        'writing': '#1f77b4',    # 蓝色
        'pubmed': '#ff7f0e',     # 橙色
        'xsum': '#2ca02c',       # 绿色
        'peerread': '#d62728',   # 红色
        'harmful': '#9467bd'     # 紫色
    }
    
    # 绘制每个domain的线
    for domain, auroc_scores in results.items():
        plt.plot(range(len(timepoints)), auroc_scores, 
                marker='o', linewidth=2, markersize=8,
                color=colors[domain], label=domain.capitalize())
    
    plt.xlabel('Time Point', fontsize=12)
    plt.ylabel('AUROC Score', fontsize=12)
    plt.title('Claude Model Classification Performance Over Time', fontsize=14, fontweight='bold')
    
    plt.xticks(range(len(timepoints)), timepoints, fontsize=10)
    plt.ylim(0.4, 1.0)
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=11)
    
    plt.tight_layout()
    
    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"图表已保存到: {save_path}")
    
    plt.show()
    
    return plt


def print_results_table(results, timepoints):
    """
    打印结果表格
    """
    print("\n" + "="*80)
    print("AUROC 结果汇总表")
    print("="*80)
    
    # 打印表头
    header = f"{'Domain':<12}"
    for tp in timepoints:
        header += f"{tp.replace(chr(10), ' '):<20}"
    print(header)
    print("-" * 80)
    
    # 打印每个domain的结果
    for domain, scores in results.items():
        row = f"{domain.capitalize():<12}"
        for score in scores:
            if np.isnan(score):
                row += f"{'N/A':<20}"
            else:
                row += f"{score:.4f}{'':15}"
        print(row)
    
    print("="*80)


def main():
    parser = argparse.ArgumentParser(description='Claude模型时间线实验')
    parser.add_argument('--save_plot', type=str, default='claude_timeline.pdf',
                       help='保存图表的路径')
    parser.add_argument('--no_plot', action='store_true', default=False,
                       help='不显示图表')
    
    args = parser.parse_args()
    
    # 运行实验
    results, timepoints = run_timeline_experiment()
    
    # 打印结果表格
    print_results_table(results, timepoints)
    
    # 绘制图表
    if not args.no_plot:
        plot_timeline(results, timepoints, args.save_plot)


if __name__ == '__main__':
    main()
