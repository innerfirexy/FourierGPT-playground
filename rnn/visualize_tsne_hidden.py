#!/usr/bin/env python3
"""
从已训练的 BiLSTMClassifier 提取表示，并用 t-SNE 在二维空间可视化。

本脚本提供 exp_gpt4o() 实验函数，内部硬编码模型与数据路径。
默认将图像保存到当前目录（tsne_hidden.png）。
"""

import os
import sys
import json
import argparse
import numpy as np
import torch
import matplotlib.pyplot as plt
from sklearn.manifold import TSNE

# 确保可从当前目录导入本包模块
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from model import BiLSTMClassifier


def load_config(model_dir: str):
    config_path = os.path.join(model_dir, 'config.json')
    with open(config_path, 'r') as f:
        return json.load(f)


def build_model_from_config(cfg: dict, device: str):
    mcfg = cfg.get('model', {})
    model = BiLSTMClassifier(
        input_size=mcfg.get('input_size', 1),
        hidden_size=mcfg.get('hidden_size', 128),
        num_layers=mcfg.get('num_layers', 2),
        num_classes=mcfg.get('num_classes', 2),
        dropout=mcfg.get('dropout', 0.5),
        bidirectional=mcfg.get('bidirectional', True),
    )
    return model.to(device)


def load_best_checkpoint(model_dir: str, device: str):
    # 该目录中是 K 折的 fold_i_model.pth；选 fold_1_model.pth 或者任一存在的
    candidates = [
        'fold_1_model.pth', 'fold_2_model.pth', 'fold_3_model.pth',
        'fold_4_model.pth', 'fold_5_model.pth'
    ]
    for name in candidates:
        path = os.path.join(model_dir, name)
        if os.path.exists(path):
            return path
    raise FileNotFoundError(f"未找到 fold_i_model.pth 于: {model_dir}")


def load_text_scores(path: str, max_samples: int | None = None):
    """每行空格分隔的浮点序列。返回 list[np.ndarray]。"""
    sequences = []
    with open(path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            arr = np.array([float(x) for x in line.split()], dtype=np.float32)
            sequences.append(arr)
            if max_samples is not None and len(sequences) >= max_samples:
                break
    return sequences


def pad_and_to_tensor(sequences: list[np.ndarray]):
    lengths = [len(seq) for seq in sequences]
    max_len = max(lengths)
    padded = np.zeros((len(sequences), max_len), dtype=np.float32)
    for i, seq in enumerate(sequences):
        padded[i, : len(seq)] = seq
    x = torch.tensor(padded, dtype=torch.float32)
    lengths_t = torch.tensor(lengths, dtype=torch.long)
    return x, lengths_t


def parse_args():
    parser = argparse.ArgumentParser(description='t-SNE 可视化 BiLSTM 表示 (GPT-4o 写作域)')
    parser.add_argument('--max_samples', type=int, default=None,
                        help='每类最多采样条数（None 表示全量）')
    parser.add_argument('--perplexity', type=float, default=30.0)
    parser.add_argument('--seed', type=int, default=42)
    parser.add_argument('--where', type=str, default='second_linear', choices=['last_hidden', 'first_linear', 'second_linear'],
                        help='选择可视化的表示层：最后隐藏态/第一线性层/第二线性层（最后线性层的输入）')
    parser.add_argument('--save_path', type=str, default='tsne_hidden.png',
                        help='输出路径，默认当前目录 tsne_hidden.png；若为目录则自动追加文件名')
    return parser.parse_args()


def exp_gpt4o(max_samples=None, perplexity=30.0, seed=42, where='second_linear', save_path='tsne_hidden.png'):
    # 硬编码路径（绝对路径）
    domain = 'harmful'
    model_dir = f'/Users/xy/projects/FourierGPT-playground/rnn/outputs/domain_experiments/{domain}_chatgpt-4o-latest'
    human_file = f'/Users/xy/projects/FourierGPT-playground/data/GPT4o/{domain}_chatgpt-4o-latest_human.txt'
    model_file = f'/Users/xy/projects/FourierGPT-playground/data/GPT4o/{domain}_chatgpt-4o-latest_model.txt'

    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    np.random.seed(seed)
    torch.manual_seed(seed)

    cfg = load_config(model_dir)
    model = build_model_from_config(cfg, device)
    ckpt_path = load_best_checkpoint(model_dir, device)

    # 兼容两种保存格式：state_dict 或包含键的字典
    try:
        state = torch.load(ckpt_path, map_location=device, weights_only=False)
    except Exception:
        try:
            import numpy as _np
            from torch.serialization import add_safe_globals as _add_safe_globals
            _add_safe_globals([_np.core.multiarray.scalar])
            state = torch.load(ckpt_path, map_location=device, weights_only=True)
        except Exception as e:
            raise RuntimeError(f"无法加载模型权重: {ckpt_path}: {e}")

    if isinstance(state, dict) and 'model_state_dict' in state:
        model.load_state_dict(state['model_state_dict'])
    else:
        model.load_state_dict(state)
    model.eval()

    # 载入数据（human 为 0，model 为 1）
    human_seqs = load_text_scores(human_file, max_samples=max_samples)
    model_seqs = load_text_scores(model_file, max_samples=max_samples)

    all_seqs = human_seqs + model_seqs
    labels = np.array([0] * len(human_seqs) + [1] * len(model_seqs), dtype=np.int64)

    x, lengths = pad_and_to_tensor(all_seqs)
    x = x.to(device)
    lengths = lengths.to(device)

    with torch.no_grad():
        if where == 'last_hidden':
            feats_t = model.extract_last_hidden(x, lengths)
        elif where == 'first_linear':
            feats_t = model.extract_first_linear(x, lengths)
        elif where == 'second_linear':
            feats_t = model.extract_second_linear(x, lengths)
        else:
            raise ValueError(f"未知的 where: {where}")
        feats = feats_t.detach().cpu().numpy()

    tsne = TSNE(n_components=2, perplexity=perplexity, random_state=seed, init='pca')
    emb2d = tsne.fit_transform(feats)

    # 可视化（不同颜色与形状）
    plt.figure(figsize=(8, 7))
    colors = np.array(['#1f77b4', '#d62728'])  # human: 蓝, model: 红
    markers = np.array(['o', '^'])
    for cls in [0, 1]:
        idx = (labels == cls)
        plt.scatter(
            emb2d[idx, 0], emb2d[idx, 1],
            c=colors[cls], marker=markers[cls], s=26, alpha=0.85,
            label='Human' if cls == 0 else 'Model'
        )
    plt.legend()
    plt.title(f't-SNE of BiLSTM representations ({where}) - {domain}, chatgpt-4o-latest')
    plt.xlabel('t-SNE 1')
    plt.ylabel('t-SNE 2')
    plt.tight_layout()

    # 若 save_path 是目录则追加默认文件名
    if os.path.isdir(save_path):
        save_path = os.path.join(save_path, 'tsne_hidden.png')
    out_dir = os.path.dirname(save_path)
    if out_dir:
        os.makedirs(out_dir, exist_ok=True)
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    print(f"已保存到: {save_path}")


def main():
    args = parse_args()
    exp_gpt4o(max_samples=args.max_samples, perplexity=args.perplexity, seed=args.seed, where=args.where, save_path=args.save_path)


if __name__ == '__main__':
    main()


