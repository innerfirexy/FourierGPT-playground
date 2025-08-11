#!/usr/bin/env python3
"""
将 Claude-Haiku 目录中的不同 domain 的 claude-3-opus-20240229 数据分别拷贝到 train_data 目录
"""

import os
import shutil
import glob
import argparse

def copy_domain_data(data_dir, output_dir):
    """
    将不同 domain 的数据分别拷贝到输出目录
    
    Args:
        data_dir: Claude-Haiku 数据目录路径
        output_dir: 输出目录路径
    """
    # 确保输出目录存在
    os.makedirs(output_dir, exist_ok=True)
    
    # 查找所有包含 claude-3-opus-20240229 的文件
    pattern = os.path.join(data_dir, "*claude-3-opus-20240229*.txt")
    files = glob.glob(pattern)
    
    print(f"找到 {len(files)} 个文件:")
    for f in files:
        print(f"  {os.path.basename(f)}")
    
    # 按 domain 分组文件
    domain_files = {}
    for file_path in files:
        filename = os.path.basename(file_path)
        # 提取 domain 名称（文件名中第一个下划线前的部分）
        domain = filename.split('_')[0]
        
        if domain not in domain_files:
            domain_files[domain] = {'human': [], 'model': []}
        
        if '_human.txt' in filename:
            domain_files[domain]['human'].append(file_path)
        elif '_model.txt' in filename:
            domain_files[domain]['model'].append(file_path)
    
    print(f"\n按 domain 分组:")
    for domain, files_dict in domain_files.items():
        print(f"  {domain}: {len(files_dict['human'])} human, {len(files_dict['model'])} model")
    
    # 拷贝每个 domain 的文件
    copied_files = []
    for domain, files_dict in domain_files.items():
        print(f"\n处理 domain: {domain}")
        
        # 拷贝 human 文件
        for human_file in files_dict['human']:
            dest_file = os.path.join(output_dir, os.path.basename(human_file))
            shutil.copy2(human_file, dest_file)
            copied_files.append(dest_file)
            print(f"  拷贝: {os.path.basename(human_file)}")
        
        # 拷贝 model 文件
        for model_file in files_dict['model']:
            dest_file = os.path.join(output_dir, os.path.basename(model_file))
            shutil.copy2(model_file, dest_file)
            copied_files.append(dest_file)
            print(f"  拷贝: {os.path.basename(model_file)}")
    
    print(f"\n拷贝完成! 共拷贝 {len(copied_files)} 个文件到 {output_dir}")
    
    # 列出所有拷贝的文件
    print(f"\n拷贝的文件列表:")
    for file_path in sorted(copied_files):
        file_size = os.path.getsize(file_path) / 1024  # KB
        print(f"  {os.path.basename(file_path)} ({file_size:.1f} KB)")
    
    return copied_files

def main():
    parser = argparse.ArgumentParser(description='拷贝不同 domain 的 Claude-Haiku 数据文件')
    parser.add_argument('--data_dir', type=str, 
                       default='../data/Claude/Claude-Haiku',
                       help='Claude-Haiku 数据目录路径')
    parser.add_argument('--output_dir', type=str, 
                       default='./train_data',
                       help='输出目录路径')
    
    args = parser.parse_args()
    
    # 检查数据目录是否存在
    if not os.path.exists(args.data_dir):
        print(f"错误: 数据目录 {args.data_dir} 不存在")
        return
    
    # 拷贝数据
    copied_files = copy_domain_data(args.data_dir, args.output_dir)
    
    print(f"\n现在可以分别对每个 domain 进行训练:")
    domains = ['pubmed', 'writing', 'xsum', 'peerread', 'harmful']
    for domain in domains:
        human_file = f"./train_data/{domain}_claude-3-opus-20240229_human.txt"
        model_file = f"./train_data/{domain}_claude-3-opus-20240229_model.txt"
        print(f"python train_sup_cls.py --human {human_file} --model {model_file} --save_classifier best_claude_{domain}_classifier.pkl --verbose")

if __name__ == '__main__':
    main()
