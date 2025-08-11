#!/usr/bin/env python3
"""
合并 Claude-Haiku 目录中的 claude-3-opus-20240229 数据文件
"""

import os
import glob
import argparse

def merge_claude_data(data_dir, output_dir):
    """
    合并所有 claude-3-opus-20240229 的数据文件
    
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
    
    # 分离 human 和 model 文件
    human_files = [f for f in files if "_human.txt" in f]
    model_files = [f for f in files if "_model.txt" in f]
    
    print(f"\nHuman 文件: {len(human_files)} 个")
    print(f"Model 文件: {len(model_files)} 个")
    
    # 合并 human 文件
    human_output = os.path.join(output_dir, "claude-3-opus-20240229_human_merged.txt")
    print(f"\n合并 human 文件到: {human_output}")
    
    with open(human_output, 'w') as outfile:
        for i, file_path in enumerate(human_files):
            print(f"  处理 {os.path.basename(file_path)}...")
            with open(file_path, 'r') as infile:
                content = infile.read()
                if i > 0:  # 除了第一个文件，其他文件前加换行符
                    outfile.write('\n')
                outfile.write(content)
    
    # 合并 model 文件
    model_output = os.path.join(output_dir, "claude-3-opus-20240229_model_merged.txt")
    print(f"\n合并 model 文件到: {model_output}")
    
    with open(model_output, 'w') as outfile:
        for i, file_path in enumerate(model_files):
            print(f"  处理 {os.path.basename(file_path)}...")
            with open(file_path, 'r') as infile:
                content = infile.read()
                if i > 0:  # 除了第一个文件，其他文件前加换行符
                    outfile.write('\n')
                outfile.write(content)
    
    print(f"\n合并完成!")
    print(f"Human 数据: {human_output}")
    print(f"Model 数据: {model_output}")
    
    # 检查文件大小
    human_size = os.path.getsize(human_output) / 1024  # KB
    model_size = os.path.getsize(model_output) / 1024  # KB
    print(f"Human 文件大小: {human_size:.1f} KB")
    print(f"Model 文件大小: {model_size:.1f} KB")
    
    return human_output, model_output

def main():
    parser = argparse.ArgumentParser(description='合并 Claude-Haiku 数据文件')
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
    
    # 合并数据
    human_file, model_file = merge_claude_data(args.data_dir, args.output_dir)
    
    print(f"\n现在可以使用以下命令运行训练:")
    print(f"python train_sup_cls.py --human {human_file} --model {model_file} --save_classifier best_claude_classifier.pkl --verbose")

if __name__ == '__main__':
    main()
