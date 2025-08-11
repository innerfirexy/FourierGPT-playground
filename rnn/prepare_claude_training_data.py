#!/usr/bin/env python3
"""
准备Claude-Sonnet 2024-02-29的训练数据
合并所有域的数据用于训练LSTM分类器
"""

import os
import glob

def prepare_training_data():
    """准备训练数据"""
    
    # 创建训练数据目录
    train_dir = "claude_sonnet_training"
    os.makedirs(train_dir, exist_ok=True)
    
    # 2024-02-29的数据文件（训练集）
    train_files_20240229 = [
        "../data/Claude/Claude-Sonnet/xsum_claude-3-sonnet-20240229_human.txt",
        "../data/Claude/Claude-Sonnet/xsum_claude-3-sonnet-20240229_model.txt",
        "../data/Claude/Claude-Sonnet/writing_claude-3-sonnet-20240229_human.txt",
        "../data/Claude/Claude-Sonnet/writing_claude-3-sonnet-20240229_model.txt",
        "../data/Claude/Claude-Sonnet/pubmed_claude-3-sonnet-20240229_human.txt",
        "../data/Claude/Claude-Sonnet/pubmed_claude-3-sonnet-20240229_model.txt",
        "../data/Claude/Claude-Sonnet/peerread_claude-3-sonnet-20240229_human.txt",
        "../data/Claude/Claude-Sonnet/peerread_claude-3-sonnet-20240229_model.txt",
        "../data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_human.txt",
        "../data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_model.txt"
    ]
    
    # 合并训练数据
    print("准备训练数据 (2024-02-29)...")
    
    # 合并人类数据
    human_train_file = os.path.join(train_dir, "train_human.txt")
    with open(human_train_file, 'w') as outfile:
        for file_path in train_files_20240229:
            if 'human' in file_path:
                print(f"处理: {file_path}")
                with open(file_path, 'r') as infile:
                    outfile.write(infile.read())
    
    # 合并模型数据
    model_train_file = os.path.join(train_dir, "train_model.txt")
    with open(model_train_file, 'w') as outfile:
        for file_path in train_files_20240229:
            if 'model' in file_path:
                print(f"处理: {file_path}")
                with open(file_path, 'r') as infile:
                    outfile.write(infile.read())
    
    # 统计训练数据
    with open(human_train_file, 'r') as f:
        human_lines = len(f.readlines())
    
    with open(model_train_file, 'r') as f:
        model_lines = len(f.readlines())
    
    print(f"训练数据统计:")
    print(f"  人类数据: {human_lines} 行")
    print(f"  模型数据: {model_lines} 行")
    print(f"  总计: {human_lines + model_lines} 行")
    
    # 准备测试数据
    print("\n准备测试数据...")
    
    # 2024-06-20的测试数据
    test_files_20240620 = [
        "../data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20240620_human.txt",
        "../data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20240620_model.txt",
        "../data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20240620_human.txt",
        "../data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20240620_model.txt",
        "../data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20240620_human.txt",
        "../data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20240620_model.txt",
        "../data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20240620_human.txt",
        "../data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20240620_model.txt",
        "../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_human.txt",
        "../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_model.txt"
    ]
    
    # 2024-10-22的测试数据
    test_files_20241022 = [
        "../data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20241022_human.txt",
        "../data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20241022_model.txt",
        "../data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20241022_human.txt",
        "../data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20241022_model.txt",
        "../data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20241022_human.txt",
        "../data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20241022_model.txt",
        "../data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20241022_human.txt",
        "../data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20241022_model.txt",
        "../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_human.txt",
        "../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_model.txt"
    ]
    
    # 创建测试数据目录
    test_dir = os.path.join(train_dir, "test")
    os.makedirs(test_dir, exist_ok=True)
    
    # 处理2024-06-20测试数据
    print("处理2024-06-20测试数据...")
    test_20240620_human = os.path.join(test_dir, "test_20240620_human.txt")
    test_20240620_model = os.path.join(test_dir, "test_20240620_model.txt")
    
    with open(test_20240620_human, 'w') as outfile:
        for file_path in test_files_20240620:
            if 'human' in file_path:
                print(f"处理: {file_path}")
                with open(file_path, 'r') as infile:
                    outfile.write(infile.read())
    
    with open(test_20240620_model, 'w') as outfile:
        for file_path in test_files_20240620:
            if 'model' in file_path:
                print(f"处理: {file_path}")
                with open(file_path, 'r') as infile:
                    outfile.write(infile.read())
    
    # 处理2024-10-22测试数据
    print("处理2024-10-22测试数据...")
    test_20241022_human = os.path.join(test_dir, "test_20241022_human.txt")
    test_20241022_model = os.path.join(test_dir, "test_20241022_model.txt")
    
    with open(test_20241022_human, 'w') as outfile:
        for file_path in test_files_20241022:
            if 'human' in file_path:
                print(f"处理: {file_path}")
                with open(file_path, 'r') as infile:
                    outfile.write(infile.read())
    
    with open(test_20241022_model, 'w') as outfile:
        for file_path in test_files_20241022:
            if 'model' in file_path:
                print(f"处理: {file_path}")
                with open(file_path, 'r') as infile:
                    outfile.write(infile.read())
    
    # 统计测试数据
    print("\n测试数据统计:")
    
    with open(test_20240620_human, 'r') as f:
        test_20240620_human_lines = len(f.readlines())
    with open(test_20240620_model, 'r') as f:
        test_20240620_model_lines = len(f.readlines())
    
    with open(test_20241022_human, 'r') as f:
        test_20241022_human_lines = len(f.readlines())
    with open(test_20241022_model, 'r') as f:
        test_20241022_model_lines = len(f.readlines())
    
    print(f"  2024-06-20: 人类 {test_20240620_human_lines} 行, 模型 {test_20240620_model_lines} 行")
    print(f"  2024-10-22: 人类 {test_20241022_human_lines} 行, 模型 {test_20241022_model_lines} 行")
    
    print(f"\n数据准备完成！")
    print(f"训练数据目录: {train_dir}")
    print(f"训练文件: {human_train_file}, {model_train_file}")
    print(f"测试文件: {test_dir}/")

if __name__ == "__main__":
    prepare_training_data()
