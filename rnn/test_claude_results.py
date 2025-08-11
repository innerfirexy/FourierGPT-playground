#!/usr/bin/env python3
"""
汇总RNN模型在Claude-Sonnet harmful数据上的测试结果
"""

import subprocess
import re
import os

def run_inference(model_path, input_file):
    """运行推理并返回结果"""
    try:
        result = subprocess.run([
            'python', 'inference.py',
            '--model_path', model_path,
            '--input_file', input_file
        ], capture_output=True, text=True, cwd='.')
        
        if result.returncode == 0:
            # 解析输出找到人类和ChatGPT的百分比
            output = result.stdout
            human_match = re.search(r'人类: (\d+) \(([\d.]+)%\)', output)
            chatgpt_match = re.search(r'ChatGPT: (\d+) \(([\d.]+)%\)', output)
            
            if human_match and chatgpt_match:
                human_count = int(human_match.group(1))
                human_percent = float(human_match.group(2))
                chatgpt_count = int(chatgpt_match.group(1))
                chatgpt_percent = float(chatgpt_match.group(2))
                
                return {
                    'human_count': human_count,
                    'human_percent': human_percent,
                    'chatgpt_count': chatgpt_count,
                    'chatgpt_percent': chatgpt_percent,
                    'total': human_count + chatgpt_count
                }
        
        return None
    except Exception as e:
        print(f"运行推理时出错: {e}")
        return None

def main():
    """主函数"""
    # 模型路径
    model_path = "outputs/claude_sonnet_run_20250811_025519/best_model_epoch_13.pth"
    
    # Claude-Sonnet harmful数据文件
    test_files = [
        {
            'name': 'Claude-3-Sonnet Human (2024-02-29)',
            'path': '../data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_human.txt',
            'expected': 'human'
        },
        {
            'name': 'Claude-3-Sonnet Model (2024-02-29)',
            'path': '../data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_model.txt',
            'expected': 'model'
        },
        {
            'name': 'Claude-3.5-Sonnet Human (2024-06-20)',
            'path': '../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_human.txt',
            'expected': 'human'
        },
        {
            'name': 'Claude-3.5-Sonnet Model (2024-06-20)',
            'path': '../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_model.txt',
            'expected': 'model'
        },
        {
            'name': 'Claude-3.5-Sonnet Human (2024-10-22)',
            'path': '../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_human.txt',
            'expected': 'human'
        },
        {
            'name': 'Claude-3.5-Sonnet Model (2024-10-22)',
            'path': '../data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_model.txt',
            'expected': 'model'
        }
    ]
    
    print("=" * 80)
    print("RNN模型在Claude-Sonnet Harmful数据上的测试结果")
    print("=" * 80)
    print(f"模型: {model_path}")
    print(f"模型类型: BiLSTM")
    print("-" * 80)
    
    results = []
    
    for test_file in test_files:
        print(f"\n测试: {test_file['name']}")
        print(f"文件: {test_file['path']}")
        print(f"期望类型: {test_file['expected']}")
        
        result = run_inference(model_path, test_file['path'])
        
        if result:
            print(f"结果: 人类 {result['human_count']} ({result['human_percent']:.1f}%) | "
                  f"ChatGPT {result['chatgpt_count']} ({result['chatgpt_percent']:.1f}%)")
            
            # 计算准确率
            if test_file['expected'] == 'human':
                accuracy = result['human_percent']
                correct_predictions = result['human_count']
            else:  # model
                accuracy = result['chatgpt_percent']
                correct_predictions = result['chatgpt_count']
            
            print(f"准确率: {accuracy:.1f}% ({correct_predictions}/{result['total']})")
            
            results.append({
                'name': test_file['name'],
                'expected': test_file['expected'],
                'accuracy': accuracy,
                'human_percent': result['human_percent'],
                'chatgpt_percent': result['chatgpt_percent'],
                'total': result['total']
            })
        else:
            print("测试失败")
            results.append({
                'name': test_file['name'],
                'expected': test_file['expected'],
                'accuracy': 0,
                'human_percent': 0,
                'chatgpt_percent': 0,
                'total': 0
            })
    
    # 汇总结果
    print("\n" + "=" * 80)
    print("汇总结果")
    print("=" * 80)
    
    print(f"{'数据集':<35} {'期望':<8} {'人类%':<8} {'ChatGPT%':<10} {'准确率':<8}")
    print("-" * 80)
    
    total_accuracy = 0
    valid_tests = 0
    
    for result in results:
        print(f"{result['name']:<35} {result['expected']:<8} "
              f"{result['human_percent']:<8.1f} {result['chatgpt_percent']:<10.1f} "
              f"{result['accuracy']:<8.1f}")
        
        if result['accuracy'] > 0:
            total_accuracy += result['accuracy']
            valid_tests += 1
    
    if valid_tests > 0:
        avg_accuracy = total_accuracy / valid_tests
        print("-" * 80)
        print(f"{'平均准确率':<35} {'':<8} {'':<8} {'':<10} {avg_accuracy:<8.1f}")
    else:
        avg_accuracy = 0.0
        print("-" * 80)
        print(f"{'平均准确率':<35} {'':<8} {'':<8} {'':<10} {avg_accuracy:<8.1f}")
    
    print("\n" + "=" * 80)
    print("分析:")
    print("=" * 80)
    
    # 分析人类数据的检测准确率
    human_tests = [r for r in results if r['expected'] == 'human']
    model_tests = [r for r in results if r['expected'] == 'model']
    
    if human_tests:
        human_avg = sum(r['accuracy'] for r in human_tests) / len(human_tests)
        print(f"人类数据检测平均准确率: {human_avg:.1f}%")
    else:
        print("人类数据检测平均准确率: 0.0%")
    
    if model_tests:
        model_avg = sum(r['accuracy'] for r in model_tests) / len(model_tests)
        print(f"模型数据检测平均准确率: {model_avg:.1f}%")
    else:
        print("模型数据检测平均准确率: 0.0%")
    
    print(f"总体平均准确率: {avg_accuracy:.1f}%")

if __name__ == "__main__":
    main()
