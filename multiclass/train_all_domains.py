#!/usr/bin/env python3
"""
训练所有领域的多类分类器
"""

import subprocess
import sys
import time
from pathlib import Path

def run_training(domain, output_dir="multiclass/domain_classifiers"):
    """训练指定领域的分类器"""
    print(f"\n{'='*80}")
    print(f"开始训练 {domain.upper()} 领域分类器")
    print(f"{'='*80}")
    
    cmd = [
        sys.executable, 
        "multiclass/train_svm_ovo.py",
        "--domains", domain,
        "--skip_pairwise",
        "--output_dir", output_dir
    ]
    
    start_time = time.time()
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        print(result.stdout)
        
        end_time = time.time()
        duration = end_time - start_time
        print(f"\n✓ {domain} 领域训练完成，耗时: {duration:.1f}秒")
        return True, result.stdout
        
    except subprocess.CalledProcessError as e:
        print(f"✗ {domain} 领域训练失败:")
        print(e.stdout)
        print(e.stderr)
        return False, e.stderr

def main():
    """训练所有领域的分类器"""
    domains = ['harmful', 'peerread', 'pubmed', 'writing', 'xsum']
    output_dir = "multiclass/domain_classifiers"
    
    # 创建输出目录
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    
    print("开始训练所有领域的多类AI模型分类器")
    print(f"输出目录: {output_dir}")
    print(f"训练领域: {', '.join(domains)}")
    
    results = {}
    successful_domains = []
    failed_domains = []
    
    total_start_time = time.time()
    
    for domain in domains:
        success, output = run_training(domain, output_dir)
        results[domain] = {'success': success, 'output': output}
        
        if success:
            successful_domains.append(domain)
        else:
            failed_domains.append(domain)
    
    total_end_time = time.time()
    total_duration = total_end_time - total_start_time
    
    # 打印总结
    print(f"\n{'='*80}")
    print("训练总结")
    print(f"{'='*80}")
    print(f"总耗时: {total_duration:.1f}秒")
    print(f"成功训练: {len(successful_domains)}/{len(domains)} 个领域")
    
    if successful_domains:
        print(f"\n✓ 成功的领域: {', '.join(successful_domains)}")
    
    if failed_domains:
        print(f"\n✗ 失败的领域: {', '.join(failed_domains)}")
    
    # 提取性能信息
    print(f"\n性能总结:")
    print("-" * 40)
    
    for domain in successful_domains:
        output = results[domain]['output']
        # 简单的性能提取（可以改进）
        lines = output.split('\n')
        for line in lines:
            if '测试准确率:' in line:
                accuracy = line.split(':')[1].strip()
                print(f"{domain.upper():<10}: {accuracy}")
                break
    
    print(f"\n所有分类器已保存到: {output_dir}")
    print("可以使用这些分类器进行AI模型检测。")

if __name__ == '__main__':
    main()
