#!/usr/bin/env python3
"""
Script to train RNN classifiers for all domains (harmful, writing, xsum, peerread, pubmed)
"""

import subprocess
import os
import json
from datetime import datetime

def get_domain_configs():
    """Get configurations for all domains"""
    domains = ['writing', 'xsum', 'peerread', 'pubmed']
    file_model_versions = ['claude-3-opus-20240229', 'claude-3-haiku-20240307', 'claude-3-5-haiku-20241022']
    
    configs = []
    for domain in domains:
        for file_model_version in file_model_versions:
            # 从完整版本生成短版本用于输出目录名
            short_version = file_model_version.replace('claude-', '').replace('-', '_').replace('.', '')
            configs.append({
                'domain': domain,
                'model_version': file_model_version,
                'file_model_version': file_model_version,
                'output_dir': f"outputs/domain_experiments/{domain}_{short_version}",
                'hidden_size': 64,
                'num_layers': 1,
                'dropout': 0.3,
                'batch_size': 16,
                'learning_rate': 0.001,
                'num_epochs': 50,
                'cv_folds': 5,
                'device': 'cpu'
            })
    
    return configs

def train_single_domain(config):
    """Train a single domain classifier"""
    print(f"\n{'='*60}")
    print(f"Training {config['domain']} domain - {config['model_version']}")
    print(f"{'='*60}")
    
    # Create output directory
    os.makedirs(config['output_dir'], exist_ok=True)
    
    # Build command
    cmd = [
        'python', 'train_domain_classifier.py',
        '--domain', config['domain'],
        '--model_version', config['model_version'],
        '--file_model_version', config['file_model_version'],
        '--output_dir', config['output_dir'],
        '--hidden_size', str(config['hidden_size']),
        '--num_layers', str(config['num_layers']),
        '--dropout', str(config['dropout']),
        '--batch_size', str(config['batch_size']),
        '--learning_rate', str(config['learning_rate']),
        '--num_epochs', str(config['num_epochs']),
        '--cv_folds', str(config['cv_folds']),
        '--device', config['device']
    ]
    
    print(f"Command: {' '.join(cmd)}")
    
    # Run training
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        print("Training completed successfully!")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Training failed with error: {e}")
        print(f"Error output: {e.stderr}")
        return False

def main():
    """Main function to train all domains"""
    print("Starting training for all domains...")
    print(f"Start time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Get all configurations
    configs = get_domain_configs()
    
    print(f"Total configurations to train: {len(configs)}")
    
    # Track results
    results = {
        'successful': [],
        'failed': []
    }
    
    # Train each domain
    for i, config in enumerate(configs, 1):
        print(f"\nProgress: {i}/{len(configs)}")
        
        success = train_single_domain(config)
        
        if success:
            results['successful'].append(config)
        else:
            results['failed'].append(config)
    
    # Print summary
    print(f"\n{'='*60}")
    print("TRAINING SUMMARY")
    print(f"{'='*60}")
    print(f"Total configurations: {len(configs)}")
    print(f"Successful: {len(results['successful'])}")
    print(f"Failed: {len(results['failed'])}")
    
    if results['failed']:
        print(f"\nFailed configurations:")
        for config in results['failed']:
            print(f"  - {config['domain']} - {config['model_version']}")
    
    print(f"\nEnd time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Save results summary
    summary_file = "all_domains_training_summary.json"
    with open(summary_file, 'w') as f:
        json.dump({
            'timestamp': datetime.now().isoformat(),
            'total_configs': len(configs),
            'successful': len(results['successful']),
            'failed': len(results['failed']),
            'successful_configs': results['successful'],
            'failed_configs': results['failed']
        }, f, indent=2)
    
    print(f"\nTraining summary saved to: {summary_file}")

if __name__ == "__main__":
    main()
