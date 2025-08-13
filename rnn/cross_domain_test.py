#!/usr/bin/env python3
"""
Cross-domain testing script for RNN classifiers
Tests classifiers trained on Harmful domain on other domains (writing, xsum, peerread, pubmed)
"""

import torch
import torch.nn as nn
import numpy as np
import json
import argparse
import os
from tqdm import tqdm
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
from data_loader import NLLDataset, collate_fn
from model import BiLSTMClassifier
from torch.utils.data import DataLoader

def load_model(model_path, config):
    """Load a trained RNN model"""
    model_config = config['model']
    model = BiLSTMClassifier(
        input_size=model_config['input_size'],
        hidden_size=model_config['hidden_size'],
        num_layers=model_config['num_layers'],
        num_classes=model_config['num_classes'],
        dropout=model_config['dropout']
    )
    
    try:
        # Try with weights_only=True first (PyTorch 2.6+ default)
        checkpoint = torch.load(model_path, map_location='cpu', weights_only=True)
    except:
        # Fallback to weights_only=False for compatibility
        checkpoint = torch.load(model_path, map_location='cpu', weights_only=False)
    
    model.load_state_dict(checkpoint['model_state_dict'])
    model.eval()
    return model

def load_test_data(human_file, model_file, batch_size=32):
    """Load test data from files"""
    # Load combined dataset
    dataset = NLLDataset(human_file, model_file)
    
    # Create data loader
    test_loader = DataLoader(
        dataset,
        batch_size=batch_size,
        shuffle=False,
        collate_fn=collate_fn
    )
    
    # Count samples (assuming equal number of human and model samples)
    total_samples = len(dataset)
    n_human = total_samples // 2
    n_model = total_samples // 2
    
    return test_loader, n_human, n_model

def evaluate_model(model, test_loader, device='cpu'):
    """Evaluate model on test data"""
    model.to(device)
    model.eval()
    
    all_predictions = []
    all_labels = []
    all_probabilities = []
    
    with torch.no_grad():
        for sequences, lengths, labels in tqdm(test_loader, desc="Testing"):
            sequences = sequences.to(device)
            lengths = lengths.to(device)
            labels = labels.to(device)
            
            # Forward pass
            outputs = model(sequences, lengths)
            probabilities = torch.softmax(outputs, dim=1)
            
            # Get predictions
            _, predicted = torch.max(outputs.data, 1)
            
            # Store results
            all_predictions.extend(predicted.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())
            all_probabilities.extend(probabilities[:, 1].cpu().numpy())  # Probability of class 1 (AI)
    
    # Calculate metrics
    accuracy = float(accuracy_score(all_labels, all_predictions))
    precision = float(precision_score(all_labels, all_predictions))
    recall = float(recall_score(all_labels, all_predictions))
    f1 = float(f1_score(all_labels, all_predictions))
    auc = float(roc_auc_score(all_labels, all_probabilities))
    
    return {
        'accuracy': accuracy,
        'precision': precision,
        'recall': recall,
        'f1': f1,
        'auc': auc,
        'predictions': [int(p) for p in all_predictions],
        'labels': [int(l) for l in all_labels],
        'probabilities': [float(p) for p in all_probabilities]
    }

def get_domain_data_paths(domain, model_version):
    """Get data file paths for a specific domain and model version"""
    base_path = "../data/Claude/Claude-Haiku"
    
    # Map domain names to file patterns
    domain_patterns = {
        'writing': 'writing',
        'xsum': 'xsum', 
        'peerread': 'peerread',
        'pubmed': 'pubmed',
        'harmful': 'harmful'
    }
    
    if domain not in domain_patterns:
        raise ValueError(f"Unknown domain: {domain}")
    
    pattern = domain_patterns[domain]
    human_file = f"{base_path}/{pattern}_claude-{model_version}_human.txt"
    model_file = f"{base_path}/{pattern}_claude-{model_version}_model.txt"
    
    return human_file, model_file

def test_cross_domain_performance():
    """Test cross-domain performance of all trained classifiers"""
    
    # Define domains and model versions to test
    domains = ['harmful', 'writing', 'xsum', 'peerread', 'pubmed']
    model_versions = ['3-opus-20240229', '3-haiku-20240307', '3-5-haiku-20241022']
    
    # Define all experiments
    all_experiments = []
    for domain in domains:
        for model_version in model_versions:
            short_version = model_version.replace('-', '_').replace('.', '')
            exp_name = f"{domain}_{short_version}"
            all_experiments.append((exp_name, domain, model_version))
    
    results = {}
    
    print("="*80)
    print("CROSS-DOMAIN PERFORMANCE TESTING")
    print("="*80)
    
    for exp_name, train_domain, model_version in all_experiments:
        print(f"\nTesting classifier trained on {exp_name} ({model_version})")
        print("-" * 60)
        
        # Load model configuration
        config_path = f"outputs/domain_experiments/{exp_name}/config.json"
        if not os.path.exists(config_path):
            print(f"Warning: Config not found at {config_path}")
            continue
            
        with open(config_path, 'r') as f:
            config = json.load(f)
        
        # Load the best performing fold model (using fold 1 for simplicity)
        model_path = f"outputs/domain_experiments/{exp_name}/fold_1_model.pth"
        
        if not os.path.exists(model_path):
            print(f"Warning: Model not found at {model_path}")
            continue
            
        model = load_model(model_path, config)
        
        domain_results = {}
        
        # Test on all domains except the training domain
        for test_domain in domains:
            if test_domain == train_domain:
                continue  # Skip same domain testing
                
            print(f"  Testing on {test_domain} domain...")
            
            try:
                # Get data paths
                human_file, model_file = get_domain_data_paths(test_domain, model_version)
                
                if not os.path.exists(human_file) or not os.path.exists(model_file):
                    print(f"    Warning: Data files not found for {test_domain}")
                    continue
                
                # Load test data
                test_loader, n_human, n_model = load_test_data(human_file, model_file, batch_size=32)
                
                # Evaluate model
                metrics = evaluate_model(model, test_loader, device='cpu')
                
                domain_results[test_domain] = {
                    'metrics': metrics,
                    'data_size': {'human': n_human, 'model': n_model}
                }
                
                print(f"    Accuracy: {metrics['accuracy']:.3f}, AUC: {metrics['auc']:.3f}")
                
            except Exception as e:
                print(f"    Error testing on {test_domain}: {str(e)}")
                continue
        
        results[exp_name] = {
            'train_domain': train_domain,
            'model_version': model_version,
            'domain_results': domain_results
        }
    
    # Save results
    output_file = "cross_domain_results.json"
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\nResults saved to {output_file}")
    
    # Print summary
    print("\n" + "="*80)
    print("CROSS-DOMAIN PERFORMANCE SUMMARY")
    print("="*80)
    
    for exp_name, exp_data in results.items():
        print(f"\n{exp_name} ({exp_data['model_version']}):")
        print(f"{'Domain':<12} {'Accuracy':<10} {'AUC':<10} {'F1':<10}")
        print("-" * 50)
        
        for domain, domain_data in exp_data['domain_results'].items():
            metrics = domain_data['metrics']
            print(f"{domain:<12} {metrics['accuracy']:<10.3f} {metrics['auc']:<10.3f} {metrics['f1']:<10.3f}")
    
    return results

def load_training_results():
    """Load training results from all domain experiments"""
    import os
    import json
    
    domains = ['harmful', 'writing', 'xsum', 'peerread', 'pubmed']
    model_versions = ['3-opus-20240229', '3-haiku-20240307', '3-5-haiku-20241022']
    
    training_results = {}
    
    for domain in domains:
        training_results[domain] = {}
        for model_version in model_versions:
            # Generate output directory name
            short_version = model_version.replace('-', '_').replace('.', '')
            exp_dir = f"outputs/domain_experiments/{domain}_{short_version}"
            
            # Check if cv_results.json exists
            cv_results_path = os.path.join(exp_dir, 'cv_results.json')
            if os.path.exists(cv_results_path):
                try:
                    with open(cv_results_path, 'r') as f:
                        cv_results = json.load(f)
                    training_results[domain][model_version] = cv_results['mean_auc']
                except:
                    training_results[domain][model_version] = None
            else:
                training_results[domain][model_version] = None
    
    return training_results

def create_confusion_matrices(results, training_results):
    """Create confusion matrices for all model versions"""
    import matplotlib.pyplot as plt
    import seaborn as sns
    
    domains = ['harmful', 'writing', 'xsum', 'peerread', 'pubmed']
    model_versions = ['3-opus-20240229', '3-haiku-20240307', '3-5-haiku-20241022']
    model_names = ['3-Opus', '3-Haiku', '3.5-Haiku']
    
    # Create figure with 3 subplots (one for each model version)
    fig, axes = plt.subplots(1, 3, figsize=(20, 6))
    
    for idx, (model_version, model_name) in enumerate(zip(model_versions, model_names)):
        ax = axes[idx]
        
        # Create confusion matrix
        confusion_matrix = np.zeros((len(domains), len(domains)))
        
        # Map domain names to indices
        domain_to_idx = {domain: i for i, domain in enumerate(domains)}
        
        # Fill diagonal with training results (same domain performance)
        for i, domain in enumerate(domains):
            if training_results[domain][model_version] is not None:
                confusion_matrix[i, i] = training_results[domain][model_version]
        
        # Fill cross-domain results
        # Find all experiments for this model version
        for exp_name, exp_data in results.items():
            if exp_data['model_version'] == model_version:
                train_domain = exp_data['train_domain']
                train_idx = domain_to_idx[train_domain]
                
                # Fill cross-domain results
                for test_domain, domain_data in exp_data['domain_results'].items():
                    if test_domain in domain_to_idx:
                        test_idx = domain_to_idx[test_domain]
                        confusion_matrix[train_idx, test_idx] = domain_data['metrics']['auc']
        
        # Debug: Print the confusion matrix
        print(f"\nConfusion Matrix for {model_name}:")
        print(confusion_matrix)
        
        # Create heatmap
        sns.heatmap(confusion_matrix, 
                   annot=True, 
                   fmt='.3f', 
                   cmap='RdYlBu_r', 
                   vmin=0.0, 
                   vmax=1.0,
                   cbar_kws={'label': 'AUC'},
                   ax=ax)
        
        ax.set_title(f'{model_name} Cross-Domain Performance', fontsize=14, fontweight='bold')
        ax.set_xlabel('Testing Domain', fontsize=12)
        ax.set_ylabel('Training Domain', fontsize=12)
        ax.set_xticklabels(domains, rotation=45, ha='right')
        ax.set_yticklabels(domains, rotation=0)
    
    plt.tight_layout()
    plt.savefig('confusion_matrices.pdf', dpi=300, bbox_inches='tight')
    print("Confusion matrices saved to confusion_matrices.pdf")
    
    return fig

def create_cross_domain_visualization(results):
    """Create visualization for cross-domain results"""
    import matplotlib.pyplot as plt
    
    # Extract data for plotting
    domains = ['writing', 'xsum', 'peerread', 'pubmed']
    model_versions = ['3-Opus', '3-Haiku', '3.5-Haiku']
    colors = ['#1f77b4', '#ff7f0e', '#2ca02c']  # Blue, Orange, Green
    
    # Create figure
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 6))
    
    # Plot accuracy
    x = np.arange(len(domains))
    width = 0.25
    
    for i, (exp_name, exp_data) in enumerate(results.items()):
        accuracies = []
        for domain in domains:
            if domain in exp_data['domain_results']:
                acc = exp_data['domain_results'][domain]['metrics']['accuracy']
                accuracies.append(acc)
            else:
                accuracies.append(0)
        
        bars = ax1.bar(x + i*width, accuracies, width, label=model_versions[i], 
                      color=colors[i], alpha=0.8)
        
        # Add value labels on bars
        for bar, acc in zip(bars, accuracies):
            height = bar.get_height()
            ax1.text(bar.get_x() + bar.get_width()/2., height + 0.01,
                    f'{acc:.3f}', ha='center', va='bottom', fontsize=9)
    
    ax1.set_title('Cross-Domain Accuracy Performance', fontsize=14, fontweight='bold')
    ax1.set_xlabel('Target Domain', fontsize=12)
    ax1.set_ylabel('Accuracy', fontsize=12)
    ax1.set_xticks(x + width)
    ax1.set_xticklabels(domains)
    ax1.legend()
    ax1.grid(True, alpha=0.3, axis='y')
    ax1.set_ylim(0.2, 0.9)
    
    # Plot AUC
    for i, (exp_name, exp_data) in enumerate(results.items()):
        aucs = []
        for domain in domains:
            if domain in exp_data['domain_results']:
                auc = exp_data['domain_results'][domain]['metrics']['auc']
                aucs.append(auc)
            else:
                aucs.append(0)
        
        bars = ax2.bar(x + i*width, aucs, width, label=model_versions[i], 
                      color=colors[i], alpha=0.8)
        
        # Add value labels on bars
        for bar, auc in zip(bars, aucs):
            height = bar.get_height()
            ax2.text(bar.get_x() + bar.get_width()/2., height + 0.01,
                    f'{auc:.3f}', ha='center', va='bottom', fontsize=9)
    
    ax2.set_title('Cross-Domain AUC Performance', fontsize=14, fontweight='bold')
    ax2.set_xlabel('Target Domain', fontsize=12)
    ax2.set_ylabel('AUC', fontsize=12)
    ax2.set_xticks(x + width)
    ax2.set_xticklabels(domains)
    ax2.legend()
    ax2.grid(True, alpha=0.3, axis='y')
    ax2.set_ylim(0.2, 0.9)
    
    plt.tight_layout()
    plt.savefig('cross_domain_performance.pdf', dpi=300, bbox_inches='tight')
    print("Cross-domain performance visualization saved to cross_domain_performance.pdf")

def main():
    """Main function"""
    print("Starting cross-domain performance testing...")
    
    # Load training results from all domain experiments
    print("Loading training results from domain experiments...")
    training_results = load_training_results()
    
    # Run cross-domain testing
    results = test_cross_domain_performance()
    
    # Create visualizations
    if results:
        print("Creating confusion matrices...")
        create_confusion_matrices(results, training_results)
    
    print("\nCross-domain testing completed!")

if __name__ == "__main__":
    main()
