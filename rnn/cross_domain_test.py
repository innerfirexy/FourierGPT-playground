#!/usr/bin/env python3
"""
Cross-domain testing script for RNN classifiers
Tests classifiers trained on one domain on other domains
"""

import torch
import torch.nn as nn
import numpy as np
import json
import os
import sys
from tqdm import tqdm
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
from data_loader import NLLDataset, collate_fn
from model import BiLSTMClassifier
from torch.utils.data import DataLoader
from typing import Dict, List, Tuple
import matplotlib.pyplot as plt
import seaborn as sns
import pickle

class CrossDomainEvaluator:
    def __init__(self, model_configs: List[Dict], domains: List[str], 
                 output_dir: str = "outputs/domain_experiments"):
        """
        Initialize CrossDomainEvaluator with multiple model configurations.
        
        Args:
            model_configs: List of dictionaries, each containing:
                - 'name': Model name for display
                - 'id': Model identifier
                - 'data_dir': Directory containing the model's data
            domains: List of domain names
            output_dir: Directory containing trained models
        """
        self.model_configs = model_configs
        self.domains = domains
        self.models = [config['id'] for config in model_configs]
        self.model_names = [config['name'] for config in model_configs]
        self.output_dir = output_dir
        
        # Results storage
        self.results = {}
        
    def get_data_paths(self, domain: str, model_id: str) -> Tuple[str, str]:
        """Get human and model data file paths for a given domain and model."""
        # Find the model configuration
        model_config = None
        for config in self.model_configs:
            if config['id'] == model_id:
                model_config = config
                break
        
        if model_config is None:
            raise ValueError(f"Model {model_id} not found in configurations")
        
        # Handle different naming patterns
        if model_id.startswith('claude-'):
            # Claude models: pattern_claude-version_human.txt
            human_file = f"{domain}_{model_id}_human.txt"
            model_file = f"{domain}_{model_id}_model.txt"
        else:
            # GPT models: pattern_version_human.txt
            human_file = f"{domain}_{model_id}_human.txt"
            model_file = f"{domain}_{model_id}_model.txt"
        
        human_path = os.path.join(model_config['data_dir'], human_file)
        model_path = os.path.join(model_config['data_dir'], model_file)
        
        if not os.path.exists(human_path):
            raise FileNotFoundError(f"Human data file not found: {human_path}")
        if not os.path.exists(model_path):
            raise FileNotFoundError(f"Model data file not found: {model_path}")
            
        return human_path, model_path
    
    def load_model(self, model_path: str, config: Dict) -> BiLSTMClassifier:
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
    
    def load_test_data(self, human_file: str, model_file: str, batch_size: int = 32):
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
    
    def evaluate_model(self, model: BiLSTMClassifier, test_loader: DataLoader, device: str = 'cpu') -> Dict:
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
    
    def get_experiment_name(self, domain: str, model_id: str) -> str:
        """Generate experiment name for a domain-model combination"""
        # Use the original model_id format as it matches the actual directory names
        return f"{domain}_{model_id}"
    
    def load_training_results(self) -> Dict:
        """Load training results from all domain experiments"""
        training_results = {}
        
        for domain in self.domains:
            training_results[domain] = {}
            for model_id in self.models:
                # Generate output directory name
                exp_name = self.get_experiment_name(domain, model_id)
                exp_dir = os.path.join(self.output_dir, exp_name)
                
                # Check if cv_results.json exists
                cv_results_path = os.path.join(exp_dir, 'cv_results.json')
                if os.path.exists(cv_results_path):
                    try:
                        with open(cv_results_path, 'r') as f:
                            cv_results = json.load(f)
                        training_results[domain][model_id] = cv_results['mean_auc']
                    except:
                        training_results[domain][model_id] = None
                else:
                    training_results[domain][model_id] = None
        
        return training_results
    
    def run_cross_domain_evaluation(self):
        """Run the complete cross-domain evaluation"""
        print("Starting cross-domain evaluation...")
        print("=" * 80)
        
        # Load training results
        print("Loading training results from domain experiments...")
        training_results = self.load_training_results()
        
        # Define all experiments
        all_experiments = []
        for domain in self.domains:
            for model_id in self.models:
                exp_name = self.get_experiment_name(domain, model_id)
                all_experiments.append((exp_name, domain, model_id))
        
        for exp_name, train_domain, model_id in all_experiments:
            print(f"\nTesting classifier trained on {exp_name} ({model_id})")
            print("-" * 60)
            
            # Load model configuration
            config_path = os.path.join(self.output_dir, exp_name, 'config.json')
            if not os.path.exists(config_path):
                print(f"Warning: Config not found at {config_path}")
                continue
                
            with open(config_path, 'r') as f:
                config = json.load(f)
            
            # Load the best performing fold model (using fold 1 for simplicity)
            model_path = os.path.join(self.output_dir, exp_name, 'fold_1_model.pth')
            
            if not os.path.exists(model_path):
                print(f"Warning: Model not found at {model_path}")
                continue
                
            model = self.load_model(model_path, config)
            
            # Store results for this experiment
            self.results[exp_name] = {
                'train_domain': train_domain,
                'model_id': model_id,
                'model_name': next(config['name'] for config in self.model_configs if config['id'] == model_id),
                'domain_results': {}
            }
            
            # Test on all domains except the training domain
            for test_domain in self.domains:
                if test_domain == train_domain:
                    continue  # Skip same domain testing
                    
                print(f"  Testing on {test_domain} domain...")
                
                try:
                    # Get data paths
                    human_file, model_file = self.get_data_paths(test_domain, model_id)
                    
                    # Load test data
                    test_loader, n_human, n_model = self.load_test_data(human_file, model_file, batch_size=32)
                    
                    # Evaluate model
                    metrics = self.evaluate_model(model, test_loader, device='cpu')
                    
                    self.results[exp_name]['domain_results'][test_domain] = {
                        'metrics': metrics,
                        'data_size': {'human': n_human, 'model': n_model}
                    }
                    
                    print(f"    Accuracy: {metrics['accuracy']:.3f}, AUC: {metrics['auc']:.3f}")
                    
                except Exception as e:
                    print(f"    Error testing on {test_domain}: {str(e)}")
                    continue
        
        # Add training results to the results
        self.training_results = training_results
    
    def print_domain_summary(self, domain: str):
        """Print a summary of results for a specific domain"""
        print(f"\n{'='*20} {domain.upper()} DOMAIN Summary {'='*20}")
        
        # Find all experiments for this domain
        domain_experiments = []
        for exp_name, exp_data in self.results.items():
            if exp_data['train_domain'] == domain:
                domain_experiments.append((exp_name, exp_data))
        
        if not domain_experiments:
            print(f"No experiments found for {domain} domain")
            return
        
        # Print header
        print(f"{'Model':<25} {'Test Domain':<12} {'Accuracy':<10} {'AUC':<10} {'F1':<10}")
        print("-" * 80)
        
        for exp_name, exp_data in domain_experiments:
            model_name = exp_data['model_name']
            for test_domain, domain_data in exp_data['domain_results'].items():
                metrics = domain_data['metrics']
                print(f"{model_name:<25} {test_domain:<12} {metrics['accuracy']:<10.3f} {metrics['auc']:<10.3f} {metrics['f1']:<10.3f}")
    
    def plot_confusion_matrices(self, save_path: str):
        """Create confusion matrices for all model versions"""
        # Create figure with subplots (one for each model version)
        n_models = len(self.models)
        fig, axes = plt.subplots(1, n_models, figsize=(6*n_models, 6))
        if n_models == 1:
            axes = [axes]
        
        for idx, model_id in enumerate(self.models):
            ax = axes[idx]
            model_name = self.model_names[idx]
            
            # Create confusion matrix
            confusion_matrix = np.zeros((len(self.domains), len(self.domains)))
            
            # Map domain names to indices
            domain_to_idx = {domain: i for i, domain in enumerate(self.domains)}
            
            # Fill diagonal with training results (same domain performance)
            for i, domain in enumerate(self.domains):
                if self.training_results[domain][model_id] is not None:
                    confusion_matrix[i, i] = self.training_results[domain][model_id]
            
            # Fill cross-domain results
            # Find all experiments for this model version
            for exp_name, exp_data in self.results.items():
                if exp_data['model_id'] == model_id:
                    train_domain = exp_data['train_domain']
                    train_idx = domain_to_idx[train_domain]
                    
                    # Fill cross-domain results
                    for test_domain, domain_data in exp_data['domain_results'].items():
                        if test_domain in domain_to_idx:
                            test_idx = domain_to_idx[test_domain]
                            confusion_matrix[train_idx, test_idx] = domain_data['metrics']['auc']
            
            # Create heatmap
            sns.heatmap(confusion_matrix, 
                       annot=True, 
                       fmt='.3f', 
                       cmap='RdYlBu_r', 
                       vmin=0.0, 
                       vmax=1.0,
                       cbar_kws={'label': 'AUC'},
                       ax=ax)
            
            ax.set_title(f'{model_name}', fontsize=14, fontweight='bold')
            ax.set_xlabel('Testing Domain', fontsize=12)
            ax.set_ylabel('Training Domain', fontsize=12)
            ax.set_xticklabels(self.domains, rotation=45, ha='right')
            ax.set_yticklabels(self.domains, rotation=0)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"Confusion matrices saved to {save_path}")
        
        return fig
    
    def save_results(self, save_path: str):
        """Save all results to a JSON file"""
        # Convert numpy arrays to lists for JSON serialization
        results_copy = {}
        for exp_name, exp_data in self.results.items():
            results_copy[exp_name] = {
                'train_domain': exp_data['train_domain'],
                'model_id': exp_data['model_id'],
                'model_name': exp_data['model_name'],
                'domain_results': {}
            }
            for test_domain, domain_data in exp_data['domain_results'].items():
                results_copy[exp_name]['domain_results'][test_domain] = {
                    'metrics': domain_data['metrics'],
                    'data_size': domain_data['data_size']
                }
        
        with open(save_path, 'w') as f:
            json.dump(results_copy, f, indent=2)
        print(f"Results saved to {save_path}")
    
    def print_overall_summary(self):
        """Print overall summary across all domains"""
        print(f"\n{'='*20} Overall Summary {'='*20}")
        
        for domain in self.domains:
            print(f"\n{domain.upper()} Domain:")
            print("-" * 30)
            
            # Average performance on training models
            train_scores = []
            for model_id in self.models:
                if self.training_results[domain][model_id] is not None:
                    train_scores.append(self.training_results[domain][model_id])
            
            if train_scores:
                print(f"Average training model AUROC: {np.mean(train_scores):.4f} (+/- {np.std(train_scores):.4f})")
            
            # Average cross-domain performance
            cross_domain_scores = []
            for exp_name, exp_data in self.results.items():
                if exp_data['train_domain'] == domain:
                    for test_domain, domain_data in exp_data['domain_results'].items():
                        cross_domain_scores.append(domain_data['metrics']['auc'])
            
            if cross_domain_scores:
                print(f"Average cross-domain AUROC: {np.mean(cross_domain_scores):.4f} (+/- {np.std(cross_domain_scores):.4f})")


def exp_claude_haiku():
    """Run Claude-Haiku cross-domain evaluation experiment"""
    # Claude-Haiku specific configuration
    domains = ["harmful", "writing", "xsum", "peerread", "pubmed"]
    model_configs = [
        {
            'name': "Claude-3-Opus",
            'id': "claude-3-opus-20240229",
            'data_dir': "../data/Claude/Claude-Haiku"
        },
        {
            'name': "Claude-3-Haiku",
            'id': "claude-3-haiku-20240307",
            'data_dir': "../data/Claude/Claude-Haiku"
        },
        {
            'name': "Claude-3.5-Haiku",
            'id': "claude-3-5-haiku-20241022",
            'data_dir': "../data/Claude/Claude-Haiku"
        }
    ]
    
    # Initialize evaluator
    evaluator = CrossDomainEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_domain_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-haiku_cross_domain_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-haiku_cross_domain_results.json")
    
    print("\nClaude-Haiku cross-domain evaluation completed!")
    
    return evaluator


def exp_gpt4():
    """Run GPT-4 cross-domain evaluation experiment"""
    # GPT-4 specific configuration
    domains = ["harmful", "writing", "xsum", "peerread", "pubmed"]
    model_configs = [
        {
            'name': "GPT-4",
            'id': "gpt-4",
            'data_dir': "../data/GPT4"
        },
        {
            'name': "GPT-4-1106-Preview",
            'id': "gpt-4-1106-preview",
            'data_dir': "../data/GPT4"
        },
        {
            'name': "GPT-4-0125-Preview",
            'id': "gpt-4-0125-preview",
            'data_dir': "../data/GPT4"
        },
        {
            'name': "GPT-4-Turbo-2024-04-09",
            'id': "gpt-4-turbo-2024-04-09",
            'data_dir': "../data/GPT4"
        }
    ]
    
    # Initialize evaluator
    evaluator = CrossDomainEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_domain_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("gpt4_cross_domain_matrices.pdf")
    
    # Save results
    # evaluator.save_results("gpt4_cross_domain_results.json")
    
    print("\nGPT-4 cross-domain evaluation completed!")
    
    return evaluator


def exp_claude_sonnet():
    """Run Claude-Sonnet cross-domain evaluation experiment"""
    # Claude-Sonnet specific configuration
    domains = ["harmful", "writing", "xsum", "peerread", "pubmed"]
    model_configs = [
        {
            'name': "Claude-3-Sonnet",
            'id': "claude-3-sonnet-20240229",
            'data_dir': "../data/Claude/Claude-Sonnet"
        },
        {
            'name': "Claude-3.5-Sonnet (2024-06-20)",
            'id': "claude-3-5-sonnet-20240620",
            'data_dir': "../data/Claude/Claude-Sonnet"
        },
        {
            'name': "Claude-3.5-Sonnet (2024-10-22)",
            'id': "claude-3-5-sonnet-20241022",
            'data_dir': "../data/Claude/Claude-Sonnet"
        }
    ]
    
    # Initialize evaluator
    evaluator = CrossDomainEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_domain_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-sonnet_cross_domain_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-sonnet_cross_domain_results.json")
    
    print("\nClaude-Sonnet cross-domain evaluation completed!")
    
    return evaluator


def main():
    """Main function to run the cross-domain evaluation"""
    print("Starting cross-domain evaluation for all experiments...")
    print("=" * 80)
    
    # Run Claude-Haiku experiment
    print("\nRunning Claude-Haiku cross-domain evaluation...")
    # evaluator_haiku = exp_claude_haiku()
    
    # Run Claude-Sonnet experiment
    print("\nRunning Claude-Sonnet cross-domain evaluation...")
    # evaluator_sonnet = exp_claude_sonnet()
    
    # Run GPT-4 experiment
    print("\nRunning GPT-4 cross-domain evaluation...")
    evaluator_gpt4 = exp_gpt4()
    
    print("\nAll cross-domain evaluations completed!")


if __name__ == "__main__":
    main()
