#!/usr/bin/env python3
"""
Cross-model evaluation script for RNN classifiers.
Tests classifiers trained on one model version and evaluates them across other model versions.
"""

import torch
import torch.nn as nn
import numpy as np
import json
import argparse
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

class CrossModelEvaluator:
    def __init__(self, model_configs: List[Dict], domains: List[str], 
                 output_dir: str = "outputs/domain_experiments"):
        """
        Initialize CrossModelEvaluator with multiple model configurations.
        
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
    
    def run_cross_model_evaluation(self):
        """Run the complete cross-model evaluation"""
        print("Starting cross-model evaluation...")
        print("=" * 80)
        
        # Load training results
        print("Loading training results from domain experiments...")
        training_results = self.load_training_results()
        
        for domain in self.domains:
            print(f"\n{'='*20} {domain.upper()} DOMAIN {'='*20}")
            
            # Store results for this domain
            self.results[domain] = {
                'train_results': {},  # Training model results (diagonal)
                'cross_model_results': {}  # Cross-model results
            }
            
            # Train classifiers for each model
            classifiers = {}
            for model_idx, model_id in enumerate(self.models):
                model_name = self.model_names[model_idx]
                print(f"\nTraining on {model_name} ({model_id}):")
                
                # Load model configuration
                exp_name = self.get_experiment_name(domain, model_id)
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
                classifiers[model_id] = model
                
                # Store training results (diagonal)
                if training_results[domain][model_id] is not None:
                    self.results[domain]['train_results'][model_id] = {
                        'model_name': model_name,
                        'mean_auroc': training_results[domain][model_id]
                    }
                    print(f"  - Training AUROC: {training_results[domain][model_id]:.4f}")
            
            # Cross-model evaluation
            print(f"\nCross-model evaluation for {domain} domain:")
            cross_model_matrix = np.zeros((len(self.models), len(self.models)))
            
            for i, train_model in enumerate(self.models):
                for j, test_model in enumerate(self.models):
                    if i == j:
                        # Diagonal: use training results
                        if train_model in self.results[domain]['train_results']:
                            cross_model_matrix[i, j] = self.results[domain]['train_results'][train_model]['mean_auroc']
                    else:
                        # Off-diagonal: cross-model evaluation
                        if train_model in classifiers and test_model in classifiers:
                            try:
                                auroc = self.evaluate_cross_model(classifiers[train_model], domain, train_model, test_model)
                                cross_model_matrix[i, j] = auroc
                            except Exception as e:
                                print(f"    Error evaluating {train_model} -> {test_model}: {str(e)}")
                                cross_model_matrix[i, j] = 0.0
            
            self.results[domain]['cross_model_matrix'] = cross_model_matrix
            
            # Print summary for this domain
            self.print_domain_summary(domain)
    
    def evaluate_cross_model(self, classifier: BiLSTMClassifier, domain: str, train_model: str, test_model: str) -> float:
        """Evaluate a classifier trained on one model and tested on another model."""
        print(f"  Evaluating {train_model} -> {test_model} on {domain} domain...")
        
        # Get test data
        human_file, model_file = self.get_data_paths(domain, test_model)
        
        # Load test data
        test_loader, n_human, n_model = self.load_test_data(human_file, model_file, batch_size=32)
        
        # Evaluate model
        metrics = self.evaluate_model(classifier, test_loader, device='cpu')
        
        print(f"    AUROC: {metrics['auc']:.4f}")
        return metrics['auc']
    
    def print_domain_summary(self, domain: str):
        """Print a summary of results for a specific domain"""
        print(f"\n{'='*20} {domain.upper()} DOMAIN Summary {'='*20}")
        
        # Training model results
        print("\nTraining Model Results:")
        print("-" * 50)
        for model_id in self.models:
            if model_id in self.results[domain]['train_results']:
                result = self.results[domain]['train_results'][model_id]
                print(f"{result['model_name']:25}: {result['mean_auroc']:.4f}")
        
        # Cross-model matrix
        matrix = self.results[domain]['cross_model_matrix']
        print(f"\nCross-Model Evaluation Matrix:")
        print("-" * 50)
        
        # Header
        print(f"{'Train\\Test':>25}", end="")
        for model_id in self.models:
            model_name = next(config['name'] for config in self.model_configs if config['id'] == model_id)
            print(f"{model_name:>15}", end="")
        print()
        
        # Matrix rows
        for i, train_model in enumerate(self.models):
            train_name = next(config['name'] for config in self.model_configs if config['id'] == train_model)
            print(f"{train_name:>25}", end="")
            for j, test_model in enumerate(self.models):
                value = matrix[i, j]
                print(f"{value:>15.4f}", end="")
            print()
    
    def plot_confusion_matrices(self, save_path: str):
        """Plot confusion matrices for all domains"""
        fig, axes = plt.subplots(2, 3, figsize=(20, 12))
        axes = axes.flatten()
        
        for idx, domain in enumerate(self.domains):
            matrix = self.results[domain]['cross_model_matrix']
            model_names = [next(config['name'] for config in self.model_configs if config['id'] == model_id) 
                          for model_id in self.models]
            
            # Create heatmap
            sns.heatmap(
                matrix, 
                annot=True, 
                fmt='.4f',
                xticklabels=model_names,
                yticklabels=model_names,
                cmap='RdYlBu_r',
                vmin=0.0,
                vmax=1.0,
                ax=axes[idx],
                cbar_kws={'label': 'AUROC Score'}
            )
            
            axes[idx].set_title(f'{domain.upper()} Domain\nCross-Model AUROC Matrix', fontsize=12, fontweight='bold')
            axes[idx].set_xlabel('Test Model', fontsize=10)
            axes[idx].set_ylabel('Train Model', fontsize=10)
            axes[idx].tick_params(axis='both', which='major', labelsize=8)
            
            # Rotate x-axis labels for better readability
            axes[idx].tick_params(axis='x', rotation=45)
            # Set horizontal alignment for x-axis labels
            for label in axes[idx].get_xticklabels():
                label.set_horizontalalignment('right')
        
        # Hide the last subplot if we have 5 domains
        if len(self.domains) < 6:
            axes[-1].set_visible(False)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"\nConfusion matrices saved to: {save_path}")
    
    def save_results(self, save_path: str):
        """Save all results to a pickle file"""
        with open(save_path, 'wb') as f:
            pickle.dump(self.results, f)
        print(f"\nResults saved to: {save_path}")
    
    def print_overall_summary(self):
        """Print overall summary across all domains"""
        print(f"\n{'='*20} Overall Summary {'='*20}")
        
        for domain in self.domains:
            print(f"\n{domain.upper()} Domain:")
            print("-" * 30)
            
            # Average performance on training models
            train_scores = []
            for model_id in self.models:
                if model_id in self.results[domain]['train_results']:
                    train_scores.append(self.results[domain]['train_results'][model_id]['mean_auroc'])
            
            if train_scores:
                print(f"Average training model AUROC: {np.mean(train_scores):.4f} (+/- {np.std(train_scores):.4f})")
            
            # Average cross-model performance (off-diagonal)
            matrix = self.results[domain]['cross_model_matrix']
            off_diagonal = []
            for i in range(len(self.models)):
                for j in range(len(self.models)):
                    if i != j:
                        off_diagonal.append(matrix[i, j])
            
            if off_diagonal:
                print(f"Average cross-model AUROC: {np.mean(off_diagonal):.4f} (+/- {np.std(off_diagonal):.4f})")
            
            # Model transferability (average performance when each model is used as target)
            for model_id in self.models:
                model_name = next(config['name'] for config in self.model_configs if config['id'] == model_id)
                model_idx = self.models.index(model_id)
                target_scores = matrix[:, model_idx]  # All models tested on this model
                print(f"  {model_name} as target: {np.mean(target_scores):.4f} (+/- {np.std(target_scores):.4f})")


def exp_claude_haiku():
    """Run Claude-Haiku cross-model evaluation experiment"""
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
    evaluator = CrossModelEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_model_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-haiku_cross_model_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-haiku_cross_model_results.pkl")
    
    print("\nClaude-Haiku cross-model evaluation completed!")
    
    return evaluator


def exp_claude_sonnet():
    """Run Claude-Sonnet cross-model evaluation experiment"""
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
    evaluator = CrossModelEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_model_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-sonnet_cross_model_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-sonnet_cross_model_results.pkl")
    
    print("\nClaude-Sonnet cross-model evaluation completed!")
    
    return evaluator


def exp_claude_sonnet_gpt4_gpt4o():
    """Run cross-model evaluation between Claude-3.5-Sonnet, ChatGPT-4o, and GPT-4-Turbo."""
    # Cross-model configuration
    domains = ["harmful", "writing", "xsum", "peerread", "pubmed"]
    model_configs = [
        {
            'name': "Claude-3.5-Sonnet (2024-10-22)",
            'id': "claude-3-5-sonnet-20241022",
            'data_dir': "../data/Claude/Claude-Sonnet"
        },
        {
            'name': "GPT-4-Turbo (2024-04-09)",
            'id': "gpt-4-turbo-2024-04-09",
            'data_dir': "../data/GPT4"
        },        
        {
            'name': "ChatGPT-4o-Latest",
            'id': "chatgpt-4o-latest",
            'data_dir': "../data/GPT4o"
        }
    ]
    
    # Initialize evaluator
    evaluator = CrossModelEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_model_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-sonnet_gpt4_gpt4o_cross_model_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-sonnet_gpt4_gpt4o_cross_model_results.pkl")
    
    print("\nClaude-Sonnet vs GPT-4 vs GPT-4o cross-model evaluation completed!")
    
    return evaluator


def main():
    """Main function to run the cross-model evaluation"""
    print("Starting cross-model evaluation for all experiments...")
    print("=" * 80)
    
    # Run Claude-Haiku experiment
    print("\nRunning Claude-Haiku cross-model evaluation...")
    # evaluator_haiku = exp_claude_haiku()
    
    # Run Claude-Sonnet experiment
    print("\nRunning Claude-Sonnet cross-model evaluation...")
    # evaluator_sonnet = exp_claude_sonnet()
    
    # Run cross-model experiment between Claude-Sonnet and GPT-4
    print("\nRunning Claude-Sonnet vs GPT-4 vs GPT-4o cross-model evaluation...")
    evaluator_cross = exp_claude_sonnet_gpt4_gpt4o()
    
    print("\nAll cross-model evaluations completed!")


if __name__ == "__main__":
    main()
