#!/usr/bin/env python3
"""
Cross-model evaluation script for Claude models.
Trains classifiers on one model version and evaluates them across other model versions.
"""

import os
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from typing import Dict, List, Tuple
import pickle
import tempfile
import subprocess
from pathlib import Path

# Add the parent directory to the path to import train_sup_cls
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from train_sup_cls import train_classifier, get_features, get_circular_mean


class CrossModelEvaluator:
    def __init__(self, model_configs: List[Dict], domains: List[str]):
        """
        Initialize CrossModelEvaluator with multiple model configurations.
        
        Args:
            model_configs: List of dictionaries, each containing:
                - 'name': Model name for display
                - 'id': Model identifier
                - 'data_dir': Directory containing the model's data
            domains: List of domain names
        """
        self.model_configs = model_configs
        self.domains = domains
        self.models = [config['id'] for config in model_configs]
        self.model_names = [config['name'] for config in model_configs]
        
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
        
        human_file = f"{domain}_{model_id}_human.txt"
        model_file = f"{domain}_{model_id}_model.txt"
        
        human_path = os.path.join(model_config['data_dir'], human_file)
        model_path = os.path.join(model_config['data_dir'], model_file)
        
        if not os.path.exists(human_path):
            raise FileNotFoundError(f"Human data file not found: {human_path}")
        if not os.path.exists(model_path):
            raise FileNotFoundError(f"Model data file not found: {model_path}")
            
        return human_path, model_path
    
    def train_model_classifier(self, domain: str, train_model: str) -> Tuple[object, Dict, float, np.ndarray]:
        """Train a classifier for a specific domain and training model."""
        print(f"Training classifier for {domain} domain with {train_model}...")
        
        human_path, model_path = self.get_data_paths(domain, train_model)
        
        # Train the classifier
        best_estimator, best_params, best_score, cv_scores = train_classifier(
            human_path, model_path, save_intermid=False
        )
        
        print(f"  - Best AUROC: {best_score:.4f}")
        print(f"  - CV scores: {cv_scores}")
        print(f"  - Mean CV AUROC: {np.mean(cv_scores):.4f} (+/- {np.std(cv_scores) * 2:.4f})")
        
        return best_estimator, best_params, best_score, cv_scores
    
    def evaluate_cross_model(self, classifier: object, domain: str, train_model: str, test_model: str) -> float:
        """Evaluate a classifier trained on one model and tested on another model."""
        print(f"  Evaluating {train_model} -> {test_model} on {domain} domain...")
        
        # Get test data
        human_path, model_path = self.get_data_paths(domain, test_model)
        
        # Process test data
        human_circlemean = get_circular_mean(human_path)
        model_circlemean = get_circular_mean(model_path)
        
        x_human = get_features(human_circlemean)
        y_human = np.zeros(x_human.shape[0])
        x_model = get_features(model_circlemean)
        y_model = np.ones(x_model.shape[0])
        
        x_test = np.concatenate([x_human, x_model], axis=0)
        y_test = np.concatenate([y_human, y_model], axis=0)
        
        # Get predictions
        y_pred_proba = classifier.predict_proba(x_test)[:, 1]
        
        # Calculate AUROC
        from sklearn.metrics import roc_auc_score
        auroc = roc_auc_score(y_test, y_pred_proba)
        
        print(f"    AUROC: {auroc:.4f}")
        return auroc
    
    def run_cross_model_evaluation(self):
        """Run the complete cross-model evaluation."""
        print("Starting cross-model evaluation...")
        print("=" * 80)
        
        for domain in self.domains:
            print(f"\n{'='*20} {domain.upper()} DOMAIN {'='*20}")
            
            # Store results for this domain
            self.results[domain] = {
                'train_results': {},  # Training model results (diagonal)
                'cross_model_results': {}  # Cross-model results
            }
            
            # Train classifiers for each model
            classifiers = {}
            for model_idx, model in enumerate(self.models):
                model_name = self.model_names[model_idx]
                print(f"\nTraining on {model_name} ({model}):")
                classifier, params, score, cv_scores = self.train_model_classifier(domain, model)
                classifiers[model] = classifier
                
                # Store training results (diagonal)
                self.results[domain]['train_results'][model] = {
                    'model_name': model_name,
                    'mean_auroc': np.mean(cv_scores),
                    'std_auroc': np.std(cv_scores),
                    'cv_scores': cv_scores
                }
            
            # Cross-model evaluation
            print(f"\nCross-model evaluation for {domain} domain:")
            cross_model_matrix = np.zeros((len(self.models), len(self.models)))
            
            for i, train_model in enumerate(self.models):
                for j, test_model in enumerate(self.models):
                    if i == j:
                        # Diagonal: use training results
                        cross_model_matrix[i, j] = self.results[domain]['train_results'][train_model]['mean_auroc']
                    else:
                        # Off-diagonal: cross-model evaluation
                        auroc = self.evaluate_cross_model(classifiers[train_model], domain, train_model, test_model)
                        cross_model_matrix[i, j] = auroc
            
            self.results[domain]['cross_model_matrix'] = cross_model_matrix
            
            # Print summary for this domain
            self.print_domain_summary(domain)
    
    def print_domain_summary(self, domain: str):
        """Print a summary of results for a specific domain."""
        print(f"\n{'='*20} {domain.upper()} DOMAIN Summary {'='*20}")
        
        # Training model results
        print("\nTraining Model Results (5-fold CV):")
        print("-" * 50)
        for model in self.models:
            result = self.results[domain]['train_results'][model]
            print(f"{result['model_name']:25}: {result['mean_auroc']:.4f} (+/- {result['std_auroc'] * 2:.4f})")
        
        # Cross-model matrix
        matrix = self.results[domain]['cross_model_matrix']
        print(f"\nCross-Model Evaluation Matrix:")
        print("-" * 50)
        
        # Header
        print(f"{'Train\\Test':>25}", end="")
        for model in self.models:
            model_name = self.results[domain]['train_results'][model]['model_name']
            print(f"{model_name:>15}", end="")
        print()
        
        # Matrix rows
        for i, train_model in enumerate(self.models):
            train_name = self.results[domain]['train_results'][train_model]['model_name']
            print(f"{train_name:>25}", end="")
            for j, test_model in enumerate(self.models):
                value = matrix[i, j]
                print(f"{value:>15.4f}", end="")
            print()
    
    def plot_confusion_matrices(self, save_path: str):
        """Plot confusion matrices for all domains."""
        fig, axes = plt.subplots(2, 3, figsize=(20, 12))
        axes = axes.flatten()
        
        for idx, domain in enumerate(self.domains):
            matrix = self.results[domain]['cross_model_matrix']
            model_names = [self.results[domain]['train_results'][model]['model_name'] for model in self.models]
            
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
        # plt.show()
    
    def save_results(self, save_path: str):
        """Save all results to a pickle file."""
        with open(save_path, 'wb') as f:
            pickle.dump(self.results, f)
        print(f"\nResults saved to: {save_path}")
    
    def print_overall_summary(self):
        """Print overall summary across all domains."""
        print(f"\n{'='*20} Overall Summary {'='*20}")
        
        for domain in self.domains:
            print(f"\n{domain.upper()} Domain:")
            print("-" * 30)
            
            # Average performance on training models
            train_scores = [self.results[domain]['train_results'][model]['mean_auroc'] 
                           for model in self.models]
            print(f"Average training model AUROC: {np.mean(train_scores):.4f} (+/- {np.std(train_scores):.4f})")
            
            # Average cross-model performance (off-diagonal)
            matrix = self.results[domain]['cross_model_matrix']
            off_diagonal = []
            for i in range(len(self.models)):
                for j in range(len(self.models)):
                    if i != j:
                        off_diagonal.append(matrix[i, j])
            
            print(f"Average cross-model AUROC: {np.mean(off_diagonal):.4f} (+/- {np.std(off_diagonal):.4f})")
            
            # Model transferability (average performance when each model is used as target)
            for model in self.models:
                model_name = self.results[domain]['train_results'][model]['model_name']
                model_idx = self.models.index(model)
                target_scores = matrix[:, model_idx]  # All models tested on this model
                print(f"  {model_name} as target: {np.mean(target_scores):.4f} (+/- {np.std(target_scores):.4f})")


def exp_claude_haiku():
    """Run Claude-Haiku cross-model evaluation experiment."""
    # Claude-Haiku specific configuration
    domains = ["pubmed", "peerread", "harmful", "xsum", "writing"]
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
    """Run Claude-Sonnet cross-model evaluation experiment."""
    # Claude-Sonnet specific configuration
    domains = ["pubmed", "peerread", "harmful", "xsum", "writing"]
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


def exp_claude_sonnet_gpt4():
    """Run cross-model evaluation between Claude-3.5-Sonnet, ChatGPT-4o, and GPT-4-Turbo."""
    # Cross-model configuration
    domains = ["pubmed", "peerread", "harmful", "xsum", "writing"]
    model_configs = [
        {
            'name': "Claude-3.5-Sonnet (2024-10-22)",
            'id': "claude-3-5-sonnet-20241022",
            'data_dir': "../data/Claude/Claude-Sonnet"
        },
        {
            'name': "ChatGPT-4o-Latest",
            'id': "chatgpt-4o-latest",
            'data_dir': "../data/GPT4o"
        },
        {
            'name': "GPT-4-Turbo (2024-04-09)",
            'id': "gpt-4-turbo-2024-04-09",
            'data_dir': "../data/GPT4"
        }
    ]
    
    # Initialize evaluator
    evaluator = CrossModelEvaluator(model_configs, domains)
    
    # Run the evaluation
    evaluator.run_cross_model_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude_sonnet_gpt4_gpt4o_cross_model_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude_sonnet_gpt4_cross_model_results.pkl")
    
    print("\nClaude-Sonnet vs GPT-4 cross-model evaluation completed!")
    
    return evaluator


def main():
    """Main function to run the cross-model evaluation."""
    # Run Claude-Haiku experiment
    # print("Running Claude-Haiku cross-model evaluation...")
    # evaluator_haiku = exp_claude_haiku()
    
    
    # Run Claude-Sonnet experiment
    # print("Running Claude-Sonnet cross-model evaluation...")
    # evaluator_sonnet = exp_claude_sonnet()
    
    
    # Run cross-model experiment between Claude-Sonnet and GPT-4
    print("Running Claude-Sonnet vs GPT-4 cross-model evaluation...")
    evaluator_cross = exp_claude_sonnet_gpt4()


if __name__ == "__main__":
    main()
