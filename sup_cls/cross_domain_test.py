#!/usr/bin/env python3
"""
Cross-domain evaluation script for Claude-Haiku models.
Trains classifiers on each domain and evaluates them across all domains.
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


class CrossDomainEvaluator:
    def __init__(self, data_dir: str, domains: List[str], models: List[str], model_names: List[str]):
        self.data_dir = data_dir
        self.domains = domains
        self.models = models
        self.model_names = model_names
        
        # Results storage
        self.results = {}
        
    def get_data_paths(self, domain: str, model: str) -> Tuple[str, str]:
        """Get human and model data file paths for a given domain and model."""
        human_file = f"{domain}_{model}_human.txt"
        model_file = f"{domain}_{model}_model.txt"
        
        human_path = os.path.join(self.data_dir, human_file)
        model_path = os.path.join(self.data_dir, model_file)
        
        if not os.path.exists(human_path):
            raise FileNotFoundError(f"Human data file not found: {human_path}")
        if not os.path.exists(model_path):
            raise FileNotFoundError(f"Model data file not found: {model_path}")
            
        return human_path, model_path
    
    def train_domain_classifier(self, domain: str, model: str) -> Tuple[object, Dict, float, np.ndarray]:
        """Train a classifier for a specific domain and model."""
        print(f"Training classifier for {domain} domain with {model}...")
        
        human_path, model_path = self.get_data_paths(domain, model)
        
        # Train the classifier
        best_estimator, best_params, best_score, cv_scores = train_classifier(
            human_path, model_path, save_intermid=False
        )
        
        print(f"  - Best AUROC: {best_score:.4f}")
        print(f"  - CV scores: {cv_scores}")
        print(f"  - Mean CV AUROC: {np.mean(cv_scores):.4f} (+/- {np.std(cv_scores) * 2:.4f})")
        
        return best_estimator, best_params, best_score, cv_scores
    
    def evaluate_cross_domain(self, classifier: object, train_domain: str, test_domain: str, model: str) -> float:
        """Evaluate a classifier trained on one domain and tested on another domain."""
        print(f"  Evaluating {train_domain} -> {test_domain}...")
        
        # Get test data
        human_path, model_path = self.get_data_paths(test_domain, model)
        
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
    
    def run_cross_domain_evaluation(self):
        """Run the complete cross-domain evaluation."""
        print("Starting cross-domain evaluation for Claude-Haiku models...")
        print("=" * 80)
        
        for model_idx, model in enumerate(self.models):
            model_name = self.model_names[model_idx]
            print(f"\n{'='*20} {model_name} {'='*20}")
            
            # Store results for this model
            self.results[model_name] = {
                'train_results': {},  # Training domain results (diagonal)
                'cross_domain_results': {}  # Cross-domain results
            }
            
            # Train classifiers for each domain
            classifiers = {}
            for domain in self.domains:
                print(f"\nTraining on {domain} domain:")
                classifier, params, score, cv_scores = self.train_domain_classifier(domain, model)
                classifiers[domain] = classifier
                
                # Store training results (diagonal)
                self.results[model_name]['train_results'][domain] = {
                    'mean_auroc': np.mean(cv_scores),
                    'std_auroc': np.std(cv_scores),
                    'cv_scores': cv_scores
                }
            
            # Cross-domain evaluation
            print(f"\nCross-domain evaluation:")
            cross_domain_matrix = np.zeros((len(self.domains), len(self.domains)))
            
            for i, train_domain in enumerate(self.domains):
                for j, test_domain in enumerate(self.domains):
                    if i == j:
                        # Diagonal: use training results
                        cross_domain_matrix[i, j] = self.results[model_name]['train_results'][train_domain]['mean_auroc']
                    else:
                        # Off-diagonal: cross-domain evaluation
                        auroc = self.evaluate_cross_domain(classifiers[train_domain], train_domain, test_domain, model)
                        cross_domain_matrix[i, j] = auroc
            
            self.results[model_name]['cross_domain_matrix'] = cross_domain_matrix
            
            # Print summary for this model
            self.print_model_summary(model_name)
    
    def print_model_summary(self, model_name: str):
        """Print a summary of results for a specific model."""
        print(f"\n{'='*20} {model_name} Summary {'='*20}")
        
        # Training domain results
        print("\nTraining Domain Results (5-fold CV):")
        print("-" * 50)
        for domain in self.domains:
            result = self.results[model_name]['train_results'][domain]
            print(f"{domain:12}: {result['mean_auroc']:.4f} (+/- {result['std_auroc'] * 2:.4f})")
        
        # Cross-domain matrix
        matrix = self.results[model_name]['cross_domain_matrix']
        print(f"\nCross-Domain Evaluation Matrix:")
        print("-" * 50)
        
        # Header
        print(f"{'Train\\Test':>12}", end="")
        for domain in self.domains:
            print(f"{domain:>10}", end="")
        print()
        
        # Matrix rows
        for i, train_domain in enumerate(self.domains):
            print(f"{train_domain:>12}", end="")
            for j, test_domain in enumerate(self.domains):
                value = matrix[i, j]
                print(f"{value:>10.4f}", end="")
            print()
    
    def plot_confusion_matrices(self, save_path: str):
        """Plot confusion matrices for all models."""
        n_models = len(self.model_names)
        
        # Calculate subplot layout
        if n_models <= 3:
            fig, axes = plt.subplots(1, n_models, figsize=(6*n_models, 6))
        elif n_models <= 6:
            fig, axes = plt.subplots(2, 3, figsize=(18, 12))
            axes = axes.flatten()
        else:
            # For more than 6 models, use a larger grid
            cols = min(4, n_models)
            rows = (n_models + cols - 1) // cols
            fig, axes = plt.subplots(rows, cols, figsize=(6*cols, 6*rows))
            axes = axes.flatten()
        
        # Ensure axes is always a list
        if n_models == 1:
            axes = [axes]
        
        for idx, model_name in enumerate(self.model_names):
            matrix = self.results[model_name]['cross_domain_matrix']
            
            # Create heatmap
            sns.heatmap(
                matrix, 
                annot=True, 
                fmt='.4f',
                xticklabels=self.domains,
                yticklabels=self.domains,
                cmap='RdYlBu_r',
                vmin=0.0,
                vmax=1.0,
                ax=axes[idx],
                cbar_kws={'label': 'AUROC Score'}
            )
            
            axes[idx].set_title(f'{model_name}\nCross-Domain AUROC Matrix')
            axes[idx].set_xlabel('Test Domain')
            axes[idx].set_ylabel('Train Domain')
            axes[idx].tick_params(axis='both', which='major', labelsize=10)
        
        # Hide unused subplots
        for idx in range(n_models, len(axes)):
            axes[idx].set_visible(False)
        
        plt.tight_layout()
        plt.savefig(save_path, dpi=300, bbox_inches='tight')
        print(f"\nConfusion matrices saved to: {save_path}")
        plt.show()
    
    def save_results(self, save_path: str):
        """Save all results to a pickle file."""
        with open(save_path, 'wb') as f:
            pickle.dump(self.results, f)
        print(f"\nResults saved to: {save_path}")
    
    def print_overall_summary(self):
        """Print overall summary across all models."""
        print(f"\n{'='*20} Overall Summary {'='*20}")
        
        for model_name in self.model_names:
            print(f"\n{model_name}:")
            print("-" * 30)
            
            # Average performance on training domains
            train_scores = [self.results[model_name]['train_results'][domain]['mean_auroc'] 
                           for domain in self.domains]
            print(f"Average training domain AUROC: {np.mean(train_scores):.4f} (+/- {np.std(train_scores):.4f})")
            
            # Average cross-domain performance (off-diagonal)
            matrix = self.results[model_name]['cross_domain_matrix']
            off_diagonal = []
            for i in range(len(self.domains)):
                for j in range(len(self.domains)):
                    if i != j:
                        off_diagonal.append(matrix[i, j])
            
            print(f"Average cross-domain AUROC: {np.mean(off_diagonal):.4f} (+/- {np.std(off_diagonal):.4f})")
            
            # Domain transferability (average performance when each domain is used as target)
            for domain in self.domains:
                domain_idx = self.domains.index(domain)
                target_scores = matrix[:, domain_idx]  # All models tested on this domain
                print(f"  {domain} as target: {np.mean(target_scores):.4f} (+/- {np.std(target_scores):.4f})")


def exp_claude_haiku():
    """Run Claude-Haiku cross-domain evaluation experiment."""
    # Claude-Haiku specific configuration
    data_dir = "../data/Claude/Claude-Haiku"
    domains = ["pubmed", "peerread", "harmful", "xsum", "writing"]
    models = [
        "claude-3-opus-20240229",
        "claude-3-haiku-20240307", 
        "claude-3-5-haiku-20241022"
    ]
    model_names = ["Claude-3-Opus", "Claude-3-Haiku", "Claude-3.5-Haiku"]
    
    # Initialize evaluator
    evaluator = CrossDomainEvaluator(data_dir, domains, models, model_names)
    
    # Run the evaluation
    evaluator.run_cross_domain_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-haiku_confusion_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-haiku_cross_domain_results.pkl")
    
    print("\nClaude-Haiku cross-domain evaluation completed!")
    
    return evaluator


def exp_claude_sonnet():
    """Run Claude-Sonnet cross-domain evaluation experiment."""
    # Claude-Sonnet specific configuration
    data_dir = "../data/Claude/Claude-Sonnet"
    domains = ["pubmed", "peerread", "harmful", "xsum", "writing"]
    models = [
        "claude-3-sonnet-20240229",
        "claude-3-5-sonnet-20240620",
        "claude-3-5-sonnet-20241022"
    ]
    model_names = ["Claude-3-Sonnet", "Claude-3.5-Sonnet (2024-06-20)", "Claude-3.5-Sonnet (2024-10-22)"]
    
    # Initialize evaluator
    evaluator = CrossDomainEvaluator(data_dir, domains, models, model_names)
    
    # Run the evaluation
    evaluator.run_cross_domain_evaluation()
    
    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("claude-sonnet_confusion_matrices.pdf")
    
    # Save results
    # evaluator.save_results("claude-sonnet_cross_domain_results.pkl")
    
    print("\nClaude-Sonnet cross-domain evaluation completed!")
    
    return evaluator


def exp_gpt4():
    """Run GPT-4 cross-domain evaluation experiment."""
    # GPT-4 specific configuration
    data_dir = "../data/GPT4"
    domains = ["pubmed", "peerread", "harmful", "xsum", "writing"]
    models = ["gpt-4", "gpt-4-1106-preview", "gpt-4-0125-preview", "gpt-4-turbo-2024-04-09"]
    model_names = ["GPT-4 20230613", "GPT-4 20231106", "GPT-4 20240125", "GPT-4-Turbo 20240409"]
    
    # Initialize evaluator
    evaluator = CrossDomainEvaluator(data_dir, domains, models, model_names)
    
    # Run the evaluation
    evaluator.run_cross_domain_evaluation()

    # Print overall summary
    evaluator.print_overall_summary()
    
    # Plot confusion matrices
    evaluator.plot_confusion_matrices("gpt4_confusion_matrices.pdf")
    
    # Save results
    # evaluator.save_results("gpt4_cross_domain_results.pkl")
    
    print("\nGPT-4 cross-domain evaluation completed!")
    
    return evaluator


def main():
    """Main function to run the cross-domain evaluation."""
    # Run Claude-Haiku experiment
    # evaluator = exp_claude_haiku()

    # Run Claude-Sonnet experiment
    # evaluator = exp_claude_sonnet()

    # Run GPT-4 experiment
    evaluator = exp_gpt4()

if __name__ == "__main__":
    main()
