#!/usr/bin/env python3
"""
Script to plot Claude model performance results from outputs/domain_experiments/*/cv_results.json
Creates 4 subplots: 2 metrics (Acc, AUROC) x 2 model families (Haiku, Sonnet)
"""

import matplotlib.pyplot as plt
import numpy as np
import json
import os
from collections import defaultdict
import matplotlib.patches as mpatches

def parse_cv_results():
    """Parse cv_results.json files from domain experiments"""
    data = defaultdict(lambda: defaultdict(dict))
    base_dir = "outputs/domain_experiments"
    
    if not os.path.exists(base_dir):
        print(f"Directory {base_dir} not found!")
        return data
    
    # Define model families and their patterns
    model_families = {
        'Haiku': [
            'claude-3-opus-20240229',
            'claude-3-haiku-20240307', 
            'claude-3-5-haiku-20241022'
        ],
        'Sonnet': [
            'claude-3-sonnet-20240229',
            'claude-3-5-sonnet-20240620',
            'claude-3-5-sonnet-20241022'
        ]
    }
    
    domains = ['harmful', 'writing', 'xsum', 'peerread', 'pubmed']
    
    for family, model_ids in model_families.items():
        for model_id in model_ids:
            for domain in domains:
                # Construct directory name
                exp_dir = f"{domain}_{model_id}"
                cv_results_path = os.path.join(base_dir, exp_dir, 'cv_results.json')
                
                if os.path.exists(cv_results_path):
                    try:
                        with open(cv_results_path, 'r') as f:
                            cv_results = json.load(f)
                        
                        # Extract metrics
                        mean_accuracy = cv_results.get('mean_accuracy', None)
                        mean_auc = cv_results.get('mean_auc', None)
                        
                        if mean_accuracy is not None and mean_auc is not None:
                            # Map model_id to display name
                            display_name = get_model_display_name(model_id)
                            data[family][domain][display_name] = {
                                'Acc': mean_accuracy,
                                'AUROC': mean_auc
                            }
                            print(f"Loaded: {family} - {domain} - {display_name}: Acc={mean_accuracy:.4f}, AUROC={mean_auc:.4f}")
                        else:
                            print(f"Warning: Missing metrics in {cv_results_path}")
                            
                    except Exception as e:
                        print(f"Error reading {cv_results_path}: {e}")
                else:
                    print(f"File not found: {cv_results_path}")
    
    return data

def get_model_display_name(model_id):
    """Convert model_id to display name"""
    mapping = {
        'claude-3-opus-20240229': 'Claude-3-Opus',
        'claude-3-haiku-20240307': 'Claude-3-Haiku',
        'claude-3-5-haiku-20241022': 'Claude-3.5-Haiku',
        'claude-3-sonnet-20240229': 'Claude-3-Sonnet',
        'claude-3-5-sonnet-20240620': 'Claude-3.5-Sonnet (2024-06-20)',
        'claude-3-5-sonnet-20241022': 'Claude-3.5-Sonnet (2024-10-22)'
    }
    return mapping.get(model_id, model_id)

def get_model_order():
    """Define the chronological order of models for each family"""
    haiku_order = [
        'Claude-3-Opus',
        'Claude-3-Haiku', 
        'Claude-3.5-Haiku'
    ]
    
    sonnet_order = [
        'Claude-3-Sonnet',
        'Claude-3.5-Sonnet (2024-06-20)',
        'Claude-3.5-Sonnet (2024-10-22)'
    ]
    
    return haiku_order, sonnet_order

def get_domain_colors():
    """Define colors for different domains"""
    return {
        'xsum': '#1f77b4',      # blue
        'writing': '#ff7f0e',   # orange  
        'pubmed': '#2ca02c',    # green
        'peerread': '#d62728',  # red
        'harmful': '#9467bd'    # purple
    }

def create_plots(data):
    """Create the main plotting function"""
    haiku_order, sonnet_order = get_model_order()
    domain_colors = get_domain_colors()
    
    # Create figure with 2x2 subplots
    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle('Claude Model Performance Across Domains and Time (RNN Classifiers)', fontsize=16, fontweight='bold')
    
    metrics = ['Acc', 'AUROC']
    families = ['Haiku', 'Sonnet']
    orders = [haiku_order, sonnet_order]
    
    for metric_idx, metric in enumerate(metrics):
        for family_idx, (family, model_order) in enumerate(zip(families, orders)):
            ax = axes[metric_idx, family_idx]
            
            # Plot each domain
            for domain in domain_colors.keys():
                if domain in data[family]:
                    x_vals = []
                    y_vals = []
                    
                    for i, model in enumerate(model_order):
                        if model in data[family][domain]:
                            x_vals.append(i)
                            y_vals.append(data[family][domain][model][metric])
                    
                    if x_vals and y_vals:
                        ax.plot(x_vals, y_vals, 'o-', 
                               color=domain_colors[domain], 
                               label=domain.capitalize(), 
                               linewidth=2, 
                               markersize=6)
            
            # Calculate and plot average across domains
            avg_vals = []
            for i, model in enumerate(model_order):
                model_scores = []
                for domain in domain_colors.keys():
                    if (domain in data[family] and 
                        model in data[family][domain]):
                        model_scores.append(data[family][domain][model][metric])
                
                if model_scores:
                    avg_vals.append(np.mean(model_scores))
                else:
                    avg_vals.append(None)
            
            # Plot average line
            valid_x = []
            valid_avg = []
            for i, avg in enumerate(avg_vals):
                if avg is not None:
                    valid_x.append(i)
                    valid_avg.append(avg)
            
            if valid_x and valid_avg:
                ax.plot(valid_x, valid_avg, '--o', 
                       color='black', 
                       linewidth=2, 
                       alpha=0.8,
                       markerfacecolor='white',
                       markeredgecolor='black',
                       markeredgewidth=1.5,
                       markersize=6,
                       label='Average')
            
            # Formatting
            ax.set_title(f'{family} - {metric}', fontsize=14, fontweight='bold')
            ax.set_xlabel('Model Generation', fontsize=12)
            ax.set_ylabel(metric, fontsize=12)
            ax.grid(True, alpha=0.3)
            
            # Set x-axis labels
            ax.set_xticks(range(len(model_order)))
            # Simplify model names for x-axis
            simplified_names = []
            for model in model_order:
                if 'Opus' in model:
                    simplified_names.append('3-Opus')
                elif '3-Haiku' in model:
                    simplified_names.append('3-Haiku')
                elif '3.5-Haiku' in model:
                    simplified_names.append('3.5-Haiku')
                elif '3-Sonnet' in model:
                    simplified_names.append('3-Sonnet')
                elif '3.5-Sonnet (2024-06-20)' in model:
                    simplified_names.append('3.5-Sonnet\n(Jun-20)')
                elif '3.5-Sonnet (2024-10-22)' in model:
                    simplified_names.append('3.5-Sonnet\n(Oct-22)')
                else:
                    simplified_names.append(model)
            
            ax.set_xticklabels(simplified_names, rotation=45, ha='right')
            
            # Set y-axis limits for better visualization
            if metric == 'Acc':
                ax.set_ylim(0.5, 1.0)
            else:  # AUROC
                ax.set_ylim(0.6, 1.0)
            
            # Add legend only to the top-right subplot
            if metric_idx == 0 and family_idx == 1:
                ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    
    plt.tight_layout()
    return fig

def save_separate_plots(data):
    """Save Acc and AUROC in separate files"""
    haiku_order, sonnet_order = get_model_order()
    domain_colors = get_domain_colors()
    
    for metric in ['Acc', 'AUROC']:
        fig, axes = plt.subplots(1, 2, figsize=(16, 6))
        fig.suptitle(f'Claude Model {metric} Performance Across Domains and Time (RNN Classifiers)', 
                    fontsize=16, fontweight='bold')
        
        families = ['Haiku', 'Sonnet']
        orders = [haiku_order, sonnet_order]
        
        for family_idx, (family, model_order) in enumerate(zip(families, orders)):
            ax = axes[family_idx]
            
            # Plot each domain
            for domain in domain_colors.keys():
                if domain in data[family]:
                    x_vals = []
                    y_vals = []
                    
                    for i, model in enumerate(model_order):
                        if model in data[family][domain]:
                            x_vals.append(i)
                            y_vals.append(data[family][domain][model][metric])
                    
                    if x_vals and y_vals:
                        ax.plot(x_vals, y_vals, 'o-', 
                               color=domain_colors[domain], 
                               label=domain.capitalize(), 
                               linewidth=2, 
                               markersize=6)
            
            # Calculate and plot average across domains
            avg_vals = []
            for i, model in enumerate(model_order):
                model_scores = []
                for domain in domain_colors.keys():
                    if (domain in data[family] and 
                        model in data[family][domain]):
                        model_scores.append(data[family][domain][model][metric])
                
                if model_scores:
                    avg_vals.append(np.mean(model_scores))
                else:
                    avg_vals.append(None)
            
            # Plot average line
            valid_x = []
            valid_avg = []
            for i, avg in enumerate(avg_vals):
                if avg is not None:
                    valid_x.append(i)
                    valid_avg.append(avg)
            
            if valid_x and valid_avg:
                ax.plot(valid_x, valid_avg, '--o', 
                       color='black', 
                       linewidth=2, 
                       alpha=0.8,
                       markerfacecolor='white',
                       markeredgecolor='black',
                       markeredgewidth=1.5,
                       markersize=6,
                       label='Average')
            
            # Formatting
            ax.set_title(f'{family} - {metric}', fontsize=14, fontweight='bold')
            ax.set_xlabel('Model Generation', fontsize=12)
            ax.set_ylabel(metric, fontsize=12)
            ax.grid(True, alpha=0.3)
            
            # Set x-axis labels
            ax.set_xticks(range(len(model_order)))
            # Simplify model names for x-axis
            simplified_names = []
            for model in model_order:
                if 'Opus' in model:
                    simplified_names.append('3-Opus')
                elif '3-Haiku' in model:
                    simplified_names.append('3-Haiku')
                elif '3.5-Haiku' in model:
                    simplified_names.append('3.5-Haiku')
                elif '3-Sonnet' in model:
                    simplified_names.append('3-Sonnet')
                elif '3.5-Sonnet (2024-06-20)' in model:
                    simplified_names.append('3.5-Sonnet\n(Jun-20)')
                elif '3.5-Sonnet (2024-10-22)' in model:
                    simplified_names.append('3.5-Sonnet\n(Oct-22)')
                else:
                    simplified_names.append(model)
            
            ax.set_xticklabels(simplified_names, rotation=45, ha='right')
            
            # Set y-axis limits for better visualization
            if metric == 'Acc':
                ax.set_ylim(0.5, 1.0)
            else:  # AUROC
                ax.set_ylim(0.6, 1.0)
            
            # Add legend to the right subplot
            if family_idx == 1:
                ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
        
        plt.tight_layout()
        
        # Save the plot
        filename = f'rnn_claude_{metric.lower()}_timeline.pdf'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        print(f"Saved {filename}")
        
        plt.close()

def print_summary_statistics(data):
    """Print summary statistics for the data"""
    print("\n" + "="*60)
    print("SUMMARY STATISTICS")
    print("="*60)
    
    haiku_order, sonnet_order = get_model_order()
    families = ['Haiku', 'Sonnet']
    orders = [haiku_order, sonnet_order]
    
    for family, model_order in zip(families, orders):
        print(f"\n{family} Family:")
        print("-" * 30)
        
        for metric in ['Acc', 'AUROC']:
            print(f"\n{metric} Performance:")
            for model in model_order:
                scores = []
                for domain in ['harmful', 'writing', 'xsum', 'peerread', 'pubmed']:
                    if (domain in data[family] and 
                        model in data[family][domain]):
                        scores.append(data[family][domain][model][metric])
                
                if scores:
                    mean_score = np.mean(scores)
                    std_score = np.std(scores)
                    print(f"  {model}: {mean_score:.4f} (±{std_score:.4f})")
                else:
                    print(f"  {model}: No data available")

def main():
    """Main function"""
    # Parse the cv_results.json files
    print("解析cv_results.json文件...")
    data = parse_cv_results()
    
    # Print parsed data structure for verification
    print("\n已解析的数据结构:")
    for family in data:
        print(f"\n{family}:")
        for domain in data[family]:
            print(f"  {domain}: {list(data[family][domain].keys())}")
    
    # Print summary statistics
    print_summary_statistics(data)
    
    # Create and save combined plot
    print("\n创建组合图表...")
    fig = create_plots(data)
    plt.savefig('rnn_claude_combined_timeline.pdf', dpi=300, bbox_inches='tight')
    print("已保存 rnn_claude_combined_timeline.pdf")
    plt.close()
    
    # Create and save separate plots
    print("\n创建分离的图表...")
    save_separate_plots(data)
    
    print("\n所有图表已创建完成!")

if __name__ == "__main__":
    main()
