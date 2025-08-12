#!/usr/bin/env python3
"""
Script to plot Claude model performance results from claude-all_sup_cls_results.txt
Creates 4 subplots: 2 metrics (Acc, AUROC) x 2 model families (Haiku, Sonnet)
"""

import matplotlib.pyplot as plt
import numpy as np
import re
from collections import defaultdict
import matplotlib.patches as mpatches

def parse_results_file(filename):
    """Parse the results file and extract performance data"""
    data = defaultdict(lambda: defaultdict(dict))
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        # Look for section headers like "--- Claude-Haiku XSum ---"
        if line.startswith('---') and 'Claude-' in line:
            # Extract family and domain
            header_match = re.search(r'Claude-(Haiku|Sonnet)\s+(.+?)\s*---', line)
            if not header_match:
                i += 1
                continue
                
            family = header_match.group(1)
            domain = header_match.group(2).strip()
            
            i += 1
            # Parse models in this section
            while i < len(lines):
                line = lines[i].strip()
                
                # Check if we've reached the end of this section
                if line.startswith('---') or line == 'Done':
                    break
                
                # Look for model names
                if line.startswith('Claude-'):
                    model = line
                    
                    # Get the next two lines for Acc and AUROC
                    if i + 2 < len(lines):
                        acc_line = lines[i + 1].strip()
                        auroc_line = lines[i + 2].strip()
                        
                        acc_match = re.search(r'Mean Acc: ([\d.]+)', acc_line)
                        auroc_match = re.search(r'Mean AUROC: ([\d.]+)', auroc_line)
                        
                        if acc_match and auroc_match:
                            data[family][domain][model] = {
                                'Acc': float(acc_match.group(1)),
                                'AUROC': float(auroc_match.group(1))
                            }
                        
                        i += 3  # Skip the acc and auroc lines
                    else:
                        i += 1
                else:
                    i += 1
        else:
            i += 1
    
    return data

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
        'XSum': '#1f77b4',      # blue
        'Writing': '#ff7f0e',   # orange  
        'PubMed': '#2ca02c',    # green
        'PeerRead': '#d62728',  # red
        'Harmful': '#9467bd'    # purple
    }

def create_plots(data):
    """Create the main plotting function"""
    haiku_order, sonnet_order = get_model_order()
    domain_colors = get_domain_colors()
    
    # Create figure with 2x2 subplots
    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle('Claude Model Performance Across Domains and Time', fontsize=16, fontweight='bold')
    
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
                               label=domain, 
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
        fig.suptitle(f'Claude Model {metric} Performance Across Domains and Time', 
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
                               label=domain, 
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
        filename = f'claude_{metric.lower()}_timeline.pdf'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        print(f"Saved {filename}")
        
        plt.close()

def main():
    """Main function"""
    # Parse the results file
    print("解析结果文件...")
    data = parse_results_file('claude-all_sup_cls_results.txt')
    
    # Print parsed data structure for verification
    print("已解析的数据结构:")
    for family in data:
        print(f"\n{family}:")
        for domain in data[family]:
            print(f"  {domain}: {list(data[family][domain].keys())}")
    
    # Create and save combined plot
    print("\n创建组合图表...")
    fig = create_plots(data)
    plt.savefig('claude_combined_timeline.pdf', dpi=300, bbox_inches='tight')
    print("已保存 claude_combined_timeline.pdf")
    plt.close()
    
    # Create and save separate plots
    print("\n创建分离的图表...")
    save_separate_plots(data)
    
    print("\n所有图表已创建完成!")

if __name__ == "__main__":
    main()
