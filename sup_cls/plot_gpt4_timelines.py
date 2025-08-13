#!/usr/bin/env python3
"""
Script to plot GPT4 model performance results from gpt4-all_sup_cls_results.txt
Creates plots showing GPT4 evolution across domains and time
"""

import matplotlib.pyplot as plt
import numpy as np
import re
from collections import defaultdict
import matplotlib.patches as mpatches

def parse_results_file(filename):
    """Parse the results file and extract performance data"""
    data = defaultdict(dict)
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        # Look for section headers like "--- GPT4 XSum ---"
        if line.startswith('---') and 'GPT4' in line:
            # Extract domain
            header_match = re.search(r'GPT4\s+(.+?)\s*---', line)
            if not header_match:
                i += 1
                continue
                
            domain = header_match.group(1).strip()
            
            i += 1
            # Parse models in this section
            while i < len(lines):
                line = lines[i].strip()
                
                # Check if we've reached the end of this section
                if line.startswith('---') or line == 'Done':
                    break
                
                # Look for model names
                if line.startswith('GPT-4'):
                    model = line
                    
                    # Get the next two lines for Acc and AUROC
                    if i + 2 < len(lines):
                        acc_line = lines[i + 1].strip()
                        auroc_line = lines[i + 2].strip()
                        
                        acc_match = re.search(r'Mean Acc: ([\d.]+)', acc_line)
                        auroc_match = re.search(r'Mean AUROC: ([\d.]+)', auroc_line)
                        
                        if acc_match and auroc_match:
                            if domain not in data:
                                data[domain] = {}
                            data[domain][model] = {
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
    """Define the chronological order of GPT4 models based on GPT4_evolving_timeline.txt"""
    # Based on the timeline file:
    # GPT-4 2023-06-13 gpt-4-0613
    # GPT-4 2023-11-06 gpt-4-1106-preview  
    # GPT-4 2024-01-25 gpt-4-0125-preview
    # GPT-4 2024-04-09 gpt-4-turbo-2024-04-09
    
    return [
        'GPT-4',
        'GPT-4-1106-preview',
        'GPT-4-0125-preview', 
        'GPT-4-Turbo (2024-04-09)'
    ]

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
    model_order = get_model_order()
    domain_colors = get_domain_colors()
    
    # Create figure with 1x2 subplots (Acc and AUROC)
    fig, axes = plt.subplots(1, 2, figsize=(16, 6))
    fig.suptitle('GPT-4 Model Performance Across Domains and Time', fontsize=16, fontweight='bold')
    
    metrics = ['Acc', 'AUROC']
    
    for metric_idx, metric in enumerate(metrics):
        ax = axes[metric_idx]
        
        # Plot each domain
        for domain in domain_colors.keys():
            if domain in data:
                x_vals = []
                y_vals = []
                
                for i, model in enumerate(model_order):
                    if model in data[domain]:
                        x_vals.append(i)
                        y_vals.append(data[domain][model][metric])
                
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
                if (domain in data and 
                    model in data[domain]):
                    model_scores.append(data[domain][model][metric])
            
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
        ax.set_title(f'GPT-4 - {metric}', fontsize=14, fontweight='bold')
        ax.set_xlabel('Model Version', fontsize=12)
        ax.set_ylabel(metric, fontsize=12)
        ax.grid(True, alpha=0.3)
        
        # Set x-axis labels
        ax.set_xticks(range(len(model_order)))
        # Simplify model names for x-axis
        simplified_names = []
        for model in model_order:
            if model == 'GPT-4':
                simplified_names.append('GPT-4\n(2023-06-13)')
            elif model == 'GPT-4-1106-preview':
                simplified_names.append('GPT-4\n(2023-11-06)')
            elif model == 'GPT-4-0125-preview':
                simplified_names.append('GPT-4\n(2024-01-25)')
            elif model == 'GPT-4-Turbo (2024-04-09)':
                simplified_names.append('GPT-4-Turbo\n(2024-04-09)')
            else:
                simplified_names.append(model)
        
        ax.set_xticklabels(simplified_names, rotation=0, ha='center')
        
        # Set y-axis limits for better visualization
        if metric == 'Acc':
            ax.set_ylim(0.5, 0.9)
        else:  # AUROC
            ax.set_ylim(0.5, 1.0)
        
        # Add legend to the right subplot
        if metric_idx == 1:
            ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    
    plt.tight_layout()
    return fig

def save_separate_plots(data):
    """Save Acc and AUROC in separate files"""
    model_order = get_model_order()
    domain_colors = get_domain_colors()
    
    for metric in ['Acc', 'AUROC']:
        fig, ax = plt.subplots(1, 1, figsize=(12, 6))
        fig.suptitle(f'GPT-4 Model {metric} Performance Across Domains and Time', 
                    fontsize=16, fontweight='bold')
        
        # Plot each domain
        for domain in domain_colors.keys():
            if domain in data:
                x_vals = []
                y_vals = []
                
                for i, model in enumerate(model_order):
                    if model in data[domain]:
                        x_vals.append(i)
                        y_vals.append(data[domain][model][metric])
                
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
                if (domain in data and 
                    model in data[domain]):
                    model_scores.append(data[domain][model][metric])
            
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
        ax.set_title(f'GPT-4 - {metric}', fontsize=14, fontweight='bold')
        ax.set_xlabel('Model Version', fontsize=12)
        ax.set_ylabel(metric, fontsize=12)
        ax.grid(True, alpha=0.3)
        
        # Set x-axis labels
        ax.set_xticks(range(len(model_order)))
        # Simplify model names for x-axis
        simplified_names = []
        for model in model_order:
            if model == 'GPT-4':
                simplified_names.append('GPT-4\n(2023-06-13)')
            elif model == 'GPT-4-1106-preview':
                simplified_names.append('GPT-4\n(2023-11-06)')
            elif model == 'GPT-4-0125-preview':
                simplified_names.append('GPT-4\n(2024-01-25)')
            elif model == 'GPT-4-Turbo (2024-04-09)':
                simplified_names.append('GPT-4-Turbo\n(2024-04-09)')
            else:
                simplified_names.append(model)
        
        ax.set_xticklabels(simplified_names, rotation=0, ha='center')
        
        # Set y-axis limits for better visualization
        if metric == 'Acc':
            ax.set_ylim(0.5, 0.9)
        else:  # AUROC
            ax.set_ylim(0.5, 1.0)
        
        # Add legend
        ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
        
        plt.tight_layout()
        
        # Save the plot
        filename = f'gpt4_{metric.lower()}_timeline.pdf'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        print(f"Saved {filename}")
        
        plt.close()

def main():
    """Main function"""
    # Parse the results file
    print("解析GPT4结果文件...")
    data = parse_results_file('gpt4-all_sup_cls_results.txt')
    
    # Print parsed data structure for verification
    print("已解析的数据结构:")
    for domain in data:
        print(f"{domain}: {list(data[domain].keys())}")
    
    # Create and save combined plot
    print("\n创建组合图表...")
    fig = create_plots(data)
    plt.savefig('gpt4_combined_timeline.pdf', dpi=300, bbox_inches='tight')
    print("已保存 gpt4_combined_timeline.pdf")
    plt.close()
    
    # Create and save separate plots
    print("\n创建分离的图表...")
    save_separate_plots(data)
    
    print("\n所有GPT4图表已创建完成!")

if __name__ == "__main__":
    main()
