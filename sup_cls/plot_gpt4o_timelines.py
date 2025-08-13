#!/usr/bin/env python3
"""
Script to plot GPT4o model performance results from gpt4o-all_sup_cls_results.txt
Creates plots showing GPT4o evolution across domains and time
Note: Excludes GPT-4o-mini results
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
        
        # Look for section headers like "--- GPT4o XSum ---"
        if line.startswith('---') and 'GPT4o' in line:
            # Extract domain
            header_match = re.search(r'GPT4o\s+(.+?)\s*---', line)
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
                
                # Look for model names, but skip GPT-4o-mini
                if line.startswith('GPT-4o') or line.startswith('ChatGPT-4o'):
                    # Skip GPT-4o-mini
                    if 'mini' in line:
                        # Skip this model and its results
                        i += 3  # Skip model name, acc line, auroc line
                        continue
                        
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
    """Define the chronological order of GPT4o models based on GPT4o_evolving_timeline.txt"""
    # Based on the timeline file, excluding mini:
    # GPT-4o 2024-05-13 gpt-4o-2024-05-13
    # GPT-4o 2024-08-06 gpt-4o-2024-08-06
    # GPT-4o 2024-11-20 gpt-4o-2024-11-20
    # GPT-4o Latest chatgpt-4o-latest
    
    return [
        'GPT-4o (2024-05-13)',
        'GPT-4o (2024-08-06)',
        'GPT-4o (2024-11-20)',
        'ChatGPT-4o-latest'
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
    fig.suptitle('GPT-4o Model Performance Across Domains and Time', fontsize=16, fontweight='bold')
    
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
        ax.set_title(f'GPT-4o - {metric}', fontsize=14, fontweight='bold')
        ax.set_xlabel('Model Version', fontsize=12)
        ax.set_ylabel(metric, fontsize=12)
        ax.grid(True, alpha=0.3)
        
        # Set x-axis labels
        ax.set_xticks(range(len(model_order)))
        # Simplify model names for x-axis
        simplified_names = []
        for model in model_order:
            if model == 'GPT-4o (2024-05-13)':
                simplified_names.append('GPT-4o\n(2024-05-13)')
            elif model == 'GPT-4o (2024-08-06)':
                simplified_names.append('GPT-4o\n(2024-08-06)')
            elif model == 'GPT-4o (2024-11-20)':
                simplified_names.append('GPT-4o\n(2024-11-20)')
            elif model == 'ChatGPT-4o-latest':
                simplified_names.append('ChatGPT-4o\n(latest)')
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
        fig.suptitle(f'GPT-4o Model {metric} Performance Across Domains and Time', 
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
        ax.set_title(f'GPT-4o - {metric}', fontsize=14, fontweight='bold')
        ax.set_xlabel('Model Version', fontsize=12)
        ax.set_ylabel(metric, fontsize=12)
        ax.grid(True, alpha=0.3)
        
        # Set x-axis labels
        ax.set_xticks(range(len(model_order)))
        # Simplify model names for x-axis
        simplified_names = []
        for model in model_order:
            if model == 'GPT-4o (2024-05-13)':
                simplified_names.append('GPT-4o\n(2024-05-13)')
            elif model == 'GPT-4o (2024-08-06)':
                simplified_names.append('GPT-4o\n(2024-08-06)')
            elif model == 'GPT-4o (2024-11-20)':
                simplified_names.append('GPT-4o\n(2024-11-20)')
            elif model == 'ChatGPT-4o-latest':
                simplified_names.append('ChatGPT-4o\n(latest)')
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
        filename = f'gpt4o_{metric.lower()}_timeline.pdf'
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        print(f"Saved {filename}")
        
        plt.close()

def main():
    """Main function"""
    # Parse the results file
    print("解析GPT4o结果文件...")
    data = parse_results_file('gpt4o-all_sup_cls_results.txt')
    
    # Print parsed data structure for verification
    print("已解析的数据结构 (排除GPT-4o-mini):")
    for domain in data:
        print(f"{domain}: {list(data[domain].keys())}")
    
    # Create and save combined plot
    print("\n创建组合图表...")
    fig = create_plots(data)
    plt.savefig('gpt4o_combined_timeline.pdf', dpi=300, bbox_inches='tight')
    print("已保存 gpt4o_combined_timeline.pdf")
    plt.close()
    
    # Create and save separate plots
    print("\n创建分离的图表...")
    save_separate_plots(data)
    
    print("\n所有GPT4o图表已创建完成!")

if __name__ == "__main__":
    main()
