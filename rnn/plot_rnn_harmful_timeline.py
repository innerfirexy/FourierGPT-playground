#!/usr/bin/env python3
"""
Script to plot RNN model performance results for Harmful domain
Creates timeline plots showing accuracy, F1, and AUC across different Claude model versions
Fixed version with English labels and actual SVM results
"""

import matplotlib.pyplot as plt
import numpy as np
import json
import matplotlib.patches as mpatches

def load_results(filename):
    """Load the results from JSON file"""
    with open(filename, 'r') as f:
        data = json.load(f)
    return data

def get_model_info():
    """Define model information and chronological order"""
    model_info = {
        'opus_20240229': {
            'name': 'Claude-3-Opus\n(2024-02-29)',
            'short_name': '3-Opus',
            'order': 0,
            'color': '#1f77b4'  # blue
        },
        'haiku_20240307': {
            'name': 'Claude-3-Haiku\n(2024-03-07)', 
            'short_name': '3-Haiku',
            'order': 1,
            'color': '#ff7f0e'  # orange
        },
        'haiku35_20241022': {
            'name': 'Claude-3.5-Haiku\n(2024-10-22)',
            'short_name': '3.5-Haiku', 
            'order': 2,
            'color': '#2ca02c'  # green
        }
    }
    return model_info

def get_svm_results():
    """Extract actual SVM+FFT results for Harmful domain from sup_cls results"""
    # From claude-all_sup_cls_results.txt, Harmful section:
    # Claude-3-Opus: Acc=0.6167, AUROC=0.6878
    # Claude-3-Haiku: Acc=0.6467, AUROC=0.7116  
    # Claude-3.5-Haiku: Acc=0.7067, AUROC=0.7824
    
    svm_results = {
        'opus_20240229': {'accuracy': 0.6167, 'auc': 0.6878},
        'haiku_20240307': {'accuracy': 0.6467, 'auc': 0.7116},
        'haiku35_20241022': {'accuracy': 0.7067, 'auc': 0.7824}
    }
    return svm_results

def create_timeline_plot(data):
    """Create the main timeline plot with all metrics"""
    model_info = get_model_info()
    
    # Sort data by chronological order
    sorted_data = sorted(data, key=lambda x: model_info[x['model']]['order'])
    
    # Extract data for plotting
    model_names = [model_info[item['model']]['short_name'] for item in sorted_data]
    colors = [model_info[item['model']]['color'] for item in sorted_data]
    
    accuracy = [item['accuracy'] for item in sorted_data]
    accuracy_std = [item['std_accuracy'] for item in sorted_data]
    
    f1 = [item['f1'] for item in sorted_data]
    f1_std = [item['std_f1'] for item in sorted_data]
    
    auc = [item['auc'] for item in sorted_data]
    auc_std = [item['std_auc'] for item in sorted_data]
    
    # Create figure with subplots
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))
    fig.suptitle('RNN Model Performance on Harmful Domain', fontsize=16, fontweight='bold')
    
    metrics = [
        ('Accuracy', accuracy, accuracy_std),
        ('F1 Score', f1, f1_std), 
        ('AUC', auc, auc_std)
    ]
    
    x_positions = range(len(model_names))
    
    for idx, (metric_name, values, stds) in enumerate(metrics):
        ax = axes[idx]
        
        # Plot bars with error bars
        bars = ax.bar(x_positions, values, color=colors, alpha=0.7, 
                     capsize=5, width=0.6)
        ax.errorbar(x_positions, values, yerr=stds, fmt='none', 
                   color='black', capsize=5, capthick=1)
        
        # Plot line connecting points
        ax.plot(x_positions, values, 'o-', color='black', 
               linewidth=2, markersize=6, alpha=0.8)
        
        # Add value labels on bars
        for i, (val, std) in enumerate(zip(values, stds)):
            ax.text(i, val + std + 0.01, f'{val:.3f}±{std:.3f}', 
                   ha='center', va='bottom', fontsize=10, fontweight='bold')
        
        # Formatting
        ax.set_title(f'{metric_name}', fontsize=14, fontweight='bold')
        ax.set_xlabel('Claude Model Version', fontsize=12)
        ax.set_ylabel(metric_name, fontsize=12)
        ax.grid(True, alpha=0.3, axis='y')
        
        # Set x-axis
        ax.set_xticks(x_positions)
        ax.set_xticklabels(model_names, rotation=0, ha='center')
        
        # Set y-axis limits for better visualization
        if metric_name == 'Accuracy':
            ax.set_ylim(0.6, 0.9)
        elif metric_name == 'F1 Score':
            ax.set_ylim(0.6, 0.9)
        else:  # AUC
            ax.set_ylim(0.7, 0.9)
    
    plt.tight_layout()
    return fig

def create_combined_plot(data):
    """Create a single plot with all metrics on the same chart"""
    model_info = get_model_info()
    
    # Sort data by chronological order
    sorted_data = sorted(data, key=lambda x: model_info[x['model']]['order'])
    
    # Extract data
    model_names = [model_info[item['model']]['short_name'] for item in sorted_data]
    
    accuracy = [item['accuracy'] for item in sorted_data]
    f1 = [item['f1'] for item in sorted_data]
    auc = [item['auc'] for item in sorted_data]
    
    x_positions = range(len(model_names))
    
    # Create figure
    fig, ax = plt.subplots(figsize=(12, 8))
    
    # Plot lines for each metric
    ax.plot(x_positions, accuracy, 'o-', color='#1f77b4', linewidth=3, 
           markersize=8, label='Accuracy', markerfacecolor='white', 
           markeredgewidth=2)
    ax.plot(x_positions, f1, 's-', color='#ff7f0e', linewidth=3, 
           markersize=8, label='F1 Score', markerfacecolor='white',
           markeredgewidth=2)
    ax.plot(x_positions, auc, '^-', color='#2ca02c', linewidth=3, 
           markersize=8, label='AUC', markerfacecolor='white',
           markeredgewidth=2)
    
    # Add value annotations
    for i, (acc, f1_val, auc_val) in enumerate(zip(accuracy, f1, auc)):
        ax.annotate(f'{acc:.3f}', (i, acc), textcoords="offset points", 
                   xytext=(0,10), ha='center', fontsize=10, color='#1f77b4', fontweight='bold')
        ax.annotate(f'{f1_val:.3f}', (i, f1_val), textcoords="offset points", 
                   xytext=(0,10), ha='center', fontsize=10, color='#ff7f0e', fontweight='bold')
        ax.annotate(f'{auc_val:.3f}', (i, auc_val), textcoords="offset points", 
                   xytext=(0,10), ha='center', fontsize=10, color='#2ca02c', fontweight='bold')
    
    # Formatting
    ax.set_title('RNN Model Performance Comparison on Harmful Domain', fontsize=16, fontweight='bold')
    ax.set_xlabel('Claude Model Version', fontsize=14)
    ax.set_ylabel('Performance Metric', fontsize=14)
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=12, loc='upper right')
    
    # Set x-axis
    ax.set_xticks(x_positions)
    ax.set_xticklabels(model_names, fontsize=12)
    
    # Set y-axis limits
    ax.set_ylim(0.7, 0.85)
    
    plt.tight_layout()
    return fig

def create_comparison_with_svm(data):
    """Create comparison plot with actual SVM+FFT results"""
    svm_results = get_svm_results()
    model_info = get_model_info()
    sorted_data = sorted(data, key=lambda x: model_info[x['model']]['order'])
    
    model_names = [model_info[item['model']]['short_name'] for item in sorted_data]
    
    # RNN results
    rnn_accuracy = [item['accuracy'] for item in sorted_data]
    rnn_auc = [item['auc'] for item in sorted_data]
    
    # SVM results (in same order)
    svm_accuracy = [svm_results[item['model']]['accuracy'] for item in sorted_data]
    svm_auc = [svm_results[item['model']]['auc'] for item in sorted_data]
    
    # Create figure with subplots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 6))
    fig.suptitle('RNN vs SVM+FFT Performance Comparison - Harmful Domain', fontsize=16, fontweight='bold')
    
    x_positions = range(len(model_names))
    width = 0.35
    
    # Accuracy comparison
    bars1 = ax1.bar([x - width/2 for x in x_positions], rnn_accuracy, width, 
                   label='RNN', color='#1f77b4', alpha=0.7)
    bars2 = ax1.bar([x + width/2 for x in x_positions], svm_accuracy, width,
                   label='SVM+FFT', color='#ff7f0e', alpha=0.7)
    
    # Add value labels on bars
    for bar, val in zip(bars1, rnn_accuracy):
        ax1.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01, 
                f'{val:.3f}', ha='center', va='bottom', fontsize=10)
    for bar, val in zip(bars2, svm_accuracy):
        ax1.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01, 
                f'{val:.3f}', ha='center', va='bottom', fontsize=10)
    
    ax1.set_title('Accuracy Comparison', fontsize=14, fontweight='bold')
    ax1.set_xlabel('Claude Model Version', fontsize=12)
    ax1.set_ylabel('Accuracy', fontsize=12)
    ax1.set_xticks(x_positions)
    ax1.set_xticklabels(model_names)
    ax1.legend()
    ax1.grid(True, alpha=0.3, axis='y')
    ax1.set_ylim(0.5, 0.8)
    
    # AUC comparison  
    bars3 = ax2.bar([x - width/2 for x in x_positions], rnn_auc, width,
                   label='RNN', color='#1f77b4', alpha=0.7)
    bars4 = ax2.bar([x + width/2 for x in x_positions], svm_auc, width,
                   label='SVM+FFT', color='#ff7f0e', alpha=0.7)
    
    # Add value labels on bars
    for bar, val in zip(bars3, rnn_auc):
        ax2.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01, 
                f'{val:.3f}', ha='center', va='bottom', fontsize=10)
    for bar, val in zip(bars4, svm_auc):
        ax2.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.01, 
                f'{val:.3f}', ha='center', va='bottom', fontsize=10)
    
    ax2.set_title('AUC Comparison', fontsize=14, fontweight='bold')
    ax2.set_xlabel('Claude Model Version', fontsize=12)
    ax2.set_ylabel('AUC', fontsize=12)
    ax2.set_xticks(x_positions)
    ax2.set_xticklabels(model_names)
    ax2.legend()
    ax2.grid(True, alpha=0.3, axis='y')
    ax2.set_ylim(0.6, 0.9)
    
    plt.tight_layout()
    return fig

def print_comparison_summary(data):
    """Print a summary comparison between RNN and SVM+FFT results"""
    svm_results = get_svm_results()
    model_info = get_model_info()
    sorted_data = sorted(data, key=lambda x: model_info[x['model']]['order'])
    
    print("\n" + "="*60)
    print("PERFORMANCE COMPARISON SUMMARY - HARMFUL DOMAIN")
    print("="*60)
    print(f"{'Model':<20} {'Method':<10} {'Accuracy':<10} {'AUC':<10}")
    print("-" * 60)
    
    for item in sorted_data:
        model_key = item['model']
        model_name = model_info[model_key]['short_name']
        
        # RNN results
        rnn_acc = item['accuracy']
        rnn_auc = item['auc']
        
        # SVM results
        svm_acc = svm_results[model_key]['accuracy']
        svm_auc = svm_results[model_key]['auc']
        
        print(f"{model_name:<20} {'RNN':<10} {rnn_acc:<10.3f} {rnn_auc:<10.3f}")
        print(f"{'':<20} {'SVM+FFT':<10} {svm_acc:<10.3f} {svm_auc:<10.3f}")
        
        # Calculate improvement
        acc_diff = rnn_acc - svm_acc
        auc_diff = rnn_auc - svm_auc
        
        print(f"{'':<20} {'Diff':<10} {acc_diff:+.3f}{'':^4} {auc_diff:+.3f}")
        print("-" * 60)

def main():
    """Main function"""
    print("Loading RNN experiment results...")
    data = load_results('outputs/domain_experiments/harmful_summary.json')
    
    print("Parsed data:")
    for item in data:
        print(f"  {item['model']}: Acc={item['accuracy']:.3f}±{item['std_accuracy']:.3f}, "
              f"F1={item['f1']:.3f}±{item['std_f1']:.3f}, "
              f"AUC={item['auc']:.3f}±{item['std_auc']:.3f}")
    
    # Print comparison summary
    print_comparison_summary(data)
    
    # Create timeline plot
    print("\nCreating timeline plots...")
    fig1 = create_timeline_plot(data)
    plt.savefig('rnn_harmful_timeline_fixed.pdf', dpi=300, bbox_inches='tight')
    print("Saved rnn_harmful_timeline_fixed.pdf")
    plt.close()
    
    # Create combined plot
    print("\nCreating combined performance plot...")
    fig2 = create_combined_plot(data)
    plt.savefig('rnn_harmful_combined_fixed.pdf', dpi=300, bbox_inches='tight')
    print("Saved rnn_harmful_combined_fixed.pdf")
    plt.close()
    
    # Create comparison plot
    print("\nCreating RNN vs SVM+FFT comparison plot...")
    fig3 = create_comparison_with_svm(data)
    plt.savefig('rnn_vs_svm_harmful_fixed.pdf', dpi=300, bbox_inches='tight')
    print("Saved rnn_vs_svm_harmful_fixed.pdf")
    plt.close()
    
    print("\nAll plots created successfully!")
    print("\nGenerated files:")
    print("- rnn_harmful_timeline_fixed.pdf: Timeline comparison chart")
    print("- rnn_harmful_combined_fixed.pdf: Combined performance chart") 
    print("- rnn_vs_svm_harmful_fixed.pdf: RNN vs SVM+FFT comparison chart")

if __name__ == "__main__":
    main()
