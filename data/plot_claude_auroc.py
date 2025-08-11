#!/usr/bin/env python3
"""
Plot AUROC results for Claude-Haiku and Claude-Sonnet models over time.
"""

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from datetime import datetime
import re

# Define the data from claude_sup_cls_results.txt
data = {
    'Claude-Haiku': {
        'Claude-3-Opus': {
            'date': '2024-02-29',
            'xsum': 0.8909,
            'writing': 0.9322,
            'pubmed': 0.8989,
            'peerread': 0.8064,
            'harmful': 0.6878
        },
        'Claude-3-Haiku': {
            'date': '2024-03-07',
            'xsum': 0.8787,
            'writing': 0.9400,
            'pubmed': 0.9298,
            'peerread': 0.8531,
            'harmful': 0.7116
        },
        'Claude-3.5-Haiku': {
            'date': '2024-10-22',
            'xsum': 0.8696,
            'writing': 0.7824,
            'pubmed': 0.7976,
            'peerread': 0.9771,
            'harmful': 0.7824
        }
    },
    'Claude-Sonnet': {
        'Claude-3-Sonnet': {
            'date': '2024-02-29',
            'xsum': 0.7691,
            'writing': 0.8933,
            'pubmed': 0.9231,
            'peerread': 0.8320,
            'harmful': 0.7711
        },
        'Claude-3.5-Sonnet (2024-06-20)': {
            'date': '2024-06-20',
            'xsum': 0.7904,
            'writing': 0.8398,
            'pubmed': 0.8556,
            'peerread': 0.7418,
            'harmful': 0.6851
        },
        'Claude-3.5-Sonnet (2024-10-22)': {
            'date': '2024-10-22',
            'xsum': 0.6758,
            'writing': 0.6922,
            'pubmed': 0.8109,
            'peerread': 0.6673,
            'harmful': 0.6280
        }
    }
}

# Convert dates to datetime objects for proper ordering
for model_family in data.values():
    for model_name, model_data in model_family.items():
        model_data['datetime'] = datetime.strptime(model_data['date'], '%Y-%m-%d')

# Create the plot
plt.figure(figsize=(15, 10))

# Set up the color palette
colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd']
datasets = ['xsum', 'writing', 'pubmed', 'peerread', 'harmful']

# Plot for Claude-Haiku
plt.subplot(2, 1, 1)
haiku_data = data['Claude-Haiku']
haiku_models = sorted(haiku_data.keys(), key=lambda x: haiku_data[x]['datetime'])

for i, dataset in enumerate(datasets):
    values = [haiku_data[model][dataset] for model in haiku_models]
    plt.plot(range(len(haiku_models)), values, 'o-', label=dataset.capitalize(), 
             color=colors[i], linewidth=2, markersize=8)

# Add dashed line for average across domains
avg_values = [np.mean([haiku_data[model][dataset] for dataset in datasets]) for model in haiku_models]
plt.plot(range(len(haiku_models)), avg_values, 'k--', label='Average', 
         linewidth=3, markersize=10, marker='s')

plt.title('Claude-Haiku AUROC Over Time', fontsize=16, fontweight='bold')
plt.ylabel('AUROC', fontsize=12)
plt.xticks(range(len(haiku_models)), [model.replace('Claude-3.5-Haiku', 'Claude-3.5-Haiku\n(2024-10-22)') 
                                      for model in haiku_models], rotation=0)
plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
plt.grid(True, alpha=0.3)
plt.ylim(0.6, 1.0)

# Plot for Claude-Sonnet
plt.subplot(2, 1, 2)
sonnet_data = data['Claude-Sonnet']
sonnet_models = sorted(sonnet_data.keys(), key=lambda x: sonnet_data[x]['datetime'])

for i, dataset in enumerate(datasets):
    values = [sonnet_data[model][dataset] for model in sonnet_models]
    plt.plot(range(len(sonnet_models)), values, 'o-', label=dataset.capitalize(), 
             color=colors[i], linewidth=2, markersize=8)

# Add dashed line for average across domains
avg_values = [np.mean([sonnet_data[model][dataset] for dataset in datasets]) for model in sonnet_models]
plt.plot(range(len(sonnet_models)), avg_values, 'k--', label='Average', 
         linewidth=3, markersize=10, marker='s')

plt.title('Claude-Sonnet AUROC Over Time', fontsize=16, fontweight='bold')
plt.ylabel('AUROC', fontsize=12)
plt.xlabel('Model Version (Release Date)', fontsize=12)
plt.xticks(range(len(sonnet_models)), [model.replace('Claude-3.5-Sonnet (2024-06-20)', 'Claude-3.5-Sonnet\n(2024-06-20)')
                                       .replace('Claude-3.5-Sonnet (2024-10-22)', 'Claude-3.5-Sonnet\n(2024-10-22)') 
                                       for model in sonnet_models], rotation=0)
plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
plt.grid(True, alpha=0.3)
plt.ylim(0.6, 1.0)

plt.tight_layout()
plt.savefig('claude_auroc_timeline.png', dpi=300, bbox_inches='tight')
plt.savefig('claude_auroc_timeline.pdf', bbox_inches='tight')
plt.show()

# Print summary statistics
print("=== Claude-Haiku AUROC Summary ===")
for model in haiku_models:
    avg_auroc = np.mean([haiku_data[model][dataset] for dataset in datasets])
    print(f"{model}: {avg_auroc:.4f}")

print("\n=== Claude-Sonnet AUROC Summary ===")
for model in sonnet_models:
    avg_auroc = np.mean([sonnet_data[model][dataset] for dataset in datasets])
    print(f"{model}: {avg_auroc:.4f}")

# Create a table for better comparison
print("\n=== Detailed AUROC Comparison ===")
print(f"{'Model':<30} {'XSum':<8} {'Writing':<8} {'PubMed':<8} {'PeerRead':<8} {'Harmful':<8} {'Avg':<8}")
print("-" * 80)

for model in haiku_models:
    values = [haiku_data[model][dataset] for dataset in datasets]
    avg = np.mean(values)
    print(f"{model:<30} {values[0]:<8.4f} {values[1]:<8.4f} {values[2]:<8.4f} {values[3]:<8.4f} {values[4]:<8.4f} {avg:<8.4f}")

print("-" * 80)
for model in sonnet_models:
    values = [sonnet_data[model][dataset] for dataset in datasets]
    avg = np.mean(values)
    print(f"{model:<30} {values[0]:<8.4f} {values[1]:<8.4f} {values[2]:<8.4f} {values[3]:<8.4f} {values[4]:<8.4f} {avg:<8.4f}")
