#!/usr/bin/env python3
"""
测试训练好的分类器在新数据上的表现
"""

import sys
sys.path.append('..')
from run_fft import FFTProcessor
import numpy as np
import pandas as pd
import argparse
import os
import pickle
from typing import Union

from sklearn.metrics import roc_auc_score, accuracy_score, classification_report


# Preprocessing functions (copied from train_sup_cls.py)
def circular(input: list, n: int = None, include_self: bool = True):
    """
    >>> circular([1, 2, 3, 4, 5])
    >>> [[1, 2, 3, 4, 5], [2, 3, 4, 5, 1], [3, 4, 5, 1, 2], [4, 5, 1, 2, 3], [5, 1, 2, 3, 4]]
    """
    if n is None:
        n = len(input) - 1
    output = []
    if include_self:
        output.append(input)
    for i in range(n):
        out = input[i+1:] + input[:i+1]
        output.append(out)
    return output

def get_circular_mean(input_file: str, require_sid=True):
    """
    For each nll sequence, use circular to compute n spectra, then calculte its mean
    """
    fft_processor = FFTProcessor(method='fft', preprocess='logzs', value='norm', require_sid=False)
    nlls = fft_processor._read_data(data_file=input_file)
    freqs, powers, sids = [], [], []
    for i, nll in enumerate(nlls):
        nll_circle = circular(nll)
        data = fft_processor._preprocess(nll_circle)
        freq, power, _ = fft_processor._fft_batch(data, verbose=False)
        power_mean = np.mean(power, axis=0) # This is where the mean is calculated
        freqs.append(freq[0])
        powers.append(power_mean)
        sids.append(np.repeat(i, len(power_mean)))
    if require_sid:
        df = pd.DataFrame.from_dict({'freq': np.concatenate(freqs),
                                     'power': np.concatenate(powers),
                                     'sid': np.concatenate(sids)})
    else:
        df = pd.DataFrame.from_dict({'freq': np.concatenate(freqs),
                                'power': np.concatenate(powers)})
    return df

def get_features(spectrum_data: Union[str, pd.DataFrame], interp_len: int = 500):
    """
    Extract features by linear interpolation
    """
    if isinstance(spectrum_data, str):
        df = pd.read_csv(spectrum_data)
    else:
        df = spectrum_data

    # If `sid` column does not exist, create it
    if 'sid' not in df.columns:
        df['sdiff']  = df['freq'] < df['freq'].shift(1, fill_value=0)
        df['sdiff'] = df['sdiff'].astype(int)
        df['sid'] = df['sdiff'].cumsum()

    features_interp = []
    for _, group in df.groupby('sid'):
        freqs = group['freq'].values
        features = group['power'].values
        new_freq = np.linspace(0, 0.5, interp_len)
        new_feat = np.interp(new_freq, freqs, features)
        features_interp.append(new_feat)

    return np.array(features_interp)


def test_classifier(classifier_path: str, test_human_file: str, test_model_file: str, verbose: bool = False):
    """
    Test the trained classifier on new data
    
    Args:
        classifier_path: Path to the saved classifier (pkl file)
        test_human_file: Path to human test data
        test_model_file: Path to model test data
        verbose: Whether to print detailed results
    
    Returns:
        auroc_score, accuracy_score
    """
    # Load the trained classifier
    with open(classifier_path, 'rb') as f:
        classifier = pickle.load(f)
    
    if verbose:
        print(f"Loaded classifier from: {classifier_path}")
    
    # Process test data
    human_circlemean = get_circular_mean(test_human_file)
    model_circlemean = get_circular_mean(test_model_file)
    
    x_human = get_features(human_circlemean)
    y_human = np.zeros(x_human.shape[0])  # Human = 0
    x_model = get_features(model_circlemean)
    y_model = np.ones(x_model.shape[0])   # Model = 1
    
    x_test = np.concatenate([x_human, x_model], axis=0)
    y_test = np.concatenate([y_human, y_model], axis=0)
    
    if verbose:
        print(f"Test data shape: {x_test.shape}")
        print(f"Human samples: {len(x_human)}, Model samples: {len(x_model)}")
    
    # Make predictions
    y_pred = classifier.predict(x_test)
    y_pred_proba = classifier.predict_proba(x_test)[:, 1]  # Probability of being model-generated
    
    # Calculate metrics
    auroc = roc_auc_score(y_test, y_pred_proba)
    accuracy = accuracy_score(y_test, y_pred)
    
    return auroc, accuracy, y_test, y_pred, y_pred_proba


def main(args):
    # Check if files exist
    assert os.path.exists(args.classifier), f"Classifier file {args.classifier} does not exist"
    assert os.path.exists(args.test_human), f"Test human file {args.test_human} does not exist"
    assert os.path.exists(args.test_model), f"Test model file {args.test_model} does not exist"
    
    # Test the classifier
    auroc, accuracy, y_test, y_pred, y_pred_proba = test_classifier(
        args.classifier, args.test_human, args.test_model, args.verbose
    )
    
    # Print results
    if args.verbose:
        print(f"\n=== Test Results ===")
        print(f"AUROC: {auroc:.4f}")
        print(f"Accuracy: {accuracy:.4f}")
        
        print(f"\nClassification Report:")
        print(classification_report(y_test, y_pred, target_names=['Human', 'Model']))
        
        # Print some prediction examples
        # print(f"\nSample predictions (first 10):")
        # for i in range(min(10, len(y_test))):
        #     true_label = "Human" if y_test[i] == 0 else "Model"
        #     pred_label = "Human" if y_pred[i] == 0 else "Model"
        #     print(f"  True: {true_label:5s} | Pred: {pred_label:5s} | Prob: {y_pred_proba[i]:.3f}")
    else:
        print(f"AUROC: {auroc:.4f}")
        print(f"Accuracy: {accuracy:.4f}")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Test trained classifier on new data')
    parser.add_argument('--classifier', type=str, required=True, 
                       help='Path to the saved classifier (pkl file)')
    parser.add_argument('--test_human', type=str, required=True, 
                       help='Path to human test data')
    parser.add_argument('--test_model', type=str, required=True, 
                       help='Path to model test data')
    parser.add_argument('--verbose', action='store_true', default=False, 
                       help='Print detailed results')
    
    args = parser.parse_args()
    main(args)
