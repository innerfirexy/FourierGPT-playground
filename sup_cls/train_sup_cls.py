import sys
sys.path.append('..')
from run_fft import FFTProcessor
import numpy as np
import pandas as pd
import argparse
import os
import pickle
from typing import Union

from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import accuracy_score
from sklearn.feature_selection import SelectKBest
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.metrics import roc_auc_score


# Preprocessing
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

def get_circular_full(input_file: str, require_sid=True):
    fft_processor = FFTProcessor(method='fft', preprocess='logzs', value='norm', require_sid=False)
    nll_raw = fft_processor._read_data(data_file=input_file)
    circle_results = []
    for i, nll in enumerate(nll_raw):
        nll_c = circular(nll)
        nll_c = fft_processor._preprocess(nll_c)
        f, p, sids = fft_processor._fft_batch(nll_c, require_sid=True) # Note this `require_sid` is different from the function argument
        df = pd.DataFrame({'freq': np.concatenate(f), 
                           'power': np.concatenate(p), 
                           'circular_index': np.concatenate(sids)}) # The `sids` returned from `_fft_batch` means the index of each circular operation
        if require_sid: # This is the actual sequence id
            df['sid'] = i
        circle_results.append(df)
    df_circle = pd.concat(circle_results)
    return df_circle

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
        power_mean = np.mean(power, axis=0) # This is where the mean is calculated, different from get_circular_full()
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


# Extract features by linear interpolation
def get_features(spectrum_data: Union[str, pd.DataFrame], interp_len: int = 500):
    """
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


# Run hyperparameter tuning and training
def train_classifier(human_nll_file: str, model_nll_file: str, save_intermid: bool = False):
    """
    Train classifier with hyperparameter tuning using cross-validation
    """
    # circularization
    human_ciclemean = get_circular_mean(human_nll_file)
    model_circlemean = get_circular_mean(model_nll_file)
    if save_intermid:
        def get_output_file(nll_file):
            _dir, _basename = os.path.split(nll_file)
            _basename, _ext = os.path.splitext(_basename)
            return os.path.join(_dir, _basename + '.circlemean.txt')
        human_ciclemean.to_csv(get_output_file(human_nll_file), index=False)
        model_circlemean.to_csv(get_output_file(model_nll_file), index=False)

    x_human = get_features(human_ciclemean)
    y_human = np.zeros(x_human.shape[0])
    x_model = get_features(model_circlemean)
    y_model = np.ones(x_model.shape[0])

    x = np.concatenate([x_human, x_model], axis=0)
    y = np.concatenate([y_human, y_model], axis=0)

    # Define the pipeline
    pipeline = make_pipeline(
        StandardScaler(),
        SelectKBest(),
        SVC(gamma='auto', kernel='rbf', probability=True)
    )

    # Define parameter grid for hyperparameter tuning
    param_grid = {
        'selectkbest__k': [50, 100, 120, 150, 200],
        'svc__C': [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]
    }

    # Perform grid search with cross-validation
    grid_search = GridSearchCV(
        pipeline, 
        param_grid, 
        cv=5, 
        scoring='roc_auc',
        n_jobs=-1,
        verbose=1
    )

    # Fit the grid search
    grid_search.fit(x, y)

    # Get best results
    best_params = grid_search.best_params_
    best_score = grid_search.best_score_
    best_estimator = grid_search.best_estimator_

    # Cross-validation scores for the best model
    cv_scores = cross_val_score(best_estimator, x, y, cv=5, scoring='roc_auc')

    return best_estimator, best_params, best_score, cv_scores


def main(args):
    best_estimator, best_params, best_score, cv_scores = train_classifier(
        args.human, args.model, args.save_intermid
    )
    
    if args.verbose:
        print(f'Best parameters: {best_params}')
        print(f'Best cross-validation AUROC: {best_score:.4f}')
        print(f'Cross-validated AUROC scores: {cv_scores}')
    
    print(f'Best k: {best_params["selectkbest__k"]}')
    print(f'Best C: {best_params["svc__C"]}')
    print(f'Mean AUROC: {np.mean(cv_scores):.4f} (+/- {np.std(cv_scores) * 2:.4f})')

    # Save the best classifier if path is provided
    if args.save_classifier:
        with open(args.save_classifier, 'wb') as f:
            pickle.dump(best_estimator, f)
        print(f'Best classifier saved to: {args.save_classifier}')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--human', type=str, required=True, help='Human raw NLL data')
    parser.add_argument('--model', type=str, required=True, help='Model raw NLL data')
    parser.add_argument('--verbose', action='store_true', default=False, help='Print detailed results or not (default: False)')
    parser.add_argument('--save_intermid', action='store_true', default=False, help='Save intermediate results')
    parser.add_argument('--save_classifier', type=str, default=None, help='Path to save the best trained classifier (pickle format)')
    
    args = parser.parse_args()
    assert os.path.exists(args.human), f'File {args.human} does not exist'
    assert os.path.exists(args.model), f'File {args.model} does not exist'
    main(args)
