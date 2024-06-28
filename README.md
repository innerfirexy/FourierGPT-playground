# FourierGPT

## Estimate likelihood scores

```bash
python run_entropy.py -i INPUT_TEXT_FILE -o OUTPUT_NLL_FILE --model_path MODEL_NAME
```

`NLL` is for negative log likelihood.

## Normalize likelihood and Fourier transform

```bash
python run_fft.py -i INPUT_NLL_FILE -o OUTPUT_FFT_FILE -p zscore --value norm
```

You can set `-p` to `zscore`, `logzs`, or other values for different normalization methods

## Further steps

- Use `circular.ipynb` to conduct circularization operation on likelihood scores.
- Use `classifier_circlemean.ipynb` for supervised classification.
- Use `classifier_pairwise.ipynb` for heuristic-based classification.