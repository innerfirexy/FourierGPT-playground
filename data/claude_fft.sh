#!/bin/bash

# FFT processing script for Claude-Haiku and Claude-Sonnet datasets
# This script processes all .txt files in the specified directories using run_fft.py

# Change to the project root directory where run_fft.py is located
cd "$(dirname "$0")/.."

echo "Starting FFT processing for Claude datasets..."

# Process Claude-Haiku dataset
echo "Processing Claude-Haiku dataset..."

# xsum files
python run_fft.py -i data/Claude/Claude-Haiku/xsum_claude-3-opus-20240229_human.txt -o data/Claude-Haiku_nllzs_fftnorm/xsum_claude-3-opus-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/xsum_claude-3-opus-20240229_model.txt -o data/Claude-Haiku_nllzs_fftnorm/xsum_claude-3-opus-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/xsum_claude-3-haiku-20240307_human.txt -o data/Claude-Haiku_nllzs_fftnorm/xsum_claude-3-haiku-20240307_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/xsum_claude-3-haiku-20240307_model.txt -o data/Claude-Haiku_nllzs_fftnorm/xsum_claude-3-haiku-20240307_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/xsum_claude-3-5-haiku-20241022_human.txt -o data/Claude-Haiku_nllzs_fftnorm/xsum_claude-3-5-haiku-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/xsum_claude-3-5-haiku-20241022_model.txt -o data/Claude-Haiku_nllzs_fftnorm/xsum_claude-3-5-haiku-20241022_model.nllzs.fftnorm.txt -p zscore

# writing files
python run_fft.py -i data/Claude/Claude-Haiku/writing_claude-3-opus-20240229_human.txt -o data/Claude-Haiku_nllzs_fftnorm/writing_claude-3-opus-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/writing_claude-3-opus-20240229_model.txt -o data/Claude-Haiku_nllzs_fftnorm/writing_claude-3-opus-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/writing_claude-3-haiku-20240307_human.txt -o data/Claude-Haiku_nllzs_fftnorm/writing_claude-3-haiku-20240307_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/writing_claude-3-haiku-20240307_model.txt -o data/Claude-Haiku_nllzs_fftnorm/writing_claude-3-haiku-20240307_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/writing_claude-3-5-haiku-20241022_human.txt -o data/Claude-Haiku_nllzs_fftnorm/writing_claude-3-5-haiku-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/writing_claude-3-5-haiku-20241022_model.txt -o data/Claude-Haiku_nllzs_fftnorm/writing_claude-3-5-haiku-20241022_model.nllzs.fftnorm.txt -p zscore

# pubmed files
python run_fft.py -i data/Claude/Claude-Haiku/pubmed_claude-3-opus-20240229_human.txt -o data/Claude-Haiku_nllzs_fftnorm/pubmed_claude-3-opus-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/pubmed_claude-3-opus-20240229_model.txt -o data/Claude-Haiku_nllzs_fftnorm/pubmed_claude-3-opus-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/pubmed_claude-3-haiku-20240307_human.txt -o data/Claude-Haiku_nllzs_fftnorm/pubmed_claude-3-haiku-20240307_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/pubmed_claude-3-haiku-20240307_model.txt -o data/Claude-Haiku_nllzs_fftnorm/pubmed_claude-3-haiku-20240307_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/pubmed_claude-3-5-haiku-20241022_human.txt -o data/Claude-Haiku_nllzs_fftnorm/pubmed_claude-3-5-haiku-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/pubmed_claude-3-5-haiku-20241022_model.txt -o data/Claude-Haiku_nllzs_fftnorm/pubmed_claude-3-5-haiku-20241022_model.nllzs.fftnorm.txt -p zscore

# peerread files
python run_fft.py -i data/Claude/Claude-Haiku/peerread_claude-3-opus-20240229_human.txt -o data/Claude-Haiku_nllzs_fftnorm/peerread_claude-3-opus-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/peerread_claude-3-opus-20240229_model.txt -o data/Claude-Haiku_nllzs_fftnorm/peerread_claude-3-opus-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/peerread_claude-3-haiku-20240307_human.txt -o data/Claude-Haiku_nllzs_fftnorm/peerread_claude-3-haiku-20240307_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/peerread_claude-3-haiku-20240307_model.txt -o data/Claude-Haiku_nllzs_fftnorm/peerread_claude-3-haiku-20240307_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/peerread_claude-3-5-haiku-20241022_human.txt -o data/Claude-Haiku_nllzs_fftnorm/peerread_claude-3-5-haiku-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/peerread_claude-3-5-haiku-20241022_model.txt -o data/Claude-Haiku_nllzs_fftnorm/peerread_claude-3-5-haiku-20241022_model.nllzs.fftnorm.txt -p zscore

# harmful files
python run_fft.py -i data/Claude/Claude-Haiku/harmful_claude-3-opus-20240229_human.txt -o data/Claude-Haiku_nllzs_fftnorm/harmful_claude-3-opus-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/harmful_claude-3-opus-20240229_model.txt -o data/Claude-Haiku_nllzs_fftnorm/harmful_claude-3-opus-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/harmful_claude-3-haiku-20240307_human.txt -o data/Claude-Haiku_nllzs_fftnorm/harmful_claude-3-haiku-20240307_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/harmful_claude-3-haiku-20240307_model.txt -o data/Claude-Haiku_nllzs_fftnorm/harmful_claude-3-haiku-20240307_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/harmful_claude-3-5-haiku-20241022_human.txt -o data/Claude-Haiku_nllzs_fftnorm/harmful_claude-3-5-haiku-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Haiku/harmful_claude-3-5-haiku-20241022_model.txt -o data/Claude-Haiku_nllzs_fftnorm/harmful_claude-3-5-haiku-20241022_model.nllzs.fftnorm.txt -p zscore

# Process Claude-Sonnet dataset
echo "Processing Claude-Sonnet dataset..."

# xsum files
python run_fft.py -i data/Claude/Claude-Sonnet/xsum_claude-3-sonnet-20240229_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/xsum_claude-3-sonnet-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/xsum_claude-3-sonnet-20240229_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/xsum_claude-3-sonnet-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20240620_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/xsum_claude-3-5-sonnet-20240620_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20240620_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/xsum_claude-3-5-sonnet-20240620_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20241022_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/xsum_claude-3-5-sonnet-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20241022_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/xsum_claude-3-5-sonnet-20241022_model.nllzs.fftnorm.txt -p zscore

# writing files
python run_fft.py -i data/Claude/Claude-Sonnet/writing_claude-3-sonnet-20240229_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/writing_claude-3-sonnet-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/writing_claude-3-sonnet-20240229_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/writing_claude-3-sonnet-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20240620_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/writing_claude-3-5-sonnet-20240620_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20240620_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/writing_claude-3-5-sonnet-20240620_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20241022_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/writing_claude-3-5-sonnet-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20241022_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/writing_claude-3-5-sonnet-20241022_model.nllzs.fftnorm.txt -p zscore

# pubmed files
python run_fft.py -i data/Claude/Claude-Sonnet/pubmed_claude-3-sonnet-20240229_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/pubmed_claude-3-sonnet-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/pubmed_claude-3-sonnet-20240229_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/pubmed_claude-3-sonnet-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20240620_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/pubmed_claude-3-5-sonnet-20240620_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20240620_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/pubmed_claude-3-5-sonnet-20240620_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20241022_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/pubmed_claude-3-5-sonnet-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20241022_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/pubmed_claude-3-5-sonnet-20241022_model.nllzs.fftnorm.txt -p zscore

# peerread files
python run_fft.py -i data/Claude/Claude-Sonnet/peerread_claude-3-sonnet-20240229_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/peerread_claude-3-sonnet-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/peerread_claude-3-sonnet-20240229_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/peerread_claude-3-sonnet-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20240620_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/peerread_claude-3-5-sonnet-20240620_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20240620_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/peerread_claude-3-5-sonnet-20240620_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20241022_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/peerread_claude-3-5-sonnet-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20241022_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/peerread_claude-3-5-sonnet-20241022_model.nllzs.fftnorm.txt -p zscore

# harmful files
python run_fft.py -i data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/harmful_claude-3-sonnet-20240229_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/harmful_claude-3-sonnet-20240229_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/harmful_claude-3-5-sonnet-20240620_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/harmful_claude-3-5-sonnet-20240620_model.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_human.txt -o data/Claude-Sonnet_nllzs_fftnorm/harmful_claude-3-5-sonnet-20241022_human.nllzs.fftnorm.txt -p zscore
python run_fft.py -i data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_model.txt -o data/Claude-Sonnet_nllzs_fftnorm/harmful_claude-3-5-sonnet-20241022_model.nllzs.fftnorm.txt -p zscore

echo "FFT processing completed for all Claude datasets!"