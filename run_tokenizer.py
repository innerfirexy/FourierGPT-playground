import argparse
import os
import json
import torch
from transformers import GPT2LMHeadModel, GPT2Tokenizer, AutoTokenizer
from tqdm import tqdm
import numpy as np
from config import load_config
from model import Model


def create_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', '-i', type=str, default='', 
                        help='input file', required=True)
    parser.add_argument('--output', '-o', type=str, default='',
                        help='output file', required=True)
    parser.add_argument(
        '--model', type=str, default='gpt2',
        choices=['gpt2', 'gpt2-medium', 'gpt2-large', 'gpt2-xl'],
        help=
        'if specified, this model will be used for estimating the entropy \
            (negative log-likelihood output) in replace of the default models'
    )
    parser.add_argument('--model_path', type=str, default='', help='load model locally if specified')
    parser.add_argument(
        "--config",
        type=str,
        default=None,
        help="path to the configuration file"
    )
    parser.add_argument('--delimiter', type=str, default=' ')
    parser.add_argument('--output_type', type=str, default='int', choices=['str', 'int'])
    return parser


def load_tokenizer(args):
    if len(args.model_path) > 0:
        model_path = args.model_path
        tokenizer = GPT2Tokenizer(tokenizer_file=os.path.join(model_path, 'tokenizer.json'),
                                  vocab_file=os.path.join(model_path, 'vocab.json'),
                                  merges_file=os.path.join(model_path, 'merges.txt'))
    elif len(args.config) > 0:
        config = load_config(args.config)
        tokenizer = AutoTokenizer.from_pretrained(config.model_est, trust_remote_code=True)
    else:
        model_path = args.model
        tokenizer = GPT2Tokenizer.from_pretrained(model_path)
    if isinstance(tokenizer, GPT2Tokenizer):
        tokenizer.pad_token = tokenizer.eos_token
    return tokenizer


def main(args):
    tokenizer = load_tokenizer(args)
    with open(args.input, 'r') as fr:
        data = [line.strip() for line in fr.readlines()]
    tokenized_results = []
    for line in tqdm(data):
        # print(line)
        token_ids = tokenizer.encode(line)
        # print(token_ids)
        # tokens = [tokenizer.decode([token_id]) for token_id in token_ids]
        # tokens = tokenizer.convert_ids_to_tokens(token_ids) # This results in prefix 'Ġ' as deliminater
        # print(tokens)
        tokenized_results.append(token_ids)
    with open(args.output, 'w') as fw:
        for token_ids in tokenized_results:
            fw.write(args.delimiter.join([str(token_id) for token_id in token_ids]) + '\n')


if __name__ == "__main__":
    parser = create_parser()
    args = parser.parse_args()
    main(args)