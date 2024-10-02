import torch

from tqdm import tqdm
import torch.nn as nn
import numpy as np
from einops import rearrange
import spacy
import ipdb
import kenlm

model = kenlm.Model('/data1/C4_2gram.bin')

nlp = spacy.load("en_core_web_sm")


def process_custom(input_file, output_file):
    """
    For custom models specified in configuration file
    """
    # Load model and data
    #model = Model(args.model_est)
    with open(input_file, 'r') as f:
        data = [line.strip() for line in f.readlines()]
    # Compute
    print(len(data))
    results = []
    for line in (data):
        print(line)
        doc = nlp(line)
        tmp, verb_idx = [], []
        for i, (prob, length, oov) in enumerate(model.full_scores(line)):
            #print('{0} {1}: {2}'.format(prob, length, ' '.join(words[i + 2 - length:i + 2])))
            #if oov:
                #print('\t"{0}" is an OOV'.format(words[i + 1]))
            tmp.append(prob)
            if doc[i].pos == "VERB":
                verb_idx.append(i)
        b = np.mean(tmp)
        for idx in verb_idx:
            tmp[idx] = b
        #logits, nlls = model.forward(line)
        results.append(tmp)
    # Write results
    with open(output_file, 'w') as f:
        for res in results:
            if isinstance(res, torch.Tensor):
                res = res.numpy().tolist()
            res_str = ' '.join(f'{num:.4f}' for num in res)
            f.write(f'{res_str}\n')



if __name__ == '__main__':
    input_files = ['data/gpt-4/pubmed_gpt-4.original.txt', 'data/gpt-4/pubmed_gpt-4.sampled.txt',
                   'data/gpt-4/writing_gpt-4.original.txt', 'data/gpt-4/writing_gpt-4.sampled.txt',
                   'data/gpt-4/xsum_gpt-4.original.txt', 'data/gpt-4/xsum_gpt-4.sampled.txt']
    
    output_files = ['data/gpt-4/verb_masked/pubmed_gpt-4.original.2gram_mask_verb.txt', 'data/gpt-4/verb_masked/pubmed_gpt-4.sampled.2gram_mask_verb.txt',
                   'data/gpt-4/verb_masked/writing_gpt-4.original.2gram_mask_verb.txt', 'data/gpt-4/verb_masked/writing_gpt-4.sampled.2gram_mask_verb.txt',
                   'data/gpt-4/verb_masked/xsum_gpt-4.original.2gram_mask_verb.txt', 'data/gpt-4/verb_masked/xsum_gpt-4.sampled.2gram_mask_verb.txt']

    for input_file, output_file in zip(input_files, output_files):
        process_custom(input_file, output_file)
