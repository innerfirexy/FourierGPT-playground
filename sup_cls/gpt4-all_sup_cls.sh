#!/bin/bash

# Change to the project root directory where run_sup_cls.py is located
cd "$(dirname "$0")/.."

# Run classification on GPT4 data
echo "--- GPT4 XSum ---"
echo "GPT-4"
python run_sup_cls.py --human data/GPT4/xsum_gpt-4_human.txt --model data/GPT4/xsum_gpt-4_model.txt
echo "GPT-4-1106-preview"
python run_sup_cls.py --human data/GPT4/xsum_gpt-4-1106-preview_human.txt --model data/GPT4/xsum_gpt-4-1106-preview_model.txt
echo "GPT-4-0125-preview"
python run_sup_cls.py --human data/GPT4/xsum_gpt-4-0125-preview_human.txt --model data/GPT4/xsum_gpt-4-0125-preview_model.txt
echo "GPT-4-Turbo (2024-04-09)"
python run_sup_cls.py --human data/GPT4/xsum_gpt-4-turbo-2024-04-09_human.txt --model data/GPT4/xsum_gpt-4-turbo-2024-04-09_model.txt
echo "------------"

echo "--- GPT4 Writing ---"
echo "GPT-4"
python run_sup_cls.py --human data/GPT4/writing_gpt-4_human.txt --model data/GPT4/writing_gpt-4_model.txt
echo "GPT-4-1106-preview"
python run_sup_cls.py --human data/GPT4/writing_gpt-4-1106-preview_human.txt --model data/GPT4/writing_gpt-4-1106-preview_model.txt
echo "GPT-4-0125-preview"
python run_sup_cls.py --human data/GPT4/writing_gpt-4-0125-preview_human.txt --model data/GPT4/writing_gpt-4-0125-preview_model.txt
echo "GPT-4-Turbo (2024-04-09)"
python run_sup_cls.py --human data/GPT4/writing_gpt-4-turbo-2024-04-09_human.txt --model data/GPT4/writing_gpt-4-turbo-2024-04-09_model.txt
echo "------------"

echo "--- GPT4 PubMed ---"
echo "GPT-4"
python run_sup_cls.py --human data/GPT4/pubmed_gpt-4_human.txt --model data/GPT4/pubmed_gpt-4_model.txt
echo "GPT-4-1106-preview"
python run_sup_cls.py --human data/GPT4/pubmed_gpt-4-1106-preview_human.txt --model data/GPT4/pubmed_gpt-4-1106-preview_model.txt
echo "GPT-4-0125-preview"
python run_sup_cls.py --human data/GPT4/pubmed_gpt-4-0125-preview_human.txt --model data/GPT4/pubmed_gpt-4-0125-preview_model.txt
echo "GPT-4-Turbo (2024-04-09)"
python run_sup_cls.py --human data/GPT4/pubmed_gpt-4-turbo-2024-04-09_human.txt --model data/GPT4/pubmed_gpt-4-turbo-2024-04-09_model.txt
echo "------------"

echo "--- GPT4 PeerRead ---"
echo "GPT-4"
python run_sup_cls.py --human data/GPT4/peerread_gpt-4_human.txt --model data/GPT4/peerread_gpt-4_model.txt
echo "GPT-4-1106-preview"
python run_sup_cls.py --human data/GPT4/peerread_gpt-4-1106-preview_human.txt --model data/GPT4/peerread_gpt-4-1106-preview_model.txt
echo "GPT-4-0125-preview"
python run_sup_cls.py --human data/GPT4/peerread_gpt-4-0125-preview_human.txt --model data/GPT4/peerread_gpt-4-0125-preview_model.txt
echo "GPT-4-Turbo (2024-04-09)"
python run_sup_cls.py --human data/GPT4/peerread_gpt-4-turbo-2024-04-09_human.txt --model data/GPT4/peerread_gpt-4-turbo-2024-04-09_model.txt
echo "------------"

echo "--- GPT4 Harmful ---"
echo "GPT-4"
python run_sup_cls.py --human data/GPT4/harmful_gpt-4_human.txt --model data/GPT4/harmful_gpt-4_model.txt
echo "GPT-4-1106-preview"
python run_sup_cls.py --human data/GPT4/harmful_gpt-4-1106-preview_human.txt --model data/GPT4/harmful_gpt-4-1106-preview_model.txt
echo "GPT-4-0125-preview"
python run_sup_cls.py --human data/GPT4/harmful_gpt-4-0125-preview_human.txt --model data/GPT4/harmful_gpt-4-0125-preview_model.txt
echo "GPT-4-Turbo (2024-04-09)"
python run_sup_cls.py --human data/GPT4/harmful_gpt-4-turbo-2024-04-09_human.txt --model data/GPT4/harmful_gpt-4-turbo-2024-04-09_model.txt
echo "------------"

echo "Done"
