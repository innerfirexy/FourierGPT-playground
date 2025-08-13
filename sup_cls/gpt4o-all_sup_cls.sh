#!/bin/bash

# Change to the project root directory where run_sup_cls.py is located
cd "$(dirname "$0")/.."

# Run classification on GPT4o data
echo "--- GPT4o XSum ---"
echo "GPT-4o (2024-05-13)"
python run_sup_cls.py --human data/GPT4o/xsum_gpt-4o-2024-05-13_human.txt --model data/GPT4o/xsum_gpt-4o-2024-05-13_model.txt
echo "GPT-4o-mini (2024-07-18)"
python run_sup_cls.py --human data/GPT4o/xsum_gpt-4o-mini-2024-07-18_human.txt --model data/GPT4o/xsum_gpt-4o-mini-2024-07-18_model.txt
echo "GPT-4o (2024-08-06)"
python run_sup_cls.py --human data/GPT4o/xsum_gpt-4o-2024-08-06_human.txt --model data/GPT4o/xsum_gpt-4o-2024-08-06_model.txt
echo "GPT-4o (2024-11-20)"
python run_sup_cls.py --human data/GPT4o/xsum_gpt-4o-2024-11-20_human.txt --model data/GPT4o/xsum_gpt-4o-2024-11-20_model.txt
echo "ChatGPT-4o-latest"
python run_sup_cls.py --human data/GPT4o/xsum_chatgpt-4o-latest_human.txt --model data/GPT4o/xsum_chatgpt-4o-latest_model.txt
echo "------------"

echo "--- GPT4o Writing ---"
echo "GPT-4o (2024-05-13)"
python run_sup_cls.py --human data/GPT4o/writing_gpt-4o-2024-05-13_human.txt --model data/GPT4o/writing_gpt-4o-2024-05-13_model.txt
echo "GPT-4o-mini (2024-07-18)"
python run_sup_cls.py --human data/GPT4o/writing_gpt-4o-mini-2024-07-18_human.txt --model data/GPT4o/writing_gpt-4o-mini-2024-07-18_model.txt
echo "GPT-4o (2024-08-06)"
python run_sup_cls.py --human data/GPT4o/writing_gpt-4o-2024-08-06_human.txt --model data/GPT4o/writing_gpt-4o-2024-08-06_model.txt
echo "GPT-4o (2024-11-20)"
python run_sup_cls.py --human data/GPT4o/writing_gpt-4o-2024-11-20_human.txt --model data/GPT4o/writing_gpt-4o-2024-11-20_model.txt
echo "ChatGPT-4o-latest"
python run_sup_cls.py --human data/GPT4o/writing_chatgpt-4o-latest_human.txt --model data/GPT4o/writing_chatgpt-4o-latest_model.txt
echo "------------"

echo "--- GPT4o PubMed ---"
echo "GPT-4o (2024-05-13)"
python run_sup_cls.py --human data/GPT4o/pubmed_gpt-4o-2024-05-13_human.txt --model data/GPT4o/pubmed_gpt-4o-2024-05-13_model.txt
echo "GPT-4o-mini (2024-07-18)"
python run_sup_cls.py --human data/GPT4o/pubmed_gpt-4o-mini-2024-07-18_human.txt --model data/GPT4o/pubmed_gpt-4o-mini-2024-07-18_model.txt
echo "GPT-4o (2024-08-06)"
python run_sup_cls.py --human data/GPT4o/pubmed_gpt-4o-2024-08-06_human.txt --model data/GPT4o/pubmed_gpt-4o-2024-08-06_model.txt
echo "GPT-4o (2024-11-20)"
python run_sup_cls.py --human data/GPT4o/pubmed_gpt-4o-2024-11-20_human.txt --model data/GPT4o/pubmed_gpt-4o-2024-11-20_model.txt
echo "ChatGPT-4o-latest"
python run_sup_cls.py --human data/GPT4o/pubmed_chatgpt-4o-latest_human.txt --model data/GPT4o/pubmed_chatgpt-4o-latest_model.txt
echo "------------"

echo "--- GPT4o PeerRead ---"
echo "GPT-4o (2024-05-13)"
python run_sup_cls.py --human data/GPT4o/peerread_gpt-4o-2024-05-13_human.txt --model data/GPT4o/peerread_gpt-4o-2024-05-13_model.txt
echo "GPT-4o-mini (2024-07-18)"
python run_sup_cls.py --human data/GPT4o/peerread_gpt-4o-mini-2024-07-18_human.txt --model data/GPT4o/peerread_gpt-4o-mini-2024-07-18_model.txt
echo "GPT-4o (2024-08-06)"
python run_sup_cls.py --human data/GPT4o/peerread_gpt-4o-2024-08-06_human.txt --model data/GPT4o/peerread_gpt-4o-2024-08-06_model.txt
echo "GPT-4o (2024-11-20)"
python run_sup_cls.py --human data/GPT4o/peerread_gpt-4o-2024-11-20_human.txt --model data/GPT4o/peerread_gpt-4o-2024-11-20_model.txt
echo "ChatGPT-4o-latest"
python run_sup_cls.py --human data/GPT4o/peerread_chatgpt-4o-latest_human.txt --model data/GPT4o/peerread_chatgpt-4o-latest_model.txt
echo "------------"

echo "--- GPT4o Harmful ---"
echo "GPT-4o (2024-05-13)"
python run_sup_cls.py --human data/GPT4o/harmful_gpt-4o-2024-05-13_human.txt --model data/GPT4o/harmful_gpt-4o-2024-05-13_model.txt
echo "GPT-4o-mini (2024-07-18)"
python run_sup_cls.py --human data/GPT4o/harmful_gpt-4o-mini-2024-07-18_human.txt --model data/GPT4o/harmful_gpt-4o-mini-2024-07-18_model.txt
echo "GPT-4o (2024-08-06)"
python run_sup_cls.py --human data/GPT4o/harmful_gpt-4o-2024-08-06_human.txt --model data/GPT4o/harmful_gpt-4o-2024-08-06_model.txt
echo "GPT-4o (2024-11-20)"
python run_sup_cls.py --human data/GPT4o/harmful_gpt-4o-2024-11-20_human.txt --model data/GPT4o/harmful_gpt-4o-2024-11-20_model.txt
echo "ChatGPT-4o-latest"
python run_sup_cls.py --human data/GPT4o/harmful_chatgpt-4o-latest_human.txt --model data/GPT4o/harmful_chatgpt-4o-latest_model.txt
echo "------------"

echo "Done"
