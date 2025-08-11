#!/bin/bash

# Change to the project root directory where run_sup_cls.py is located
cd "$(dirname "$0")/.."

# Run classification on Claude data
echo "--- Claude-Haiku XSum ---"
echo "Claude-3-Opus"
python run_sup_cls.py --human data/Claude/Claude-Haiku/xsum_claude-3-opus-20240229_human.txt --model data/Claude/Claude-Haiku/xsum_claude-3-opus-20240229_model.txt
echo "Claude-3-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/xsum_claude-3-haiku-20240307_human.txt --model data/Claude/Claude-Haiku/xsum_claude-3-haiku-20240307_model.txt
echo "Claude-3.5-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/xsum_claude-3-5-haiku-20241022_human.txt --model data/Claude/Claude-Haiku/xsum_claude-3-5-haiku-20241022_model.txt
echo "------------"

echo "--- Claude-Haiku Writing ---"
echo "Claude-3-Opus"
python run_sup_cls.py --human data/Claude/Claude-Haiku/writing_claude-3-opus-20240229_human.txt --model data/Claude/Claude-Haiku/writing_claude-3-opus-20240229_model.txt
echo "Claude-3-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/writing_claude-3-haiku-20240307_human.txt --model data/Claude/Claude-Haiku/writing_claude-3-haiku-20240307_model.txt
echo "Claude-3.5-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/writing_claude-3-5-haiku-20241022_human.txt --model data/Claude/Claude-Haiku/writing_claude-3-5-haiku-20241022_model.txt
echo "------------"

echo "--- Claude-Haiku PubMed ---"
echo "Claude-3-Opus"
python run_sup_cls.py --human data/Claude/Claude-Haiku/pubmed_claude-3-opus-20240229_human.txt --model data/Claude/Claude-Haiku/pubmed_claude-3-opus-20240229_model.txt
echo "Claude-3-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/pubmed_claude-3-haiku-20240307_human.txt --model data/Claude/Claude-Haiku/pubmed_claude-3-haiku-20240307_model.txt
echo "Claude-3.5-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/pubmed_claude-3-5-haiku-20241022_human.txt --model data/Claude/Claude-Haiku/pubmed_claude-3-5-haiku-20241022_model.txt
echo "------------"

echo "--- Claude-Haiku PeerRead ---"
echo "Claude-3-Opus"
python run_sup_cls.py --human data/Claude/Claude-Haiku/peerread_claude-3-opus-20240229_human.txt --model data/Claude/Claude-Haiku/peerread_claude-3-opus-20240229_model.txt
echo "Claude-3-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/peerread_claude-3-haiku-20240307_human.txt --model data/Claude/Claude-Haiku/peerread_claude-3-haiku-20240307_model.txt
echo "Claude-3.5-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/peerread_claude-3-5-haiku-20241022_human.txt --model data/Claude/Claude-Haiku/peerread_claude-3-5-haiku-20241022_model.txt
echo "------------"

echo "--- Claude-Haiku Harmful ---"
echo "Claude-3-Opus"
python run_sup_cls.py --human data/Claude/Claude-Haiku/harmful_claude-3-opus-20240229_human.txt --model data/Claude/Claude-Haiku/harmful_claude-3-opus-20240229_model.txt
echo "Claude-3-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/harmful_claude-3-haiku-20240307_human.txt --model data/Claude/Claude-Haiku/harmful_claude-3-haiku-20240307_model.txt
echo "Claude-3.5-Haiku"
python run_sup_cls.py --human data/Claude/Claude-Haiku/harmful_claude-3-5-haiku-20241022_human.txt --model data/Claude/Claude-Haiku/harmful_claude-3-5-haiku-20241022_model.txt
echo "------------"

echo "--- Claude-Sonnet XSum ---"
echo "Claude-3-Sonnet"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/xsum_claude-3-sonnet-20240229_human.txt --model data/Claude/Claude-Sonnet/xsum_claude-3-sonnet-20240229_model.txt
echo "Claude-3.5-Sonnet (2024-06-20)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20240620_human.txt --model data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20240620_model.txt
echo "Claude-3.5-Sonnet (2024-10-22)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20241022_human.txt --model data/Claude/Claude-Sonnet/xsum_claude-3-5-sonnet-20241022_model.txt
echo "------------"

echo "--- Claude-Sonnet Writing ---"
echo "Claude-3-Sonnet"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/writing_claude-3-sonnet-20240229_human.txt --model data/Claude/Claude-Sonnet/writing_claude-3-sonnet-20240229_model.txt
echo "Claude-3.5-Sonnet (2024-06-20)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20240620_human.txt --model data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20240620_model.txt
echo "Claude-3.5-Sonnet (2024-10-22)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20241022_human.txt --model data/Claude/Claude-Sonnet/writing_claude-3-5-sonnet-20241022_model.txt
echo "------------"

echo "--- Claude-Sonnet PubMed ---"
echo "Claude-3-Sonnet"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/pubmed_claude-3-sonnet-20240229_human.txt --model data/Claude/Claude-Sonnet/pubmed_claude-3-sonnet-20240229_model.txt
echo "Claude-3.5-Sonnet (2024-06-20)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20240620_human.txt --model data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20240620_model.txt
echo "Claude-3.5-Sonnet (2024-10-22)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20241022_human.txt --model data/Claude/Claude-Sonnet/pubmed_claude-3-5-sonnet-20241022_model.txt
echo "------------"

echo "--- Claude-Sonnet PeerRead ---"
echo "Claude-3-Sonnet"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/peerread_claude-3-sonnet-20240229_human.txt --model data/Claude/Claude-Sonnet/peerread_claude-3-sonnet-20240229_model.txt
echo "Claude-3.5-Sonnet (2024-06-20)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20240620_human.txt --model data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20240620_model.txt
echo "Claude-3.5-Sonnet (2024-10-22)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20241022_human.txt --model data/Claude/Claude-Sonnet/peerread_claude-3-5-sonnet-20241022_model.txt
echo "------------"

echo "--- Claude-Sonnet Harmful ---"
echo "Claude-3-Sonnet"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_human.txt --model data/Claude/Claude-Sonnet/harmful_claude-3-sonnet-20240229_model.txt
echo "Claude-3.5-Sonnet (2024-06-20)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_human.txt --model data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20240620_model.txt
echo "Claude-3.5-Sonnet (2024-10-22)"
python run_sup_cls.py --human data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_human.txt --model data/Claude/Claude-Sonnet/harmful_claude-3-5-sonnet-20241022_model.txt
echo "------------"

echo "Done"
