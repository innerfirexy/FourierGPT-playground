#!/bin/bash

# Train Claude-Haiku classifiers for all domains
# This script trains SVC classifiers using train_sup_cls.py for each domain

echo "开始训练 Claude-Haiku 分类器..."
echo "=================================="

echo ""
echo "--- 训练 Writing Domain ---"
python train_sup_cls.py \
    --human ./train_data_claude-haiku/writing_claude-3-opus-20240229_human.txt \
    --model ./train_data_claude-haiku/writing_claude-3-opus-20240229_model.txt \
    --save_classifier best_claude-haiku_writing_classifier.pkl \
    --verbose

echo ""
echo "--- 训练 XSum Domain ---"
python train_sup_cls.py \
    --human ./train_data_claude-haiku/xsum_claude-3-opus-20240229_human.txt \
    --model ./train_data_claude-haiku/xsum_claude-3-opus-20240229_model.txt \
    --save_classifier best_claude-haiku_xsum_classifier.pkl \
    --verbose

echo ""
echo "--- 训练 PubMed Domain ---"
python train_sup_cls.py \
    --human ./train_data_claude-haiku/pubmed_claude-3-opus-20240229_human.txt \
    --model ./train_data_claude-haiku/pubmed_claude-3-opus-20240229_model.txt \
    --save_classifier best_claude-haiku_pubmed_classifier.pkl \
    --verbose

echo ""
echo "--- 训练 PeerRead Domain ---"
python train_sup_cls.py \
    --human ./train_data_claude-haiku/peerread_claude-3-opus-20240229_human.txt \
    --model ./train_data_claude-haiku/peerread_claude-3-opus-20240229_model.txt \
    --save_classifier best_claude-haiku_peerread_classifier.pkl \
    --verbose

echo ""
echo "--- 训练 Harmful Domain ---"
python train_sup_cls.py \
    --human ./train_data_claude-haiku/harmful_claude-3-opus-20240229_human.txt \
    --model ./train_data_claude-haiku/harmful_claude-3-opus-20240229_model.txt \
    --save_classifier best_claude-haiku_harmful_classifier.pkl \
    --verbose

echo ""
echo "=================================="
echo "所有 Claude-Haiku 分类器训练完成！"
echo ""
echo "生成的分类器文件："
ls -lh best_claude-haiku_*_classifier.pkl

echo ""
echo "现在可以运行时间线实验："
echo "python claude-haiku_timeline.py"

