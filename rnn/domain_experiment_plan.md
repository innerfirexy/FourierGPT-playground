# RNN跨域分类实验规划

## 实验目标
仿照 `../sup_cls/claude-all_sup_cls.sh` 的实验设计，使用RNN模型验证：
1. 不同domain上的分类效果
2. 分类效果如何随模型演进而变化
3. RNN与传统SVM+FFT方法的对比

## 数据分析

### Claude-Haiku 数据集规模
- **Harmful**: 150 samples × 3 models = 450 total
- **PubMed**: 150 samples × 3 models = 450 total  
- **PeerRead**: 150 samples × 3 models = 450 total
- **Writing**: 150 samples × 3 models = 450 total
- **XSum**: 150 samples × 3 models = 450 total

每个domain每个模型：150个人类样本 + 150个AI样本 = 300样本

### 模型演进时间线
1. **Claude-3-Opus** (2024-02-29)
2. **Claude-3-Haiku** (2024-03-07)  
3. **Claude-3.5-Haiku** (2024-10-22)

## 可行性分析

### ✅ 有利因素
1. **数据充足**: 每个domain-model组合有300个样本，足够训练小型RNN
2. **已有基础设施**: 
   - 完善的数据加载器 (`data_loader.py`)
   - 训练框架 (`trainer.py`, `train.py`)
   - 评估工具 (`evaluate_model.py`)
   - 基础预测器 (`base_predictor.py`)
3. **对照基准**: 可与SVM+FFT方法对比

### ⚠️ 挑战因素
1. **数据规模相对较小**: 300样本对深度学习来说偏少
2. **序列长度差异**: 不同domain的NLL序列长度可能差异较大
3. **过拟合风险**: 小数据集容易过拟合

### 🔧 解决方案
1. **使用较小的模型**: 减少参数数量，降低过拟合风险
2. **数据增强**: 使用序列截断、填充等技术
3. **交叉验证**: 使用严格的交叉验证评估
4. **早停机制**: 防止过拟合
5. **正则化**: 使用dropout、权重衰减等

## 实验设计

### 阶段1: 单域实验 (Claude-Haiku Harmful)
**目标**: 验证RNN方法的基本可行性

```bash
# 训练不同模型版本的分类器
python train_domain_classifier.py \
    --domain harmful \
    --model_version claude-3-opus-20240229 \
    --output_dir outputs/harmful_opus

python train_domain_classifier.py \
    --domain harmful \
    --model_version claude-3-haiku-20240307 \
    --output_dir outputs/harmful_haiku

python train_domain_classifier.py \
    --domain harmful \
    --model_version claude-3-5-haiku-20241022 \
    --output_dir outputs/harmful_haiku35
```

### 阶段2: 多域实验
**目标**: 验证不同domain的分类效果

```bash
# 对所有5个domain进行实验
for domain in harmful peerread pubmed writing xsum; do
    for model in claude-3-opus-20240229 claude-3-haiku-20240307 claude-3-5-haiku-20241022; do
        python train_domain_classifier.py \
            --domain $domain \
            --model_version $model \
            --output_dir outputs/${domain}_${model}
    done
done
```

### 阶段3: 时间线分析
**目标**: 分析分类效果随模型演进的变化

```bash
python analyze_timeline.py \
    --results_dir outputs \
    --output timeline_analysis.json
```

## 实验配置

### 模型配置
```python
model_config = {
    'input_size': 1,
    'hidden_size': 64,      # 较小的隐藏层
    'num_layers': 1,        # 单层LSTM
    'num_classes': 2,
    'dropout': 0.3,         # 适中的dropout
    'bidirectional': True   # 双向LSTM
}
```

### 训练配置
```python
training_config = {
    'batch_size': 16,       # 较小的batch size
    'learning_rate': 0.001,
    'num_epochs': 50,
    'early_stopping': 10,   # 早停patience
    'train_split': 0.7,
    'val_split': 0.15,
    'test_split': 0.15,
    'cross_validation': 5   # 5折交叉验证
}
```

## 评估指标

### 主要指标
- **准确率 (Accuracy)**
- **精确率 (Precision)**  
- **召回率 (Recall)**
- **F1分数 (F1-Score)**
- **AUROC**: 与SVM方法对比

### 分析维度
1. **Domain效果**: 哪些domain更容易分类
2. **时间演进**: 分类难度是否随模型演进而变化
3. **方法对比**: RNN vs SVM+FFT
4. **泛化能力**: 跨域泛化性能

## 预期结果

### 假设1: Domain差异
- **Harmful**: 可能最容易分类（风格差异明显）
- **PubMed**: 技术性强，可能较难分类
- **Writing**: 创意性强，分类难度中等

### 假设2: 时间演进
- 随着模型演进，AI生成质量提升
- 分类难度可能逐渐增加
- Claude-3.5-Haiku可能最难检测

### 假设3: 方法对比
- RNN可能在序列模式识别上有优势
- SVM+FFT在频域特征上可能更稳定
- 小数据集上SVM可能表现更好

## 实施计划

### 第1周: 基础实验
- [ ] 实现 `train_domain_classifier.py`
- [ ] 完成单域实验 (Harmful)
- [ ] 验证基本可行性

### 第2周: 扩展实验  
- [ ] 完成所有5个domain的实验
- [ ] 实现时间线分析工具
- [ ] 收集所有结果数据

### 第3周: 分析对比
- [ ] 与SVM+FFT结果对比
- [ ] 生成可视化图表
- [ ] 撰写实验报告

## 风险评估

### 高风险
- **过拟合**: 小数据集的常见问题
- **计算资源**: 多个实验的计算需求

### 中风险  
- **超参数敏感**: 需要仔细调优
- **序列长度处理**: 不同domain的差异

### 低风险
- **基础设施**: 已有完善的代码框架
- **数据质量**: 数据已经过验证

## 成功标准

### 最小可行结果
- [ ] 至少在一个domain上RNN效果不差于随机猜测
- [ ] 能够观察到模型演进的趋势
- [ ] 完成与SVM方法的基本对比

### 理想结果
- [ ] RNN在某些domain上优于SVM+FFT
- [ ] 清晰的时间演进趋势
- [ ] 深入的跨域分析洞察
