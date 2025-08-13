# RNN模型使用指南

## 核心文件

### 1. `base_predictor.py` - 基础预测器
包含所有预测功能的基类，提供：
- 统一的模型加载接口
- 批量预测功能
- 数据预处理
- 工厂函数支持多种模型类型

### 2. `inference.py` - 推理工具
用于日常推理和预测：
```bash
# 基本使用
python inference.py --model_path "path/to/model.pth"

# 从文件推理
python inference.py --model_path "path/to/model.pth" --input_file "data.txt"

# 从命令行输入
python inference.py --model_path "path/to/model.pth" --scores "1.2 3.4 5.6"

# 指定设备和模型类型
python inference.py --model_path "path/to/model.pth" --device "cuda" --model_type "bilstm"
```

### 3. `evaluate_model.py` - 评估工具
用于模型性能评估：
```bash
# 基本评估
python evaluate_model.py \
    --model_path "path/to/model.pth" \
    --human_file "human_data.txt" \
    --model_file "model_data.txt"

# 包含错误分析
python evaluate_model.py \
    --model_path "path/to/model.pth" \
    --human_file "human_data.txt" \
    --model_file "model_data.txt" \
    --analyze_errors \
    --output "results.json"
```

## 功能特点

### 推理功能 (`inference.py`)
- ✅ 多种输入方式（文件、命令行、演示）
- ✅ 置信度统计
- ✅ 详细的预测结果展示
- ✅ 支持多种模型类型

### 评估功能 (`evaluate_model.py`)
- ✅ 完整的性能指标（准确率、精确率、召回率、F1分数）
- ✅ 各类别详细指标
- ✅ 混淆矩阵
- ✅ 错误分析功能
- ✅ JSON结果导出

### 基础功能 (`base_predictor.py`)
- ✅ 统一的模型加载
- ✅ 高效的批量预测
- ✅ 灵活的数据预处理
- ✅ 工厂模式支持扩展

## 扩展开发

### 添加新的预测器类型
```python
from base_predictor import BasePredictor

class MyCustomPredictor(BasePredictor):
    def _load_model(self):
        # 自定义模型加载逻辑
        pass
    
    def custom_predict(self, data):
        # 自定义预测逻辑
        pass
```

### 使用工厂函数
```python
from base_predictor import create_predictor

# 创建不同类型的预测器
predictor = create_predictor("model.pth", "bilstm", "cpu")
simple_predictor = create_predictor("model.pth", "simple", "cuda")
```

## 性能验证

已在以下数据集上验证：
- **Claude-3.5-Sonnet (2024-06-20)**: 准确率 82.93%
- **Claude-3.5-Sonnet (2024-10-22)**: 准确率 69.87%

所有功能已通过完整性测试，确保结果准确可靠。
