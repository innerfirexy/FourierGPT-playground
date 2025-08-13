"""
Multi-class SVM classifier using One-vs-One strategy for model classification.
Classifies between Claude-Haiku, Claude-Sonnet, GPT4, and GPT4o using their latest model data.
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from run_fft import FFTProcessor
import numpy as np
import pandas as pd
import argparse
import pickle
from typing import Dict, List, Tuple, Union
from itertools import combinations
import glob

from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.feature_selection import SelectKBest
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.multiclass import OneVsOneClassifier
from sklearn.metrics import roc_auc_score


class MultiModelClassifier:
    """Multi-class classifier for AI model detection using One-vs-One SVM strategy."""
    
    def __init__(self, data_root: str = "data"):
        self.data_root = data_root
        self.model_configs = {
            'claude-haiku': {
                'path': 'Claude/Claude-Haiku',
                'latest_version': 'claude-3-5-haiku-20241022',
                'label': 0
            },
            'claude-sonnet': {
                'path': 'Claude/Claude-Sonnet', 
                'latest_version': 'claude-3-5-sonnet-20241022',
                'label': 1
            },
            'gpt4': {
                'path': 'GPT4',
                'latest_version': 'gpt-4-turbo-2024-04-09',
                'label': 2
            },
            'gpt4o': {
                'path': 'GPT4o',
                'latest_version': 'chatgpt-4o-latest',
                'label': 3
            }
        }
        self.label_to_name = {v['label']: k for k, v in self.model_configs.items()}
        
    def get_model_files(self, model_name: str, dataset_type: str = 'harmful') -> List[str]:
        """Get model data files for a specific model and dataset type."""
        config = self.model_configs[model_name]
        model_dir = os.path.join(self.data_root, config['path'])
        pattern = f"{dataset_type}_{config['latest_version']}_model.txt"
        
        files = glob.glob(os.path.join(model_dir, pattern))
        return files
    
    def load_all_model_data(self, dataset_types: List[str] = ['harmful', 'peerread', 'pubmed', 'writing', 'xsum']) -> Tuple[np.ndarray, np.ndarray]:
        """Load data from all models and dataset types."""
        all_features = []
        all_labels = []
        
        for model_name, config in self.model_configs.items():
            print(f"正在加载 {model_name} 数据...")
            model_features = []
            
            for dataset_type in dataset_types:
                files = self.get_model_files(model_name, dataset_type)
                
                for file_path in files:
                    if os.path.exists(file_path):
                        print(f"  - 处理文件: {os.path.basename(file_path)}")
                        # 使用与train_sup_cls.py相同的特征提取方法
                        features = self._extract_features_from_file(file_path)
                        model_features.append(features)
                    else:
                        print(f"  - 警告: 文件不存在 {file_path}")
            
            if model_features:
                # 合并同一模型的所有特征
                model_features_combined = np.vstack(model_features)
                all_features.append(model_features_combined)
                
                # 创建标签
                labels = np.full(model_features_combined.shape[0], config['label'])
                all_labels.append(labels)
                
                print(f"  - {model_name} 总样本数: {model_features_combined.shape[0]}")
            else:
                print(f"  - 警告: {model_name} 没有找到有效数据文件")
        
        if not all_features:
            raise ValueError("没有找到任何有效的数据文件")
        
        # 合并所有模型的特征和标签
        X = np.vstack(all_features)
        y = np.concatenate(all_labels)
        
        print(f"\n总数据统计:")
        print(f"总样本数: {X.shape[0]}")
        print(f"特征维度: {X.shape[1]}")
        for label, count in zip(*np.unique(y, return_counts=True)):
            model_name = self.label_to_name[label]
            print(f"{model_name}: {count} 个样本")
        
        return X, y
    
    def load_domain_specific_data(self, domain: str) -> Tuple[np.ndarray, np.ndarray]:
        """Load data for a specific domain from all models."""
        all_features = []
        all_labels = []
        
        print(f"正在加载 {domain} 领域数据...")
        
        for model_name, config in self.model_configs.items():
            print(f"  - 处理 {model_name}...")
            files = self.get_model_files(model_name, domain)
            
            model_features = []
            for file_path in files:
                if os.path.exists(file_path):
                    print(f"    - 处理文件: {os.path.basename(file_path)}")
                    features = self._extract_features_from_file(file_path)
                    model_features.append(features)
                else:
                    print(f"    - 警告: 文件不存在 {file_path}")
            
            if model_features:
                # 合并同一模型的所有特征
                model_features_combined = np.vstack(model_features)
                all_features.append(model_features_combined)
                
                # 创建标签
                labels = np.full(model_features_combined.shape[0], config['label'])
                all_labels.append(labels)
                
                print(f"    - {model_name} 样本数: {model_features_combined.shape[0]}")
            else:
                print(f"    - 警告: {model_name} 在 {domain} 领域没有找到有效数据文件")
        
        if not all_features:
            raise ValueError(f"在 {domain} 领域没有找到任何有效的数据文件")
        
        # 合并所有模型的特征和标签
        X = np.vstack(all_features)
        y = np.concatenate(all_labels)
        
        print(f"\n{domain} 领域数据统计:")
        print(f"总样本数: {X.shape[0]}")
        print(f"特征维度: {X.shape[1]}")
        for label, count in zip(*np.unique(y, return_counts=True)):
            model_name = self.label_to_name[label]
            print(f"{model_name}: {count} 个样本")
        
        return X, y
    
    def _extract_features_from_file(self, file_path: str, interp_len: int = 500) -> np.ndarray:
        """从单个文件提取特征，使用与train_sup_cls.py相同的方法。"""
        # 使用circular mean方法提取特征
        spectrum_data = self._get_circular_mean(file_path)
        features = self._get_features(spectrum_data, interp_len)
        return features
    
    def _get_circular_mean(self, input_file: str) -> pd.DataFrame:
        """使用circular mean方法处理NLL数据，与train_sup_cls.py保持一致。"""
        fft_processor = FFTProcessor(method='fft', preprocess='logzs', value='norm', require_sid=False)
        nlls = fft_processor._read_data(data_file=input_file)
        freqs, powers, sids = [], [], []
        
        for i, nll in enumerate(nlls):
            nll_circle = self._circular(nll)
            data = fft_processor._preprocess(nll_circle)
            freq, power, _ = fft_processor._fft_batch(data, verbose=False)
            power_mean = np.mean(power, axis=0)
            freqs.append(freq[0])
            powers.append(power_mean)
            sids.append(np.repeat(i, len(power_mean)))
        
        df = pd.DataFrame.from_dict({
            'freq': np.concatenate(freqs),
            'power': np.concatenate(powers),
            'sid': np.concatenate(sids)
        })
        return df
    
    def _circular(self, input: list, n: int = None, include_self: bool = True) -> list:
        """Circular shifting function，与train_sup_cls.py保持一致。"""
        if n is None:
            n = len(input) - 1
        output = []
        if include_self:
            output.append(input)
        for i in range(n):
            out = input[i+1:] + input[:i+1]
            output.append(out)
        return output
    
    def _get_features(self, spectrum_data: pd.DataFrame, interp_len: int = 500) -> np.ndarray:
        """从频谱数据提取特征，与train_sup_cls.py保持一致。"""
        features_interp = []
        for _, group in spectrum_data.groupby('sid'):
            freqs = group['freq'].values
            features = group['power'].values
            new_freq = np.linspace(0, 0.5, interp_len)
            new_feat = np.interp(new_freq, freqs, features)
            features_interp.append(new_feat)
        
        return np.array(features_interp)
    
    def train_ovo_classifier(self, X: np.ndarray, y: np.ndarray, test_size: float = 0.2, random_state: int = 42) -> Dict:
        """训练One-vs-One多类SVM分类器。"""
        print("\n开始训练One-vs-One多类分类器...")
        
        # 分割训练测试集
        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=test_size, random_state=random_state, stratify=y
        )
        
        print(f"训练集大小: {X_train.shape[0]}")
        print(f"测试集大小: {X_test.shape[0]}")
        
        # 定义管道
        pipeline = make_pipeline(
            StandardScaler(),
            SelectKBest(),
            OneVsOneClassifier(SVC(gamma='auto', kernel='rbf', probability=True))
        )
        
        # 超参数网格
        param_grid = {
            'selectkbest__k': [50, 100, 120, 150, 200],
            'onevsoneclassifier__estimator__C': [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]
        }
        
        # 网格搜索
        print("进行超参数调优...")
        grid_search = GridSearchCV(
            pipeline, 
            param_grid, 
            cv=5, 
            scoring='accuracy',
            n_jobs=-1,
            verbose=1
        )
        
        grid_search.fit(X_train, y_train)
        
        # 获取最佳模型
        best_estimator = grid_search.best_estimator_
        best_params = grid_search.best_params_
        best_cv_score = grid_search.best_score_
        
        # 在测试集上评估
        y_pred = best_estimator.predict(X_test)
        test_accuracy = accuracy_score(y_test, y_pred)
        
        # 交叉验证分数
        cv_scores = cross_val_score(best_estimator, X_train, y_train, cv=5, scoring='accuracy')
        
        # 详细分类报告
        class_names = [self.label_to_name[i] for i in sorted(self.label_to_name.keys())]
        classification_rep = classification_report(y_test, y_pred, target_names=class_names)
        confusion_mat = confusion_matrix(y_test, y_pred)
        
        results = {
            'best_estimator': best_estimator,
            'best_params': best_params,
            'best_cv_score': best_cv_score,
            'test_accuracy': test_accuracy,
            'cv_scores': cv_scores,
            'classification_report': classification_rep,
            'confusion_matrix': confusion_mat,
            'class_names': class_names,
            'X_test': X_test,
            'y_test': y_test,
            'y_pred': y_pred
        }
        
        return results
    
    def train_pairwise_classifiers(self, X: np.ndarray, y: np.ndarray) -> Dict:
        """训练所有pairwise分类器以进行详细分析。"""
        print("\n训练成对分类器进行详细分析...")
        
        pairwise_results = {}
        model_names = list(self.model_configs.keys())
        
        for (name1, name2) in combinations(model_names, 2):
            label1 = self.model_configs[name1]['label']
            label2 = self.model_configs[name2]['label']
            
            # 提取两个类别的数据
            mask = (y == label1) | (y == label2)
            X_pair = X[mask]
            y_pair = y[mask]
            
            # 重新标记为0和1
            y_binary = np.where(y_pair == label1, 0, 1)
            
            print(f"\n训练 {name1} vs {name2}...")
            print(f"  {name1}: {np.sum(y_binary == 0)} 个样本")
            print(f"  {name2}: {np.sum(y_binary == 1)} 个样本")
            
            # 分割数据
            X_train, X_test, y_train, y_test = train_test_split(
                X_pair, y_binary, test_size=0.2, random_state=42, stratify=y_binary
            )
            
            # 定义管道
            pipeline = make_pipeline(
                StandardScaler(),
                SelectKBest(),
                SVC(gamma='auto', kernel='rbf', probability=True)
            )
            
            # 超参数网格
            param_grid = {
                'selectkbest__k': [50, 100, 120, 150, 200],
                'svc__C': [0.1, 0.5, 1.0, 2.0, 5.0, 10.0]
            }
            
            # 网格搜索
            grid_search = GridSearchCV(
                pipeline, param_grid, cv=5, scoring='roc_auc', n_jobs=-1
            )
            
            grid_search.fit(X_train, y_train)
            
            # 评估
            best_estimator = grid_search.best_estimator_
            y_pred = best_estimator.predict(X_test)
            y_pred_proba = best_estimator.predict_proba(X_test)[:, 1]
            
            accuracy = accuracy_score(y_test, y_pred)
            auc_score = roc_auc_score(y_test, y_pred_proba)
            
            pairwise_results[f"{name1}_vs_{name2}"] = {
                'estimator': best_estimator,
                'best_params': grid_search.best_params_,
                'best_cv_score': grid_search.best_score_,
                'test_accuracy': accuracy,
                'test_auc': auc_score,
                'y_test': y_test,
                'y_pred': y_pred,
                'y_pred_proba': y_pred_proba
            }
            
            print(f"  测试准确率: {accuracy:.4f}")
            print(f"  测试AUC: {auc_score:.4f}")
        
        return pairwise_results
    
    def print_results(self, results: Dict):
        """打印训练结果。"""
        print("\n" + "="*60)
        print("多类分类器训练结果")
        print("="*60)
        
        print(f"最佳超参数: {results['best_params']}")
        print(f"最佳交叉验证准确率: {results['best_cv_score']:.4f}")
        print(f"测试集准确率: {results['test_accuracy']:.4f}")
        print(f"交叉验证分数: {results['cv_scores']}")
        print(f"平均CV准确率: {np.mean(results['cv_scores']):.4f} (+/- {np.std(results['cv_scores']) * 2:.4f})")
        
        print("\n分类报告:")
        print(results['classification_report'])
        
        print("\n混淆矩阵:")
        print("行: 真实标签, 列: 预测标签")
        confusion_df = pd.DataFrame(
            results['confusion_matrix'], 
            index=results['class_names'],
            columns=results['class_names']
        )
        print(confusion_df)
    
    def print_pairwise_results(self, pairwise_results: Dict):
        """打印成对分类器结果。"""
        print("\n" + "="*60)
        print("成对分类器结果")
        print("="*60)
        
        for pair_name, result in pairwise_results.items():
            print(f"\n{pair_name}:")
            print(f"  最佳CV AUC: {result['best_cv_score']:.4f}")
            print(f"  测试准确率: {result['test_accuracy']:.4f}")
            print(f"  测试AUC: {result['test_auc']:.4f}")
    
    def save_results(self, results: Dict, pairwise_results: Dict, output_dir: str = "multiclass", domain: str = None):
        """保存训练结果。"""
        os.makedirs(output_dir, exist_ok=True)
        
        # 根据是否指定领域来命名文件
        if domain:
            classifier_suffix = f"_{domain}"
        else:
            classifier_suffix = "_all_domains"
        
        # 保存主分类器
        main_classifier_path = os.path.join(output_dir, f"multiclass_ovo_classifier{classifier_suffix}.pkl")
        with open(main_classifier_path, 'wb') as f:
            pickle.dump(results['best_estimator'], f)
        print(f"主分类器已保存到: {main_classifier_path}")
        
        # 保存成对分类器
        if pairwise_results:
            pairwise_classifiers = {name: result['estimator'] for name, result in pairwise_results.items()}
            pairwise_path = os.path.join(output_dir, f"pairwise_classifiers{classifier_suffix}.pkl")
            with open(pairwise_path, 'wb') as f:
                pickle.dump(pairwise_classifiers, f)
            print(f"成对分类器已保存到: {pairwise_path}")
        
        # 保存详细结果
        results_summary = {
            'domain': domain if domain else 'all_domains',
            'multiclass_results': {
                'best_params': results['best_params'],
                'best_cv_score': results['best_cv_score'],
                'test_accuracy': results['test_accuracy'],
                'cv_scores': results['cv_scores'].tolist(),
                'classification_report': results['classification_report'],
                'confusion_matrix': results['confusion_matrix'].tolist(),
                'class_names': results['class_names']
            },
            'pairwise_results': {
                name: {
                    'best_params': result['best_params'],
                    'best_cv_score': result['best_cv_score'],
                    'test_accuracy': result['test_accuracy'],
                    'test_auc': result['test_auc']
                }
                for name, result in pairwise_results.items()
            } if pairwise_results else {}
        }
        
        results_path = os.path.join(output_dir, f"training_results{classifier_suffix}.pkl")
        with open(results_path, 'wb') as f:
            pickle.dump(results_summary, f)
        print(f"详细结果已保存到: {results_path}")
    
    def train_domain_specific_classifiers(self, domains: List[str], test_size: float = 0.2, random_state: int = 42, skip_pairwise: bool = False) -> Dict:
        """为每个领域训练独立的分类器。"""
        all_domain_results = {}
        
        for domain in domains:
            print(f"\n{'='*80}")
            print(f"开始训练 {domain.upper()} 领域分类器")
            print(f"{'='*80}")
            
            try:
                # 加载该领域的数据
                X, y = self.load_domain_specific_data(domain)
                
                # 检查是否有足够的类别
                unique_labels = np.unique(y)
                if len(unique_labels) < 2:
                    print(f"警告: {domain} 领域只有 {len(unique_labels)} 个类别，跳过训练")
                    continue
                
                # 训练主分类器
                results = self.train_ovo_classifier(X, y, test_size=test_size, random_state=random_state)
                
                # 训练成对分类器（如果需要）
                pairwise_results = {}
                if not skip_pairwise and len(unique_labels) > 2:
                    pairwise_results = self.train_pairwise_classifiers(X, y)
                
                # 保存结果
                domain_results = {
                    'multiclass_results': results,
                    'pairwise_results': pairwise_results,
                    'domain': domain
                }
                all_domain_results[domain] = domain_results
                
                # 打印结果
                print(f"\n{domain.upper()} 领域结果:")
                self.print_results(results)
                if pairwise_results:
                    self.print_pairwise_results(pairwise_results)
                
            except Exception as e:
                print(f"训练 {domain} 领域分类器时出错: {str(e)}")
                continue
        
        return all_domain_results
    
    def save_all_domain_results(self, all_domain_results: Dict, output_dir: str = "multiclass"):
        """保存所有领域的训练结果。"""
        for domain, domain_results in all_domain_results.items():
            self.save_results(
                domain_results['multiclass_results'],
                domain_results['pairwise_results'],
                output_dir=output_dir,
                domain=domain
            )


def main():
    parser = argparse.ArgumentParser(description="多类AI模型分类器训练")
    parser.add_argument('--data_root', type=str, default='data', help='数据根目录')
    parser.add_argument('--dataset_types', nargs='+', default=['harmful', 'peerread', 'pubmed', 'writing', 'xsum'], 
                       help='要使用的数据集类型')
    parser.add_argument('--domains', nargs='+', default=None, 
                       help='指定要训练的领域，如果不指定则为每个领域分别训练')
    parser.add_argument('--output_dir', type=str, default='multiclass', help='输出目录')
    parser.add_argument('--test_size', type=float, default=0.2, help='测试集比例')
    parser.add_argument('--random_state', type=int, default=42, help='随机种子')
    parser.add_argument('--skip_pairwise', action='store_true', help='跳过成对分类器训练')
    parser.add_argument('--train_all_domains', action='store_true', 
                       help='训练一个使用所有领域数据的分类器（而不是分别训练）')
    
    args = parser.parse_args()
    
    # 创建分类器实例
    classifier = MultiModelClassifier(data_root=args.data_root)
    
    try:
        if args.train_all_domains:
            # 训练使用所有领域数据的单个分类器
            print("开始训练使用所有领域数据的分类器...")
            X, y = classifier.load_all_model_data(dataset_types=args.dataset_types)
            
            # 训练主分类器
            results = classifier.train_ovo_classifier(X, y, test_size=args.test_size, random_state=args.random_state)
            classifier.print_results(results)
            
            # 训练成对分类器
            pairwise_results = {}
            if not args.skip_pairwise:
                pairwise_results = classifier.train_pairwise_classifiers(X, y)
                classifier.print_pairwise_results(pairwise_results)
            
            # 保存结果
            classifier.save_results(results, pairwise_results, output_dir=args.output_dir)
            print(f"\n训练完成！结果已保存到 {args.output_dir} 目录")
            
        else:
            # 为每个领域分别训练分类器
            domains_to_train = args.domains if args.domains else args.dataset_types
            print(f"开始为以下领域分别训练分类器: {domains_to_train}")
            
            # 训练领域特定分类器
            all_domain_results = classifier.train_domain_specific_classifiers(
                domains=domains_to_train,
                test_size=args.test_size,
                random_state=args.random_state,
                skip_pairwise=args.skip_pairwise
            )
            
            # 保存所有结果
            classifier.save_all_domain_results(all_domain_results, output_dir=args.output_dir)
            
            # 打印总结
            print(f"\n{'='*80}")
            print("所有领域训练完成！")
            print(f"{'='*80}")
            
            for domain, results in all_domain_results.items():
                multiclass_acc = results['multiclass_results']['test_accuracy']
                print(f"{domain.upper()} 领域测试准确率: {multiclass_acc:.4f}")
            
            print(f"\n所有结果已保存到 {args.output_dir} 目录")
        
    except Exception as e:
        print(f"训练过程中出现错误: {str(e)}")
        raise


if __name__ == '__main__':
    main()
