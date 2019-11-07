import streamlit as st
import itertools
import numpy as np

from kagglebook_practice.dataset import load_titanic
from kagglebook_practice.ch01 import *

def app():
    train, test = load_titanic()

    (train_x, train_y), test_x = preprocess_for_xgboost(train, test)
    model = fit_model_by_xgboost(train_x, train_y)
    logloss, accuracy = n_cross_validation(train_x, train_y, 4)
    st.write(f"logloss: {logloss:.4f}, accuracy: {accuracy:.4f}")

    # 探索するハイパーパラメータの組み合わせ
    param_space = {
        'max_depth': [3, 5, 7],
        'min_child_weight': [1.0, 2.0, 4.0]
    }

    param_combinations = itertools.product(
        param_space["max_depth"], param_space["min_child_weight"]
    )
    
    # 各パラメータの組み合わせ、それに対するスコアを保存するリスト
    scores = []
    
    # 各パラメータの組み合わせごとに、クロスバリデーションで評価を行う
    for max_depth, min_child_weight in param_combinations:
        score_mean, _ = n_cross_validation(
            train_x,
            train_y,
            4,
            random_state=123456,
            max_depth=max_depth,
            min_child_weight=min_child_weight,
        )
        scores.append((score_mean, (max_depth, min_child_weight)))
    
    best_score, best_param = min(scores)
    st.write(f"max_depth: {best_param[0]}, min_child_weight: {best_param[1]}")
    # max_depth=7, min_child_weight=2.0のスコアが最もよかった
    
    # -----------------------------------
    # ロジスティック回帰用の特徴量の作成
    # -----------------------------------
    (lr_train_x, lr_train_y), lr_test = preprocess_for_logistic_regression(train, test)
    
    # xgboostモデル
    model_xgb = fit_model_by_xgboost(train_x, train_y)
    pred_xgb = model_xgb.predict_proba(test_x)[:, 1]
    
    # ロジスティック回帰モデル
    # xgboostモデルとは異なる特徴量を入れる必要があるので、別途train_x2, test_x2を作成した
    model_lr = fit_model_by_lenear_regression(lr_train_x, lr_train_y)
    pred_lr = model_lr.predict_proba(lr_test)[:, 1]
    
    # 予測値の加重平均をとる
    pred = pred_xgb * 0.8 + pred_lr * 0.2
    pred_label = np.where(pred > 0.5, 1, 0)
    st.write(pred_label)
