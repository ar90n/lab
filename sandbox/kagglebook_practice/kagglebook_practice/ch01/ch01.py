import itertools
from collections import OrderedDict

import numpy as np
import pandas as pd
import streamlit as st
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, log_loss
from sklearn.model_selection import KFold
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
from xgboost import XGBClassifier

from kagglebook_practice.ch01 import *
from kagglebook_practice.dataset import load_titanic


def preprocess_for_xgboost(train, test):
    train_x = train.drop(["Survived", "PassengerId", "Name", "Ticket", "Cabin"], axis=1)
    train_y = train["Survived"]

    test_x = test.drop(["PassengerId", "Name", "Ticket", "Cabin"], axis=1)

    for c in ["Sex", "Embarked"]:
        le = LabelEncoder()
        le.fit(train_x[c].fillna("NA"))

        train_x[c] = le.transform(train_x[c].fillna("NA"))
        test_x[c] = le.transform(test_x[c].fillna("NA"))

    return (train_x, train_y), test_x


def fit_model_by_xgboost(train_x, train_y, **model_args):
    model = XGBClassifier(mn_estimators=20, random_state=71, **model_args)
    model.fit(train_x, train_y)
    return model


def fit_model_by_lenear_regression(train_x, train_y, **model_args):
    model = LogisticRegression(solver="lbfgs", max_iter=300)
    model.fit(train_x, train_y)
    return model


def n_cross_validation(train_x, train_y, n, random_state=71, **model_args):
    scores_accuracy = []
    scores_logloss = []

    kf = KFold(n_splits=n, shuffle=True, random_state=random_state)
    for train_indice, validate_indice in kf.split(train_x):
        tr_x, va_x = train_x.iloc[train_indice], train_x.iloc[validate_indice]
        tr_y, va_y = train_y.iloc[train_indice], train_y.iloc[validate_indice]

        model = fit_model_by_xgboost(tr_x, tr_y, **model_args)

        va_pred = model.predict_proba(va_x)[:, 1]

        logloss = log_loss(va_y, va_pred)
        scores_logloss.append(logloss)

        accuracy = accuracy_score(va_y, va_pred > 0.5)
        scores_accuracy.append(accuracy)

    return np.mean(scores_logloss), np.mean(scores_accuracy)


def preprocess_for_logistic_regression(train, test):
    train_x = train.drop(["Survived", "PassengerId", "Name", "Ticket", "Cabin"], axis=1)
    train_y = train["Survived"]
    test_x = test.drop(["PassengerId", "Name", "Ticket", "Cabin"], axis=1)

    cat_cols = ["Sex", "Embarked", "Pclass"]
    ohe = OneHotEncoder(categories="auto", sparse=False)
    ohe.fit(train_x[cat_cols].fillna("NA"))

    # one-hot encodingのダミー変数の列名を作成する
    ohe_columns = []
    for i, c in enumerate(cat_cols):
        ohe_columns += [f"{c}_{v}" for v in ohe.categories_[i]]

    # one-hot encodingによる変換を行う
    ohe_train_x = pd.DataFrame(
        ohe.transform(train_x[cat_cols].fillna("NA")), columns=ohe_columns
    )
    ohe_test_x = pd.DataFrame(
        ohe.transform(test_x[cat_cols].fillna("NA")), columns=ohe_columns
    )

    # one-hot encoding済みの変数を除外する
    train_x = train_x.drop(cat_cols, axis=1)
    test_x = test_x.drop(cat_cols, axis=1)

    # one-hot encodingで変換された変数を結合する
    train_x = pd.concat([train_x, ohe_train_x], axis=1)
    test_x = pd.concat([test_x, ohe_test_x], axis=1)

    # 数値変数の欠損値を学習データの平均で埋める
    num_cols = ["Age", "SibSp", "Parch", "Fare"]
    for col in num_cols:
        train_x[col].fillna(train_x[col].mean(), inplace=True)
        test_x[col].fillna(train_x[col].mean(), inplace=True)

    # 変数Fareを対数変換する
    train_x["Fare"] = np.log1p(train_x["Fare"])
    test_x["Fare"] = np.log1p(test_x["Fare"])

    return (train_x, train_y), test_x


def app():
    train, test = load_titanic()

    (train_x, train_y), test_x = preprocess_for_xgboost(train, test)
    model = fit_model_by_xgboost(train_x, train_y)
    logloss, accuracy = n_cross_validation(train_x, train_y, 4)
    st.write(f"logloss: {logloss:.4f}, accuracy: {accuracy:.4f}")

    # 探索するハイパーパラメータの組み合わせ
    param_space = {"max_depth": [3, 5, 7], "min_child_weight": [1.0, 2.0, 4.0]}

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
