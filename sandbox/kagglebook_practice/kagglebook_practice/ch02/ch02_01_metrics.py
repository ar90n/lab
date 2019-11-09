import streamlit as st
import numpy as np
import pandas as pd


def app():
    # -----------------------------------
    # 回帰
    # -----------------------------------
    st.markdown(f'## Regression')
    # rmse
    st.markdown(f'### RMSE')

    from sklearn.metrics import mean_squared_error

    # y_trueが真の値、y_predが予測値
    y_true = np.array([1.0, 1.5, 2.0, 1.2, 1.8])
    y_pred = np.array([0.8, 1.5, 1.8, 1.3, 3.0])

    rmse_actual = np.sqrt((y_true - y_pred) @ (y_true - y_pred) / y_true.size)
    st.markdown(f'#### Calculated by codes')
    st.write(rmse_actual)

    rmse_expect = np.sqrt(mean_squared_error(y_true, y_pred))
    st.markdown(f'#### Calculated by sklearn')
    st.write(rmse_expect)

    # -----------------------------------
    # 二値分類
    # -----------------------------------
    st.markdown(f'## Binary classification')

    # 0, 1で表される二値分類の真の値と予測値
    y_true = np.array([1, 0, 1, 1, 0, 1, 1, 0])
    y_pred = np.array([0, 0, 1, 1, 0, 0, 1, 1])
    y_prob = np.array([0.1, 0.2, 0.8, 0.8, 0.1, 0.3, 0.1, 0.9])

    # 混同行列
    st.markdown(f'### Confusion Matrix')

    from sklearn.metrics import confusion_matrix

    tp = np.sum((y_true == 1) & (y_pred == 1))
    tn = np.sum((y_true == 0) & (y_pred == 0))
    fp = np.sum((y_true == 0) & (y_pred == 1))
    fn = np.sum((y_true == 1) & (y_pred == 0))
    confusion_matrix_actual = np.array([[tp, fp], [fn, tn]])
    st.markdown(f'#### Calculated by codes')
    st.write(confusion_matrix_actual)

    confusion_matrix_expect = confusion_matrix(y_true, y_pred)
    st.markdown(f'#### Calculated by sklearn')
    st.write(confusion_matrix_expect)

    # accuracy
    st.markdown(f'### Accuracy')

    from sklearn.metrics import accuracy_score

    accuracy_actual = (tp + tn) / (tp + fp + fn + tn)
    st.markdown(f'#### Calculated by codes')
    st.write(accuracy_actual)

    accuracy_expect = accuracy_score(y_true, y_pred)
    st.markdown(f'#### Calculated by sklearn')
    st.write(accuracy_expect)

    # precision
    st.markdown(f'### Precision')

    precision_actual = tp / (tp + fp)
    st.markdown(f'#### Calculated by codes')
    st.write(precision_actual)

    # recall
    st.markdown(f'### Recall')

    recall_actual = tp / (tp + fn)
    st.markdown(f'#### Calculated by codes')
    st.write(recall_actual)

    # f1 score
    st.markdown(f'### F1 score')

    from sklearn.metrics import f1_score

    f1_score_actual = 2 * tp / (2 * tp + fn + fp)
    st.markdown(f'#### Calculated by codes')
    st.write(f1_score_actual)

    f1_score_expect = f1_score(y_true, y_pred)
    st.markdown(f'#### Calculated by sklearn')
    st.write(f1_score_expect)


    # logloss
    st.markdown(f'### logloss')

    from sklearn.metrics import log_loss

    logloss_actual = -(y_true @ np.log(y_prob) + (1 - y_true) @ np.log(1 - y_prob)) / y_true.size
    st.markdown(f'#### Calculated by codes')
    st.write(logloss_actual)

    logloss_expect = log_loss(y_true, y_prob)
    st.markdown(f'#### Calculated by sklearn')
    st.write(logloss_expect)

    # -----------------------------------
    # マルチクラス分類
    # -----------------------------------
    st.markdown(f'## Multi class classification')

    # 3クラス分類の真の値と予測値
    y_true = np.array([0, 2, 1, 2, 2])
    y_pred = np.array([[0.68, 0.32, 0.00],
                       [0.00, 0.00, 1.00],
                       [0.60, 0.40, 0.00],
                       [0.00, 0.00, 1.00],
                       [0.28, 0.12, 0.60]])

    # multi-class logloss
    st.markdown(f'### multi-class logloss')

    from sklearn.metrics import log_loss

    mc_logloss_actual = - np.sum(np.log(y_pred[np.arange(y_true.size), y_true])) / y_true.size
    st.markdown(f'#### Calculated by codes')
    st.write(mc_logloss_actual)

    mc_logloss_expect = log_loss(y_true, y_pred)
    st.markdown(f'#### Calculated by sklearn')
    st.write(mc_logloss_expect)

    # -----------------------------------
    # マルチラベル分類
    # -----------------------------------
    st.markdown(f'## Multi label classification')

    # マルチラベル分類の真の値・予測値は、評価指標の計算上はレコード×クラスの二値の行列とした方が扱いやすい
    # 真の値 - [[1,2], [1], [1,2,3], [2,3], [3]]
    y_true = np.array([[1, 1, 0],
                       [1, 0, 0],
                       [1, 1, 1],
                       [0, 1, 1],
                       [0, 0, 1]])

    # 予測値 - [[1,3], [2], [1,3], [3], [3]]
    y_pred = np.array([[1, 0, 1],
                       [0, 1, 0],
                       [1, 0, 1],
                       [0, 0, 1],
                       [0, 0, 1]])

    # mean_f1
    st.markdown(f'### mean f1 score')

    from sklearn.metrics import f1_score

    tp = np.sum((y_true == 1) & (y_pred == 1), axis=1)
    fp = np.sum((y_true == 0) & (y_pred == 1), axis=1)
    fn = np.sum((y_true == 1) & (y_pred == 0), axis=1)
    mean_f1_actual = np.mean(2 * tp / (2 * tp + fp + fn))
    st.markdown(f'#### Calculated by codes')
    st.write(mean_f1_actual)

    mean_f1_expect = f1_score(y_true, y_pred, average='samples')
    st.markdown(f'#### Calculated by sklearn')
    st.write(mean_f1_expect)

    # macro_f1
    st.markdown(f'### macro f1 score')

    tp = np.sum((y_true == 1) & (y_pred == 1), axis=0)
    fp = np.sum((y_true == 0) & (y_pred == 1), axis=0)
    fn = np.sum((y_true == 1) & (y_pred == 0), axis=0)
    macro_f1_actual = np.mean(2 * tp / (2 * tp + fp + fn))
    st.markdown(f'#### Calculated by codes')
    st.write(macro_f1_actual)

    macro_f1_expect = f1_score(y_true, y_pred, average='macro')
    st.markdown(f'#### Calculated by sklearn')
    st.write(macro_f1_expect)

    # micro_f1
    st.markdown(f'### micro f1 score')

    tp = np.sum((y_true == 1) & (y_pred == 1))
    fp = np.sum((y_true == 0) & (y_pred == 1))
    fn = np.sum((y_true == 1) & (y_pred == 0))
    micro_f1_actual = np.mean(2 * tp / (2 * tp + fp + fn))
    st.markdown(f'#### Calculated by codes')
    st.write(micro_f1_actual)

    micro_f1_expect = f1_score(y_true, y_pred, average='micro')
    st.markdown(f'#### Calculated by sklearn')
    st.write(micro_f1_expect)

    # -----------------------------------
    # クラス間に順序関係があるマルチクラス分類
    # -----------------------------------
    st.markdown(f'## Ordering multi class classification')
    # quadratic weighted kappa
    st.markdown(f'### quadratic weighted kappa')

    from sklearn.metrics import confusion_matrix, cohen_kappa_score

    # quadratic weighted kappaを計算する関数
    def quadratic_weighted_kappa(c_matrix):
        numer = 0.0
        denom = 0.0

        for i in range(c_matrix.shape[0]):
            for j in range(c_matrix.shape[1]):
                n = c_matrix.shape[0]
                wij = ((i - j) ** 2.0)
                oij = c_matrix[i, j]
                eij = c_matrix[i, :].sum() * c_matrix[:, j].sum() / c_matrix.sum()
                numer += wij * oij
                denom += wij * eij

        return 1.0 - numer / denom

    # y_true は真の値のクラスのリスト、y_pred は予測値のクラスのリスト
    y_true = [1, 2, 3, 4, 3]
    y_pred = [2, 2, 4, 4, 5]

    # 混同行列を計算する
    c_matrix = confusion_matrix(y_true, y_pred, labels=[1, 2, 3, 4, 5])

    # quadratic weighted kappaを計算する
    kappa_actual = quadratic_weighted_kappa(c_matrix)
    st.markdown(f'#### Calculated by codes')
    st.write(kappa_actual)

    # scikit-learnのメソッドを使うことでも計算できる
    kappa_expect = cohen_kappa_score(y_true, y_pred, weights='quadratic')
    st.markdown(f'#### Calculated by sklearn')
    st.write(kappa_expect)

    # -----------------------------------
    # レコメンデーション
    # -----------------------------------
    st.markdown(f'## Recomendation')
    # MAP@K
    st.markdown(f'### MAP@K')

    # K=3、レコード数は5個、クラスは4種類とする
    K = 3

    # 各レコードの真の値
    y_true = [[1, 2], [1, 2], [4], [1, 2, 3, 4], [3, 4]]

    # 各レコードに対する予測値 - K=3なので、通常は各レコードにそれぞれ3個まで順位をつけて予測する
    y_pred = [[1, 2, 4], [4, 1, 2], [1, 4, 3], [1, 2, 3], [1, 2, 4]]


    # 各レコードごとのaverage precisionを計算する関数
    def apk(y_i_true, y_i_pred):
        # y_predがK以下の長さで、要素がすべて異なることが必要
        assert (len(y_i_pred) <= K)
        assert (len(np.unique(y_i_pred)) == len(y_i_pred))

        sum_precision = 0.0
        num_hits = 0.0

        for i, p in enumerate(y_i_pred):
            if p in y_i_true:
                num_hits += 1
                precision = num_hits / (i + 1)
                sum_precision += precision

        return sum_precision / min(len(y_i_true), K)


    # MAP@K を計算する関数
    def mapk(y_true, y_pred):
        return np.mean([apk(y_i_true, y_i_pred) for y_i_true, y_i_pred in zip(y_true, y_pred)])

    # MAP@Kを求める
    st.write(mapk(y_true, y_pred))
    # 0.65

    # 正解の数が同じでも、順序が違うとスコアも異なる
    st.write(apk(y_true[0], y_pred[0]))
    st.write(apk(y_true[1], y_pred[1]))
