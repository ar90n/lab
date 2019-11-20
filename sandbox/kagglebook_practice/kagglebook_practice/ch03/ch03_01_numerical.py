import streamlit as st
import numpy as np
import pandas as pd

from ..dataset import load_sample


def load_data():
    train, test_x = load_sample()
    train_x = train.drop(['target'], axis=1)
    return train_x, test_x


def app():
    # 変換する数値変数をリストに格納
    num_cols = ['age', 'height', 'weight', 'amount',
                'medical_info_a1', 'medical_info_a2', 'medical_info_a3', 'medical_info_b1']

    # -----------------------------------
    # 標準化
    # -----------------------------------
    st.markdown('## Normalization')
    # -----------------------------------
    st.markdown('### using train samples')
    from sklearn.preprocessing import StandardScaler

    st.markdown(f'#### Calculated by codes')
    train_x, test_x = load_data()

    feature_train_x = train_x[num_cols].agg(['mean', 'std'])
    train_x[num_cols] = (train_x[num_cols] - feature_train_x.loc['mean']) / feature_train_x.loc['std']
    test_x[num_cols] = (test_x[num_cols] - feature_train_x.loc['mean']) / feature_train_x.loc['std']
    st.write(train_x, test_x)

    st.markdown(f'#### Calculated by sklearn')
    train_x, test_x = load_data()
    # 学習データに基づいて複数列の標準化を定義
    scaler = StandardScaler()
    scaler.fit(train_x[num_cols])

    # 変換後のデータで各列を置換
    train_x[num_cols] = scaler.transform(train_x[num_cols])
    test_x[num_cols] = scaler.transform(test_x[num_cols])
    st.write(train_x, test_x)

    st.markdown('### using all samples')

    st.markdown(f'#### Calculated by codes')
    train_x, test_x = load_data()
    feature_concat = pd.concat([train_x[num_cols], test_x[num_cols]]).agg(['mean', 'std'])
    train_x[num_cols] = (train_x[num_cols] - feature_concat.loc['mean']) / feature_concat.loc['std']
    test_x[num_cols] = (test_x[num_cols] - feature_concat.loc['mean']) / feature_concat.loc['std']
    st.write(train_x, test_x)

    st.markdown(f'#### Calculated by sklearn')
    train_x, test_x = load_data()
    # 学習データとテストデータを結合したものに基づいて複数列の標準化を定義
    scaler = StandardScaler()
    scaler.fit(pd.concat([train_x[num_cols], test_x[num_cols]]))

    # 変換後のデータで各列を置換
    train_x[num_cols] = scaler.transform(train_x[num_cols])
    test_x[num_cols] = scaler.transform(test_x[num_cols])
    st.write(train_x, test_x)

    # -----------------------------------
    # Min-Maxスケーリング
    # -----------------------------------
    st.markdown('## min-max scaling')
    from sklearn.preprocessing import MinMaxScaler

    st.markdown(f'#### Calculated by codes')
    train_x, test_x = load_data()

    min_max_feature = train_x[num_cols].agg(['min', 'max'])
    train_x[num_cols] = (train_x[num_cols] - min_max_feature.loc['min']) / (min_max_feature.loc['max'] - min_max_feature.loc['min'])
    test_x[num_cols] = (test_x[num_cols] - min_max_feature.loc['min']) / (min_max_feature.loc['max'] - min_max_feature.loc['min'])
    st.write(train_x, test_x)


    st.markdown(f'#### Calculated by sklearn')
    train_x, test_x = load_data()

    # 学習データに基づいて複数列のMin-Maxスケーリングを定義
    scaler = MinMaxScaler()
    scaler.fit(train_x[num_cols])

    # 変換後のデータで各列を置換
    train_x[num_cols] = scaler.transform(train_x[num_cols])
    test_x[num_cols] = scaler.transform(test_x[num_cols])
    st.write(train_x, test_x)

    # -----------------------------------
    # 対数変換
    # -----------------------------------
    st.markdown('## log scaling')
    x = np.array([1.0, 10.0, 100.0, 1000.0, 10000.0])

    # 単に対数をとる
    x1 = np.log(x)

    # 1を加えたあとに対数をとる
    x2 = np.log1p(x)

    # 絶対値の対数をとってから元の符号を付加する
    x3 = np.sign(x) * np.log(np.abs(x))
    st.write(f'x1:{x1}')
    st.write(f'x2:{x2}')
    st.write(f'x3:{x3}')

    # -----------------------------------
    # Box-Cox変換
    # -----------------------------------
    st.markdown('## Box-Cox transform')

    st.markdown(f'#### Calculated by sklearn')
    train_x, test_x = load_data()
    # 正の値のみをとる変数を変換対象としてリストに格納する
    # なお、欠損値も含める場合は、(~(train_x[c] <= 0.0)).all() などとする必要があるので注意
    pos_cols = [c for c in num_cols if (train_x[c] > 0.0).all() and (test_x[c] > 0.0).all()]

    from sklearn.preprocessing import PowerTransformer

    # 学習データに基づいて複数列のBox-Cox変換を定義
    pt = PowerTransformer(method='box-cox')
    pt.fit(train_x[pos_cols])

    # 変換後のデータで各列を置換
    train_x[pos_cols] = pt.transform(train_x[pos_cols])
    test_x[pos_cols] = pt.transform(test_x[pos_cols])
    st.write(train_x, test_x)

    # -----------------------------------
    # Yeo-Johnson変換
    # -----------------------------------
    st.markdown('## Yeo-Johnson transform')

    from sklearn.preprocessing import PowerTransformer

    st.markdown(f'#### Calculated by sklearn')
    train_x, test_x = load_data()

    # 学習データに基づいて複数列のYeo-Johnson変換を定義
    pt = PowerTransformer(method='yeo-johnson')
    pt.fit(train_x[num_cols])

    # 変換後のデータで各列を置換
    train_x[num_cols] = pt.transform(train_x[num_cols])
    test_x[num_cols] = pt.transform(test_x[num_cols])
    st.write(train_x, test_x)

    # -----------------------------------
    # clipping
    # -----------------------------------
    st.markdown('## clipping')
    train_x, test_x = load_data()
    # -----------------------------------
    # 列ごとに学習データの1％点、99％点を計算
    p01 = train_x[num_cols].quantile(0.01)
    p99 = train_x[num_cols].quantile(0.99)
    
    # 1％点以下の値は1％点に、99％点以上の値は99％点にclippingする
    train_x[num_cols] = train_x[num_cols].clip(p01, p99, axis=1)
    test_x[num_cols] = test_x[num_cols].clip(p01, p99, axis=1)

    # -----------------------------------
    # binning
    # -----------------------------------
    st.markdown('## binning')
    x = [1, 7, 5, 4, 6, 3]

    # pandasのcut関数でbinningを行う

    # binの数を指定する場合
    binned = pd.cut(x, 3, labels=False)
    st.write(binned)
    # [0 2 1 1 2 0] - 変換された値は3つのbinのどれに入ったかを表す

    # binの範囲を指定する場合（3.0以下、3.0より大きく5.0以下、5.0より大きい）
    bin_edges = [-float('inf'), 3.0, 5.0, float('inf')]
    binned = pd.cut(x, bin_edges, labels=False)
    st.write(binned)
    # [0 2 1 1 2 0] - 変換された値は3つのbinのどれに入ったかを表す

    # -----------------------------------
    # 順位への変換
    # -----------------------------------
    st.markdown('## order')
    x = [10, 20, 30, 0, 40, 40]

    # pandasのrank関数で順位に変換する
    rank = pd.Series(x).rank()
    st.write(rank.values)
    # はじまりが1、同順位があった場合は平均の順位となる
    # [2. 3. 4. 1. 5.5 5.5]
    
    # numpyのargsort関数を2回適用する方法で順位に変換する
    order = np.argsort(x)
    rank = np.argsort(order)
    st.write(rank)
    # はじまりが0、同順位があった場合はどちらかが上位となる
    # [1 2 3 0 4 5]
    
    # -----------------------------------
    # RankGauss
    # -----------------------------------
    st.markdown('## RankGauss')
    # データの読み込み
    train_x, test_x = load_data()
    # -----------------------------------
    from sklearn.preprocessing import QuantileTransformer
    
    # 学習データに基づいて複数列のRankGaussによる変換を定義
    transformer = QuantileTransformer(n_quantiles=100, random_state=0, output_distribution='normal')
    transformer.fit(train_x[num_cols])
    
    # 変換後のデータで各列を置換
    train_x[num_cols] = transformer.transform(train_x[num_cols])
    test_x[num_cols] = transformer.transform(test_x[num_cols])
    st.write(train_x, test_x)
