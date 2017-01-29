#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#derived from http://codecrafthouse.jp/machine-learning/decision-tree
import numpy as np
import sklearn
from sklearn import datasets
from matplotlib import pyplot as plt

class _Node:
    def __init__(self):
        """初期化処理
        left       : 左の子ノード（しきい値未満）
        right      : 右の子ノード（しきい値以上）
        feature    : 分割する特徴番号
        threshold  : 分割するしきい値
        label      : 割り当てられたクラス番号
        numdata    : 割り当てられたデータ数
        gini_index : 分割指数（Giniインデックス）
        """

        self.left = None
        self.right = None
        self.feature = None
        self.threshold = None
        self.label = None
        self.numdata = None
        self.gini_index = None

    def build(self, data, target):
        """木の構築を行う
        data   : ノードに与えられたデータ
        target : データの分類クラス 
        """

        self.numdata = data.shape[0]
        num_features = data.shape[1]

        # 全データが同一クラスとなったら分割終了
        if len(np.unique(target)) == 1:
            self.label = target[0]
            return

        # 自分のクラスを設定(各データの多数決)
        class_cnt = {i: len(target[target==i]) for i in np.unique(target)}
        self.label= max(class_cnt.items(), key=lambda x:x[1])[0]

        # 最良の分割を記憶する変数
        best_gini_index = 0.0 # 不純度変化なし
        best_feature = None
        best_threshold = None

        # 自分の不純度は先に計算しておく
        gini = self.gini_func(target)

        for f in range(num_features):
            # 分割候補の計算
            data_f = np.unique(data[:, f]) # f番目の特徴量（重複排除）
            points = (data_f[:-1] + data_f[1:]) / 2.0 # 中間の値を計算

            # 各分割を試す
            for threshold in points:
                # しきい値で2グループに分割
                target_l = target[data[:, f] <  threshold] 
                target_r = target[data[:, f] >= threshold]

                # 分割後の不純度からGiniインデックスを計算
                gini_l = self.gini_func(target_l)
                gini_r = self.gini_func(target_r)
                pl = float(target_l.shape[0]) / self.numdata
                pr = float(target_r.shape[0]) / self.numdata
                gini_index = gini - (pl * gini_l + pr * gini_r)

                # より良い分割であれば記憶しておく
                if gini_index > best_gini_index:
                    best_gini_index = gini_index
                    best_feature = f
                    best_threshold = threshold

        # 不純度が減らなければ終了
        if best_gini_index == 0:
            return

        # 最良の分割を保持する
        self.feature = best_feature
        self.gini_index = best_gini_index
        self.threshold = best_threshold

        # 左右の子を作って再帰的に分割させる
        data_l   =   data[data[:, self.feature] <  self.threshold]
        target_l = target[data[:, self.feature] <  self.threshold]
        self.left = _Node()
        self.left.build(data_l, target_l)

        data_r   =   data[data[:, self.feature] >= self.threshold]
        target_r = target[data[:, self.feature] >= self.threshold]
        self.right = _Node()
        self.right.build(data_r, target_r)

    def gini_func(self, target):
        """Gini関数の計算

        target : 各データの分類クラス
        """
        classes = np.unique(target)
        numdata = target.shape[0]

        # Gini関数本体
        gini = 1.0
        for c in classes:
            gini -= (len(target[target == c]) / numdata) ** 2.0

        return gini

    def prune(self, criterion, numall):
        """木の剪定を行う

        criterion  : 剪定条件（この数以下は剪定対象）
        numall    : 全ノード数 
        """

        #自分が葉ノードであれば終了
        if self.feature == None:
            return

        # 子ノードの剪定
        self.left.prune(criterion, numall)
        self.right.prune(criterion, numall)

        # 子ノードが両方葉であれば剪定チェック
        if self.left.feature == None and self.right.feature == None:
            # 分割の貢献度：GiniIndex * (データ数の割合)
            result = self.gini_index * float(self.numdata) / numall

            # 貢献度が条件に満たなければ剪定する
            if result < criterion:
                self.feature = None
                self.left = None
                self.right = None

    def predict(self, d):
        """入力データ（単一）の分類先クラスを返す"""

        # 自分が節の場合は条件判定
        if self.feature != None:
            if d[self.feature] < self.threshold:
                return self.left.predict(d)
            else:
                return self.right.predict(d)

        # 自分が葉の場合は自分の分類クラスを返す
        else:
            return self.label

    def print_tree(self, depth, TF):
        """分類条件を出力する"""
        head = "    " * depth + TF + " -> "

        # 節の場合
        if self.feature != None:
            print( head + str(self.feature) + " < " + str(self.threshold) + "?" )
            self.left.print_tree(depth + 1, "T")
            self.right.print_tree(depth + 1, "F")

        # 葉の場合
        else:
            print( head + "{" + str(self.label) + ": " + str(self.numdata) + "}" )

class DecisionTree:
    """CARTによる分類木学習器"""

    def __init__(self, criterion=0.1):
        """初期化処理

        root : 決定木のルートノード
        criterion : 剪定の条件
            (a) criterion(大) -> 木が浅くなる
            (b) criterion(小) -> 木が深くなる
        """
        self.root = None
        self.criterion = criterion

    def fit(self, data, target):
        """学習を行い決定木を構築する

        data   : 学習データ
        target : 各データの分類クラス 
        """
        self.root = _Node()
        self.root.build(data, target)
        self.root.prune(self.criterion, self.root.numdata)
        pass

    def predict(self, data):
        """分類クラスの予測を行う

        data : テストデータ
        """
        ans = []
        for d in data:
            ans.append(self.root.predict(d))
        return np.array(ans)

    def print_tree(self):
        """分類木の情報を表示する"""
        self.root.print_tree(0, " ")

def main():
    train_data, train_class = sklearn.datasets.make_classification(n_features=2, n_redundant=0, n_informative=2, n_clusters_per_class=1)

    decision_tree = DecisionTree()
    decision_tree.fit( train_data, train_class )

    xmin = min( train_data[:,0])
    ymin = min( train_data[:,1])
    xmax = max( train_data[:,0] )
    ymax = max( train_data[:,1] )
    x = np.linspace( xmin, xmax, 128 )
    y = np.linspace( ymin, ymax, 128 )
    xv,yv = np.meshgrid( x, y )

    n = xv.shape[0] * xv.shape[1]
    test_data = np.hstack( ( xv.reshape(n,1), yv.reshape(n,1) ) )
    result = decision_tree.predict( test_data ).reshape( xv.shape, order='C')

    plt.imshow( result, extent=( xmin, xmax, ymax, ymin ) )
    plt.scatter( train_data[:,0], train_data[:,1], c=train_class)
    plt.show()
    return

if __name__ == '__main__':
    main()
