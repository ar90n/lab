---
title: Python for RESAS-API
date: "2017-01-12T02:45:50.564Z"
layout: post
path: "/2017-01-12T02:45:50.564Z/"
category: "Python"
description: "PythonからRESAS-APIにアクセスするライブラリ(resaspy)を作ってみました．"
---

データ解析なんかを考えると，やはりPythonでからアクセスできると都合が良いので，[resaspy](https://github.com/ar90n/resaspy)というライブラリを作成してみました．

### RESAS-APIとは？

RESASとは，地方創生事業の一環で行政に関するデータを集約・開示する内閣府の取り組みのようです．  
RESAS-APIとはこのRESASのシステムに外部のサービスがアクセスするためのAPIです．

### resaspyを使ってみる

[Github](https://github.com/ar90n/resaspy)にも書いてありますが，pipを使うと簡単に導入ができます．
```bash
pip install resasy
```

動作確認は以下のサンプルを実行することで可能です．  
RESAS-APIを使用するためには，事前にAPI-KEYを取得する必要があります．API-KEYの取得は[こちら](https://opendata.resas-portal.go.jp/)から可能です．

```python
>>> import os
>>> from resaspy import Resaspy
>>> resas_key = os.environ['RESAS_API_KEY']
>>> resaspy = Resaspy( resas_key )
>>> resaspy.prefectures()
{'result': [{'prefName': '北海道', 'prefCode': 1}, {'prefName': '青森県', 'prefCode': 2}, {'prefName': '岩手県', 'prefCode': 3}, {'prefName': '宮城県', 'prefCode': 4}, {'prefName': '秋田県', 'prefCode': 5}, {'prefName': '山形県', 'prefCode': 6}, {'prefName': '福島県', 'prefCode': 7}, {'prefName': '茨城県', 'prefCode': 8}, {'prefName': '栃木県', 'prefCode': 9}, {'prefName': '群馬県', 'prefCode': 10}, {'prefName': '埼玉県', 'prefCode': 11}, {'prefName': '千葉県', 'prefCode': 12}, {'prefName': '東京都', 'prefCode': 13}, {'prefName': '神奈川県', 'prefCode': 14}, {'prefName': '新潟県', 'prefCode': 15}, {'prefName': '富山県', 'prefCode': 16}, {'prefName': '石川県', 'prefCode': 17}, {'prefName': '福井県', 'prefCode': 18}, {'prefName': '山梨県', 'prefCode': 19}, {'prefName': '長野県', 'prefCode': 20}, {'prefName': '岐阜県', 'prefCode': 21}, {'prefName': '静岡県', 'prefCode': 22}, {'prefName': '愛知県', 'prefCode': 23}, {'prefName': '三重県', 'prefCode': 24}, {'prefName': '滋賀県', 'prefCode': 25}, {'prefName': '京都府', 'prefCode': 26}, {'prefName': '大阪府', 'prefCode': 27}, {'prefName': '兵庫県', 'prefCode': 28}, {'prefName': '奈良県', 'prefCode': 29}, {'prefName': '和歌山県','prefCode': 30}, {'prefName': '鳥取県', 'prefCode': 31}, {'prefName': '島根県', 'prefCode': 32}, {'prefName': '岡山県', 'prefCode': 33}, {'prefName': '広島県', 'prefCode': 34}, {'prefName': '山口県', 'prefCode': 35}, {'prefName': '徳島県', 'prefCode': 36}, {'prefName': '香川県', 'prefCode': 37}, {'prefName': '愛媛県', 'prefCode': 38}, {'prefName': '高知県', 'prefCode': 39}, {'prefName': '福岡県', 'prefCode': 40}, {'prefName': '佐賀県', 'prefCode': 41}, {'prefName': '長崎県', 'prefCode': 42}, {'prefName': '熊本県', 'prefCode': 43}, {'prefName': '大分県', 'prefCode': 44}, {'prefName': '宮崎県', 'prefCode': 45}, {'prefName': '鹿児島県', 'prefCode': 46}, {'prefName': '沖縄県', 'prefCode': 47}], 'message': None}
```
