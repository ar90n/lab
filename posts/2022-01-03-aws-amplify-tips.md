---
toc: true
layout: post
date: 2022-01-03
categories: [AWS, Web, クラウド, Amplify]
title: AWS Amplifyのメモ
---

## はじめに
AWS Amplifyを使ってみて個人的にハマったことのメモです．

## やったこと
* コンテナ（Fargate）に対して，環境に応じた環境変数を設定する
* ロールに対して任意のポリシーを追加する
* コンテナが配置されたVPCにリソースを配置する

## コンテナ（Fargate）に対して，環境に応じた環境変数を設定する
AWS AmplifyではAPIを実装するリソースとしてコンテナ（Fargate）がサポートされています．ですが，現状ではデプロイ環境（`dev`や`prod`など）に応じた環境変数を設定することができないようです．（少なくとも，私には見つけることができませんでした）

最初は，CloudFormationの定義ファイルを手動で書き換えることで実現できると考えていました．ですが，実際に作業を進めるとこのファイルは`amplify push`のたびに`docker-compose.yaml`から生成されることが判明しました．そこで，

* `dev`環境向けに`docker-compose.yaml.dev`を，`prod`環境向けに`docker-compose.yaml.prod`をそれぞれ作成
* [コマンドフック](https://docs.amplify.aws/cli/project/command-hooks/)を利用して，`amplify push`のタイミングで`docker-compose.yaml.dev`または`docker-compose.yaml.prod`を`docker-compose.yaml`にコピー

という対応を採用しました．


## ロールに対して任意のポリシーを追加する
カスタムリソースを追加した場合など，Lambdaに任意のポリシーを追加したい場合があると思います．そのような場合は，`custom-policies.json`を利用するのが楽です．
一応，[ここ](https://aws.amazon.com/jp/blogs/news/extend-amplify-backend-with-custom-aws-resource-using-aws-cdk-or-cloudformation/)や[ここ](https://docs.amplify.aws/cli/usage/containers/#custom-policy-file-structure)など，AWSの公式ドキュメントにも記載されているようですが，目立つところには記載されていないため気がつくまでに時間がかかりました．

## コンテナが配置されたVPCにリソースを配置する
コンテナの配置されるVPCは, `amplify/backend/backend-config.json`中に記述されている`NetworkStack`というリソースによって作成されれます．`NetworkStack`は

* CloudMapNamespaceId
* ClusterName
* Igw
* SubnetIds
* VpcCidrBlock
* VpcId
* VpcLinkId

を出力します．必要なパラメータはリソースによって異なると思いますが，`SubnetIds`や`VpcId`などを参照することでコンテナと同じVPC内にリソースを配置することができると思います．

上記パラメータを参照するためには，

1.  `amplify/backend/backend-config.json`において，上記パラメータを参照するリソースの`dependsOn`に`NetworkStack`を追加
    ```json
    ...

      "dependsOn": [
        {
          "category": "",
          "resourceName": "NetworkStack",
          "attributes": [
            "VpcId",
            "SubnetIds"
          ]
        }
      ]

    ...
    ```
2.  上述のリソースのCloudFormationの設定ファイルにパラメータの設定を追加.この際，パラメータ名は`NetworkStack<Attribute名>`となります
    ```json
    ...

    "Parameters": {
      ...

      "NetworkStackVpcId": {
        "Type": "String"
      },
      "NetworkStackSubnetIds": {
        "Type": "CommaDelimitedList"
      },

      ...
    }, 
    ...
    ```
3.  `amplify env checkout <env>`を実行して環境のチェックアウトを行う

## 参考
* [AWS CDKまたはCloudFormationを使用し、カスタムAWSリソースでAmplifyバックエンドを拡張する新機能「カスタム」のご紹介](https://aws.amazon.com/jp/blogs/news/extend-amplify-backend-with-custom-aws-resource-using-aws-cdk-or-cloudformation/)
* [Amplify Docs](https://docs.amplify.aws/)
