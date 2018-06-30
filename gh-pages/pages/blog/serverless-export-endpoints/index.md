---
title: ServerlessでデプロイしたAPIのエンドポイントを出力するプラグインを作った
date: "2017-05-14T01:14:24Z"
lang: "ja"
path: "serverless-export-endpoints"
category:
  - "Web"
  - "Serverless"
description: "エンドポイントを手動で修正していたのがしんどかったので自動化を試みました"
cover: "programming"
---
リージョンを変更してAPIをデプロイする度にAPIのURLを手動で修正していたので，この処理を自動化しました．

### なにができるか？
- デプロイしたAPIのエンドポイントをjsonとして出力します
- パラメータを含むURLはMustache形式で出力します

### 動かすとどんな感じか？
インストールは以下のようにnpmから可能です．
```bash
$ npm install --save-dev serverless-plugin-export-endpoints
```

serverless.ymlには以下のようにserverless-plugin-export-endpointsを追加します．
```yaml
custom:
  - serverless-plugin-export-endpoints
```

最低限の動作を確認するためには，[example](https://github.com/ar90n/serverless-plugin-export-endpoints/tree/master/example)を試すのが一番簡単だと思います．
APIのデプロイ，エンドポイントのエクスポート，APIのコールを実際に行った結果をいかに示します．
```bash
$ sls deploy
Serverless: Packaging service...
Serverless: Creating Stack...
Serverless: Checking Stack create progress...
.....
Serverless: Stack create finished...
Serverless: Uploading CloudFormation file to S3...
Serverless: Uploading artifacts...
Serverless: Uploading service .zip file to S3 (7.27 KB)...
Serverless: Updating Stack...
Serverless: Checking Stack update progress...
..............................
Serverless: Stack update finished...
Service Information
service: sls-plg-xprt-endpoints-example
stage: dev
region: us-east-1
api keys:
  None
endpoints:
  GET - https://your-api-deploy-id.execute-api.us-east-1.amazonaws.com/dev/hello
functions:
  hello: sls-plg-xprt-endpoints-example-dev-hello
$ sls exportEndpoints
$ cat endponts.json
{
 "hello": {
  "GET": "https://your-api-deploy-id.execute-api.us-east-1.amazonaws.com/dev/hello"
 }
}
$ node test.js
Go Serverless v1.0! Your function executed successfully!
```
