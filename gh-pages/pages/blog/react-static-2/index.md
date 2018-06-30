---
title: サイトジェネレータをReact Staticに変更する2
date: "2018-03-28T14:30:16Z"
lang: "ja"
path: "react-static-2"
category:
  - "React"
  - "Javascript"
  - "Web"
description: "markdownのインポートとシンタックスハイライトに対応"
cover: "development"
---
まずはデータのインポートから．

## やったこと

1. gray-matter, remark, remark-reactを導入してmarkdownのインポートに対応
1. react-highlightを導入してシンタックスハイライトに対応　

## markdownのインポート対応
特に何も考えることなく，static.config.jsにてファイルを読み込みgetDataで結果を返してあげるとReact Staticがいい感じにやってくれるようだ.  
今回は，static.config.jsにてファイルの読み込みとfrontmatterのパースを行い，markdownのパースはJSX側で行うことにした．  
static.config.jsは以下のような感じ.

```javascript
import axios from 'axios';
import fs from 'fs-extra';
import matter from 'gray-matter';
import path from 'path';
import React from 'react';

// Paths Aliases defined through tsconfig.json
const typescriptWebpackPaths = require('./webpack.config.js');

export default {
  entry: path.join(__dirname, 'src', 'index.tsx'),
  getSiteData: () => ({
    title: 'React Static',
  }),
  getRoutes: async () => {
    const contentsRoot = path.resolve(__dirname, './pages/blog');
    const contents = fs.readdirSync(contentsRoot).map(postDir => {
      const indexPath = path.resolve(contentsRoot, postDir, 'index.md');
      return matter(fs.readFileSync(indexPath), 'utf8');
    });

    const routes = [
      {
        path: '/',
        component: 'src/containers/Home',
      },
      {
        path: '/about',
        component: 'src/containers/About',
      },
      {
        path: '/blog',
        component: 'src/containers/Blog',
        getData: () => ({
          data: contents.map(({ data }) => data),
        }),
        children: contents.map(({ data, content }) => {
          return {
              path: `/post/${data.path}`,
              component: 'src/containers/Post',
              getData: () => ({
                  content,
              }),
          };
        }),
      },
      {
        is404: true,
        component: 'src/containers/404',
      },
    ];

    return routes;
  },
  webpack: (config, { defaultLoaders }) => {
    // Add .ts and .tsx extension to resolver
    config.resolve.extensions.push('.ts', '.tsx')

    // Add TypeScript Path Mappings (from tsconfig via webpack.config.js)
    // to react-statics alias resolution
    config.resolve.alias = typescriptWebpackPaths.resolve.alias

    // We replace the existing JS rule with one, that allows us to use
    // both TypeScript and JavaScript interchangeably
    config.module.rules = [
      {
        oneOf: [
          {
            test: /\.(js|jsx|ts|tsx)$/,
            exclude: defaultLoaders.jsLoader.exclude, // as std jsLoader exclude
            use: [
              {
                loader: 'babel-loader',
              },
              {
                loader: require.resolve('ts-loader'),
                options: {
                  transpileOnly: true,
                },
              },
            ],
          },
          defaultLoaders.cssLoader,
          defaultLoaders.fileLoader,
        ],
      },
    ]
    return config
  },
}
```

/page/blog以下に投稿単位でディレクトリを作成し，index.mdという名前のmarkdonwを配置することにした．  
各index.mdはgray-matterによってdata(yaml)とcontent(markdown)に分割され，contentsに割り当てられる．  
これらのデータは/blogにアクセスした際に使用される．   
具体的には，ヘッドラインを表示するために，全dataのリストがBlogにpropsとして渡される．  
また，各投稿を表示するために該当するcontentがPostにpropsとして渡される．

## シンタックスハイライトの対応
Postは渡されたmarkdownをremarkおよびremark-reactを使ってパースする．  
remark-reactはremarkReactComponentsに任意のタグに対するReactコンポーネントを指定することができる．  
今回はシンタックスハイライトにraect-highlightを使用するため，codeに対してHighlightを指定する．  
Post.jsxは以下のような感じ．

```jsx
import React from 'react'
import { withRouteData, Link } from 'react-static'
import { Post } from '../types'
import remark from 'remark'
import remarkReact from 'remark-react'
import Highlight from 'react-highlight'

import 'highlight.js/styles/rainbow.css';

import githubSchema from 'hast-util-sanitize/lib/github.json';

const schema = Object.assign({}, githubSchema, {
  attributes: Object.assign({}, githubSchema.attributes, {
    code: [
      ...(githubSchema.attributes.code || []),
      'className'
    ]
  })
});

const code = ( { className, children } ) => {
    return (
        <Highlight className={className} >
            {children}
        </Highlight>
    );
};

export default withRouteData(({ content }: any) => (
  <div>
    <Link to="/blog/">{'<'} Back</Link>
    <br />
    <div>
      {remark()
      .use(remarkReact, {
        sanitize: schema,
        remarkReactComponents: {
            code
        }
      })
      .processSync(content).contents}
    </div>
  </div>
))
```

## 所感
static.config.jsでデータを整形さえすれば後はReact Staticがパッケージングしてくれるので非常に楽だった．  
とりあえずデータのインポートはできそうなので，次回からはUI周りを作っていく．
