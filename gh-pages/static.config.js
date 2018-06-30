import axios from 'axios';
import fs from 'fs-extra';
import matter from 'gray-matter';
import path from 'path';
import React from 'react';

const langs = ['ja', 'en'];
const title = 'lab.ar90n.net';

// Paths Aliases defined through tsconfig.json
const typescriptWebpackPaths = require('./webpack.config.js');

const compDate = (lhs, rhs) => rhs.date.getTime() - lhs.date.getTime();

const getBlogContents = async (lang) => {
  const blogContentsRoot = path.resolve(__dirname, './pages/blog');
  return fs.readdirSync(blogContentsRoot)
        .map(postDir => {
            const indexPath = path.resolve(blogContentsRoot, postDir, 'index.md');
            const parsed  = matter(fs.readFileSync(indexPath), 'utf8');
            parsed.data.date = new Date(parsed.data.date);
            return parsed;
        })
        .filter(({ data }) => data.lang === lang)
        .sort((lhs, rhs) => compDate(lhs.data, rhs.data));
};

const getRecentPostHeadlines = async (lang) => {
  const contents = await getBlogContents(lang);
  return contents.map(({data}) => data)
    .sort(compDate)
    .slice(0,5)
    .map((data) => {
      return {
          title: data.title,
          path: `/${lang}/blog/post/${data.path}`
      }
    });
}

const getArchiveHeadlines = async (lang) => {
  const getArchiveKey = data => new Date(data.date.getFullYear(), data.date.getMonth());
  const getArchiveLabel = (data, lang) => {
      const date = new Date(data[0]);
      const year = date.getFullYear().toString();
      const month = `000${date.getMonth() + 1}`.slice(-2);
      return {
        title: `${year}/${month} (${data[1].length})`,
        path: `/${lang}/blog/archive/${year}/${month}`
      };
  }

  const contents = await getBlogContents(lang);
  return Object.entries(contents.filter(({ data }) => data.lang == lang)
    .reduce((acc, {data}) => {
      const key = getArchiveKey(data);
      return Object.assign(acc, {[key]:(acc[key] || []).concat([data])})
    }, {}))
    .sort((lhs, rhs) => compDate({date: new Date(lhs[0])}, {date: new Date(rhs[0])}))
    .map((v) => getArchiveLabel(v, lang))
}

const getCategoryHeadlines = async (lang) => {
  const getCategoryLabel = (data, lang) => {
      return {
        title: `${data[0]} (${data[1].length})`,
        path: `/${lang}/blog/category/${data[0]}`
      }
  }

  const contents = await getBlogContents(lang);
  return Object.entries(contents.filter(({ data }) => data.lang == lang).
    reduce((acc, {data}) => {
      return data['category'].reduce((acc, key) => Object.assign(acc, {[key]:(acc[key] || []).concat([data])}), acc)
    }, {}))
    .map((v) => getCategoryLabel(v, lang))
}

const getPrivacyPolicyContents = async (lang) => {
  const privacyPolicyContentsRoot = path.resolve(__dirname, './pages/privacy_policy');
  return fs.readdirSync(privacyPolicyContentsRoot)
        .map(filePath => {
            if(!filePath.endsWith('.md')) {
                return null;
            }

            const indexPath = path.resolve(privacyPolicyContentsRoot, filePath);
            const parsed  = matter(fs.readFileSync(indexPath), 'utf8');
            return parsed;
        })
        .filter(x => x !== null)
        .filter(({ data }) => data.lang === lang);
};

const getBlogRoutes = async (lang) => {
  const contents = await getBlogContents(lang);
  const postRoutes = contents.map(({ data, content }, index) => {
      const prev = (0 < index) ? contents[index - 1].data : null;
      const next = (index < (contents.length - 1)) ? contents[index + 1].data : null;
      return {
          path: `/post/${data.path}`,
          getData: () => ({
              lang,
              data,
              prev,
              next,
              content,
          }),
      };
  });

  const categoryMap = contents.reduce((acc, {data}) => {
    const accumlate = (acc, key) => Object.assign(acc, {[key]:(acc[key] || []).concat([data])});
    return data['category'].reduce(accumlate, acc)
  }, {});
  const categoryRoutes = Object.entries(categoryMap)
    .map(([category, data]) => ({
      path: `/category/${category}`,
      getData: () => ({
        lang,
        data,
      }),
    }));

  const archiveMap = contents.reduce((acc, {data}) => {
    const key = new Date(data.date.getFullYear(), data.date.getMonth());
    return Object.assign(acc, {[key]:(acc[key] || []).concat([data])})
  }, {});
  const archiveRoutes = Object.entries(archiveMap)
    .map(([date, data]) => {
      const year = new Date(date).getFullYear().toString();
      const month = `000${new Date(date).getMonth() + 1}`.slice(-2);
      return {
        path: `/archive/${year}/${month}`,
        getData: () => ({
          lang,
          data,
        }),
      }
    });

  return {
      path: `/blog`,
      getData: () => ({
        lang,
        data: contents.map(({ data }) => data),
      }),
      children: [
        ...postRoutes,
        ...categoryRoutes,
        ...archiveRoutes
      ]
  };
}

const getPrivacyPolicyRoutes = async (lang) => {
    const contents = await getPrivacyPolicyContents(lang);
    if(contents.length === 0)
    {
        return null;
    }

    return {
        path: `/privacy_policy`,
        component: 'src/containers/PrivacyPolicy',
        getData: () => contents[0]
    };
}

const getRootRoutes = async (langs) => {
  const routes = await Promise.all(langs.map(async (lang) => {
    const siteDetail = {
      photo: 'https://avatars2.githubusercontent.com/u/2285892?s=180',
      author: 'Masahiro Wada',
      description: 'I like C, C++, F#, Python, Typescript,  Image Processing and Signal Processing',
      github: 'https://github.com/ar90n',
      twitter: 'https://twitter.com/ar90n',
      linkedin: 'https://www.linkedin.com/in/ar90n/',
      mail: 'argon.argon.argon@gmail.com'
    }
    const recentPosts = {
      title: 'RECENT POSTS',
      headlines: await getRecentPostHeadlines(lang),
    }
    const archives = {
      title: 'ARCHIVES',
      headlines: await getArchiveHeadlines(lang),
    }
    const categories = {
      title: 'CATEGORIES',
      headlines: await getCategoryHeadlines(lang),
    }

    const blogRoutes = await getBlogRoutes(lang);
    const privacyPolicyRoutes = await getPrivacyPolicyRoutes(lang);
    const hasPrivacyPolicy = privacyPolicyRoutes !== null;

    const children = [blogRoutes];
    if(hasPrivacyPolicy) {
        children.push(privacyPolicyRoutes);
    }
    return {
      path: `${lang}`,
      getData: () => ({
        siteDetail,
        recentPosts,
        archives,
        categories,
        hasPrivacyPolicy
      }),
      children
    };
  }));

  return routes;
};

export default {
  siteRoot: 'lab.ar90n.net',
  entry: path.join(__dirname, 'src', 'index.tsx'),
  getSiteData: async () => {
    return {
      title,
      langs
    };
  },
  getRoutes: async () => {
    const rootRoutes = await getRootRoutes(langs);

    const routes = [
      ...rootRoutes,
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
