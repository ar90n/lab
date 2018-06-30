import React from 'react'
import { withSiteData, Route, Switch, Router, Redirect } from 'react-static'
import { hot } from 'react-hot-loader'

import { App } from '@containers/App'
import NotFound from '@containers/404'

import './root.css'

interface IProps {
  title: string
}

const Root = (props: IProps) => {
  const { title, langs } = props
  return (
    <Router>
      <Switch>
        <Route
          path={'/'}
          exact
          render={() => <Redirect to='/ja/blog' />}
        />
        {langs.map((lang, i) => (
          <Route
            key={`exact-lang-${i}`}
            path={`/${lang}`}
            exact
            render={() => <Redirect to={`/${lang}/blog`} />}
          />
        ))}
        {langs.map((lang, i) => (
          <Route
            key={`lang-${i}`}
            path={`/${lang}`}
            render={(renderProps) => {
              const { match: { url }, location: { pathname } } = renderProps;
              const content = pathname.match(RegExp(`${url}\/([a-z_]+).*`))[1];
              return (
                <App lang={lang} title={title} content={content} {...renderProps} />
              );
            }}
          />
        ))}
        <Route component={NotFound} />
      </Switch>
    </Router>
  );
}

export default hot(module)(withSiteData((Root)));
export {
  IProps
};
