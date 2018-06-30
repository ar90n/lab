import React from 'react'
import { withRouteData, Switch, Route } from 'react-static'
//
import { withStyles, WithStyles } from 'material-ui/styles';
import Grid  from 'material-ui/Grid';

import { AppBar } from '@components/AppBar'
import { SideBar, IProps as ISideBarProps } from '@components/SideBar'
import { Blog } from '@containers/Blog'
import Post from '@containers/Post'
import { PrivacyPolicy } from '@containers/PrivacyPolicy'
import NotFound from '@containers/404'

const styles = theme => ({
  root: {
    height: '100%',
    zIndex: 1,
    display: 'flex',
    justifyContent: 'center',
    overflow: 'auto',
    backgroundColor: theme.palette.background.default,
  },
  main: {
    flexGrow: 1,
    padding: theme.spacing.unit * 3,
    minWidth: 0, // So the Typography noWrap works
  },
  contents: {
    maxWidth: '1280px'
  },
  toolbar: theme.mixins.toolbar,
});

type ClassNames = keyof typeof styles;

interface IProps {
  title: string;
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const App = withRouteData(withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, lang, title, content, hasPrivacyPolicy, history } = props;

    const onContentChange = (e, content) => history.push(`/${lang}/${content}`);
    const onLangChange = (e, lang) => history.push(`/${lang}/${content}`);
    return (
      <React.Fragment>
        <AppBar
          title={title}
          lang={lang}
          content={content}
          hasPrivacyPolicy={hasPrivacyPolicy}
          onContentChange={onContentChange}
          onLangChange={onLangChange}
        />
        <div className={classes.toolbar} />
        <div className={classes.root}>
          <Grid container className={classes.contents}>
            <Grid item xs={9}>
              <main className={classes.main}>
                <Switch>
                  <Route path={`/${lang}/blog`} exact component={Blog} />
                  <Route path={`/${lang}/blog/post/:id`} exact component={Post} />
                  <Route path={`/${lang}/blog/category/:category`} exact component={Blog} />
                  <Route path={`/${lang}/blog/archive/:year/:month`} exact component={Blog} />
                  {(() => hasPrivacyPolicy && <Route path={`/${lang}/privacy_policy`} component={PrivacyPolicy} />)()}
                  <Route component={NotFound} />
                </Switch>
              </main>
            </Grid>
            <Grid item xs={3}>
              <SideBar {...props} />
            </Grid>
          </Grid>
        </div>
      </React.Fragment>
    );
  }
));

export {
  App,
  IProps
};
