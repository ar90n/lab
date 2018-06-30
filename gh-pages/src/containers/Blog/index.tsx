import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import { withRouteData, Link } from 'react-static'
import { Post } from '../types'
import { PostHeadline } from './PostHeadline'

import Grid  from 'material-ui/Grid';
import Hidden from 'material-ui/Hidden';

const styles = theme => ({
  cards: {
  },
});

type ClassNames = keyof typeof styles;

interface IProps {
  data: Post[],
  base: string
}

type IStyledProps = IProps & WithStyles<ClassNames>;

const Blog = withRouteData( 
  withStyles<{} & ClassNames>(styles)<IProps>(
    (props: IStyledProps) => {
      const {classes, lang, data } = props;
      const base = `/${lang}/blog`;
      return (
        <div>
          <Grid container className={classes.cards}>
            <Hidden smDown={true}>
              {data.map((post, i) => (
                <Grid item xs={6} key={`headline-${i}`}>
                  <PostHeadline  {...post} base={base}/>
                </Grid>
              ))}
            </Hidden>
            <Hidden mdUp={true}>
              {data.map((post, i) => (
                <Grid item xs={12} key={`headline12-${i}`}>
                  <PostHeadline  {...post} base={base}/>
                 </Grid>
              ))}
            </Hidden>
          </Grid>
        </div>
      );
    }
  )
);


export {
  Blog,
  IProps,
};
