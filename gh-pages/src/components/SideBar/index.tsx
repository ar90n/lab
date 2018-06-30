import React from 'react'
import { Link } from 'react-static'
import { withStyles, WithStyles } from 'material-ui/styles';

import { SiteDetail, IProps as ISiteDetailProps } from './SiteDetail';
import { HeadlineList, IProps as IHeadlineListProps } from './HeadlineList';

const drawerWidth = 240;

const styles = theme => ({
  root: {
    display: 'flex'
  },
  drawerPaper: {
    position: 'relative',
    width: drawerWidth,
  },
  toolbar: theme.mixins.toolbar
});

type ClassNames = keyof typeof styles;

interface IProps {
  siteDetail: ISiteDetailProps,
  recentPosts: IHeadlineListProps,
  archives: IHeadlineListProps,
  categories: IHeadlineListProps,
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const SideBar = withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, siteDetail, recentPosts, archives, categories } = props;
    return (
      <div>
        <div className={classes.toolbar} />
        <SiteDetail {...siteDetail}/>
        <HeadlineList title={recentPosts.title} headlines={recentPosts.headlines}/>
        <HeadlineList title={archives.title} headlines={archives.headlines}/>
        <HeadlineList title={categories.title} headlines={categories.headlines}/>
      </div>
    );
  }
);

export {
  IProps,
  SideBar
};
