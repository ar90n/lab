import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import Tabs, { Tab } from 'material-ui/Tabs';
import AppBar from 'material-ui/AppBar'
import Toolbar from 'material-ui/Toolbar';
import Typography from 'material-ui/Typography';
import Button from 'material-ui/Button';
import IconButton from 'material-ui/IconButton';

import { LangSelector } from './LangSelector';

const styles = theme => ({
  root: {
    zIndex: theme.zIndex.drawer + 10,
  },
  toolbar: {
    display: 'flex',
    justifyContent: 'space-between'
  },
  rightComponent: {
    display: 'flex',
  },
});

type ClassNames = keyof typeof styles;

interface IProps {
  title: string;
  lang: string;
  content: string;
  onContentChange(e: event, content: string): void
  onLangChange(e: event, lang: string): void
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const _AppBar = withStyles<{} & ClassNames>(styles)<IProps>(
    (props: IStyledProps) => {
      const { classes, title, lang, content, hasPrivacyPolicy, onContentChange, onLangChange} = props;

      return (
        <AppBar position="fixed" className={ classes.root }>
          <Toolbar className={classes.toolbar}>
            <Typography variant="title" color="textSecondary">
                { title }
            </Typography>
            <div className={classes.rightComponent}>
              <Tabs value={content} onChange={onContentChange}>
                <Tab value='blog' label="Blog" />
                {/* <Tab value='projects' label="Projects" textColorPrimary /> */}
                {(() => hasPrivacyPolicy && <Tab value='privacy_policy' label="Privacy Policy" />)()}
              </Tabs>
              <LangSelector value={lang} onChange={onLangChange} />
            </div>
          </Toolbar>
        </AppBar>
      );
    }
);

export {
  IProps,
  _AppBar as AppBar
};
