import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import Tabs, { Tab } from 'material-ui/Tabs';

const tabStyles = theme => ({
  root: {
    minWidth: '0px',
  },
  textColorSecondary: {
    color: 'inherit',
    opacity: 0.7,
    '&$selected': {
      color: theme.palette.secondary.main,
    },
  },
  labelContainer: {
    paddingTop: 6,
    paddingBottom: 6,
    paddingLeft: theme.spacing.unit,
    paddingRight: theme.spacing.unit,
  },
});

type TabClassNames = keyof typeof tabStyles;

type IStyledTabProps =  WithStyles<TabClassNames>;

const LangTab = withStyles<{} & ClassNames>(tabStyles)<{}>(
  (props: IStyledProps) => {
    const { classes, ...leftOvers } = props;

    return (
      <Tab
        {...leftOvers}
        textColor='secondary'
        classes={{
          root: classes.root,
          textColorSecondary: classes.textColorSecondary,
          labelContainer: classes.labelContainer
        }}/>
    );
  }
);


const styles = theme => ({
  flexContainer: {
    display: 'flex',
    alignItems: 'center',
    marginLeft: '32px'
  },
});

type ClassNames = keyof typeof styles;

interface IProps {
  value: string;
  onChange: any;
}

type IStyledProps = IProps & WithStyles<ClassNames>;

const LangSelector = withStyles<{} & ClassNames>(styles)<IProps>(
  (props) => {
    const { classes, value, onChange } = props;
    return (
      <Tabs
        indicatorColor="primary"
        value={value}
        onChange={onChange}
        classes={{
          flexContainer:classes.flexContainer
        }}
      >
        <LangTab value='ja' label="JA"/>
        <span>|</span>
        <LangTab value='en' label="EN"/>
      </Tabs>
    );
  }
);

export {
  IProps,
  LangSelector
}
