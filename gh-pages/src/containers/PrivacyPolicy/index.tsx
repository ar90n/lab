import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import { withRouteData, Link } from 'react-static'

import Card, { CardContent } from 'material-ui/Card';
import Paper from 'material-ui/Paper';
import Typography from 'material-ui/Typography';
import { DateChip } from '@components/DateChip';
import { Markdown } from '@components/Markdown';

const styles = theme => ({
  root: {
  },
  titleText: {
    display: 'flex',
    justifyContent: 'center',
    flexWrap: 'wrap',
    fontWeight: 600,
    fontSize: '32px'
  },
});

type ClassNames = keyof typeof styles;

interface IProps {
  data: any,
  content: any,
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const PrivacyPolicy = withRouteData(
  withStyles<{} & ClassNames>(styles)<IProps>(
    (props: IStyledProps) => {
      const {classes, data, content} = props;
      return (
        <Card className={classes.root}>
          <CardContent>
            <Typography
              gutterBottom
              variant="title"
              component="h1"
              className={classes.titleText}
            >
              {data.title}
            </Typography>
            <Markdown content={content} />
          </CardContent>
        </Card>
      );
    }
  )
);

export {
  PrivacyPolicy,
  IProps
}
