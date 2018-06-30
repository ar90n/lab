import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import Paper from 'material-ui/Paper';
import Typography from 'material-ui/Typography';
import moment from 'moment';


const styles = theme => ({
  root: {
    position: 'absolute',
    top: '24px',
    padding: '8px',
    backgroundColor: theme.palette.primary.dark
  },
  text: {
    fontWeight: 600,
    fontSize: '14px',
  }
});

type ClassNames = keyof typeof styles;

interface IProps {
  date: Date,
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const DateChip = withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, date } = props;
    return (
      <Paper elevation={0} className={classes.root}>
        <Typography className={classes.text} color="primary">
          {moment(date).format('l')}
        </Typography>
      </Paper>
    );
  }
);

export {
    IProps,
    DateChip
};
