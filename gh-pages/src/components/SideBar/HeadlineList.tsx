import React from 'react'
import { Link } from 'react-static'
import { withStyles, WithStyles } from 'material-ui/styles';
import Divider from 'material-ui/Divider';
import ListSubheader from 'material-ui/List/ListSubheader';
import List, { ListItem, ListItemText } from 'material-ui/List';
import Typography from 'material-ui/Typography';

const styles = theme => ({
  root: {
    display: 'flex',
    flexDirection: 'column',
    margin: '16px 0px',
  },
  title: {
    fontWeight: 400,
    fontSize: '20px',
    color: theme.palette.secondary.light
  },
  list: {
    listStyleType: 'none',
    padding: '0px',
    margin: '0xp'
  },
  headline: {
    margin: '8px 0px',
    fontSize: '12px',
  },
});

type ClassNames = keyof typeof styles;

interface IHeadline {
  title: string,
  path: string
};

interface IProps {
  title: string,
  headlines: IHeadline[]
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const HeadlineList = withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, title, headlines } = props;
    return (
      <div className={classes.root}>
        <div className={classes.title}>
          { title }
        </div>
        <ul className={classes.list}>
          {headlines.map(({title, path}) => (
            <li key={`item-${title}`}>
              <Link to={path}>
                <Typography gutterBottom variant="headline" className={classes.headline}>
                  {title}
                </Typography>
              </Link>
              <Divider />
            </li>
          ))}
        </ul>
      </div>
    );
  }
);

export {
  IHeadline,
  IProps,
  HeadlineList
};
