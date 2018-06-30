import React from 'react'

import { withStyles, WithStyles } from 'material-ui/styles';
import { Link } from 'react-static'

import Card from 'material-ui/Card';
import Button from 'material-ui/Button';
import Divider from 'material-ui/Divider';
import KeyboardArrowRight from '@material-ui/icons/KeyboardArrowRight';
import KeyboardArrowLeft from '@material-ui/icons/KeyboardArrowLeft';
import Typography from 'material-ui/Typography';
import moment from 'moment';


const navLabelStyles = (prev) => theme => ({
  navi: {
    display: 'flex',
    flexDirection: 'column'
  },
  categoryContainer: {
    display: 'flex',
  },
  category: {
    marginLeft: '8px'
  },
  title: {
    margin: '1px',
    display: 'flex',
    justifyContent: prev ? 'flex-start' : 'flex-end'
  },
  attribute: {
    display: 'flex',
    justifyContent: prev ? 'flex-start' : 'flex-end'
  }
});
const prevNavLabelStyles = navLabelStyles(true);
const nextNavLabelStyles = navLabelStyles(false);
type NavLabelClassNames = keyof typeof prevNavLabelStyles;

interface INavLabelProps {
  date: any,
  title: string,
  category: string[],
};

type IStyledNavLabelProps = INavLabelProps & WithStyles<NavLabelClassNames>;

const navLabel = (props: IStyledNavLabelProps) => {
  const { classes, date, title, category } = props;
  return (
    <div className={classes.navi}>
      <h5 className={classes.title}> {title} </h5>
      <div className={classes.attribute}>
        <Typography color="textSecondary">
          {moment(new Date(date)).format('l')}
        </Typography>
        <div className={classes.categoryContainer}>
          {category.map((name, i) => (
            <Typography key={`navi-category-${i}`} className={classes.category} color="textSecondary">
              #{name}
            </Typography>
          ))}
        </div>
      </div>
    </div>
  );
};

const PrevNavLabel = withStyles<{} & NavLabelClassNames>(prevNavLabelStyles)<INavLabelProps>(navLabel);
const NextNavLabel = withStyles<{} & NavLabelClassNames>(nextNavLabelStyles)<INavLabelProps>(navLabel);


const styles = theme => ({
  content: {
    display: 'flex',
    marginLeft: '24px',
    marginRight: '24px',
  },
  arrow: {
    fontSize: '48px'
  },
  prevButton: {
    display: 'flex',
    width: '100%',
    justifyContent: 'flex-start'
  },
  nextButton: {
    display: 'flex',
    width: '100%',
    justifyContent: 'flex-end'
  }
});

type ClassNames = keyof typeof styles;

interface IProps {
  prev: INavButtonProps,
  next: INavButtonProps
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const PostNavi = (props: IStyledProps) => {
  const { classes, prev, next } = props;
  return (
      <Card>
        {(() => prev && (
          <Button
            className={classes.prevButton}
            component={Link}
            to={`/${prev.lang}/blog/post/${prev.path}`}
          >
            <KeyboardArrowLeft />
            <PrevNavLabel {...prev}/>
          </Button>
        ))()}
        <Divider />
        {(() => next && (
          <Button
            className={classes.nextButton}
            component={Link}
            to={`/${next.lang}/blog/post/${next.path}`}
          >
            <NextNavLabel {...next}/>
            <KeyboardArrowRight />
          </Button>
        ))()}
      </Card>
  );
}

export default withStyles<{} & ClassNames>(styles)<IProps>(PostNavi);
export {
  IProps,
}
