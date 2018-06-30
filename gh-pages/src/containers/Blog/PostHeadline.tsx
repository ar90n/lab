import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import { Link } from 'react-static'
import Card, { CardContent, CardMedia } from 'material-ui/Card';
import Paper from 'material-ui/Paper';
import Button from 'material-ui/Button';
import Typography from 'material-ui/Typography';
import moment from 'moment';
import { DateChip } from '@components/DateChip';
import { getCoverPath } from '@src/cover';

import { compose, withState, withHandlers } from 'recompose';

const styles = theme => ({
  card: {
    height: 400,
    margin: '8px',
    position: 'relative'
  },
  media: {
    height: 230,
  },
  content: {
    display: 'flex',
    flexDirection: 'column',
    height: 170,
    boxSizing: 'border-box',
  },
  title: {
    display: 'flex',
    justifyContent: 'center',
    flexWrap: 'wrap',
    margin: '15px 0px'
  },
  categoryContainer: {
    display: 'flex',
    justifyContent: 'center',
    flexWrap: 'wrap'
  },
  category: {
    margin: '0px 5px',
    fontSize: '0.8em'
  },
  description: {
    display: 'flex',
    alignItems: 'center',
    flex: '1'
  }
});

type ClassNames = keyof typeof styles;

interface IProps {
  title: string,
  category: string[],
  description: string,
  cover: string,
  path: string,
  date: string,
  base: string
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const ElevatableCard = compose(
  withState('elevation', 'setElevation', 1),
  withHandlers({
    onMouseEnter: props => event => {
      props.setElevation(16);
      return false;
    },
    onMouseLeave: props => event => {
      props.setElevation(1);
      return false;
    }
  })
)(({ children, ...props }) => (
  <Card {...props}>
    {children}
  </Card>
));

const PostHeadline = withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, title, category, description, cover, path, date, base } = props;
    moment.locale('ja')
    const coverPath = getCoverPath(cover, 'small');
    return (
      <ElevatableCard className={classes.card} >
        <DateChip date={date} />
        <CardMedia
          className={classes.media}
          image={coverPath}
          title="Contemplative Reptile"
        />
        <CardContent className={classes.content}>
          <div className={classes.categoryContainer}>
            {category.map((name, i) => (
              <Link key={`category-${i}`} to={`${base}/category/${name}`}>
                <Typography className={classes.category} color="textSecondary">
                  #{name}
                </Typography>
              </Link>
            ))}
          </div>
          <Link to={`${base}/post/${path}`}>
            <Typography gutterBottom variant="title" component="h2" className={classes.title}>
              {title}
            </Typography>
          </Link>
          <Typography component="p" color="textSecondary" className={classes.description}>
            {description}
          </Typography>
        </CardContent>
      </ElevatableCard>
    );
  }
);

export {
    IProps,
    PostHeadline
};
