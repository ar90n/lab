import React from 'react'

import { withStyles, WithStyles } from 'material-ui/styles';
import { fade } from 'material-ui/styles/colorManipulator';
import { Link } from 'react-static'

import Card, { CardContent, CardMedia } from 'material-ui/Card';
import Paper from 'material-ui/Paper';
import Typography from 'material-ui/Typography';
import { DateChip } from '@components/DateChip';
import { Markdown } from '@components/Markdown';
import { getCoverPath } from '@src/cover';


const styles = theme => ({
  root: {
    marginBottom: '16px'
  },
  media: {
    height: 300,
    position: 'relative',
    display: 'flex',
    alignItems: 'center'
  },
  title: {
    width: '100%',
    backgroundColor: fade(theme.palette.primary.dark, 0.6),
    padding: '24px',
  },
  titleText: {
    display: 'flex',
    justifyContent: 'center',
    flexWrap: 'wrap',
    fontWeight: 600,
    fontSize: '32px'
  },
  category: {
    display: 'flex',
    justifyContent: 'center',
    flexWrap: 'wrap'
  },
  categoryItem: {
    margin: '0px 5px',
    fontWeight: 600,
    fontSize: '16px'
  },
});

type ClassNames = keyof typeof styles;

interface IProps {
  data: any,
  content: any,
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const PostContent = withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, lang, data, content } = props;
    const coverPath = getCoverPath(data.cover, 'middle');
    return (
      <Card className={classes.root}>
        <CardMedia
          className={classes.media}
          image={coverPath}
        >
          <DateChip date={data.date} />
          <div className={classes.title}>
            <Typography
              gutterBottom
              variant="title"
              component="h1"
              color="primary"
              className={classes.titleText}
            >
              {data.title}
            </Typography>
            <div className={classes.category}>
              {data.category.map((name, i) => (
                <Link key={`content-category-${i}`} to={`/${lang}/blog/category/${name}`}>
                  <Typography className={classes.categoryItem} color="textSecondary">
                    #{name}
                  </Typography>
                </Link>
              ))}
            </div>
          </div>
        </CardMedia>
        <CardContent>
          <Markdown content={content} />
        </CardContent>
      </Card>
    );
  }
);

export {
  IProps,
  PostContent
}
