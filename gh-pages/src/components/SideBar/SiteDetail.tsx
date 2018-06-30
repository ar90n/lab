import React from 'react'
import { withStyles, WithStyles } from 'material-ui/styles';
import Paper from 'material-ui/Paper';
import FontaAwesome from 'react-fontawesome';
import './font-awesome/css/font-awesome.min.css';

const styles = theme => ({
  root: {
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center',
    marginBottom: '32px',
  },
  photo: {
    margin: '0px 0px 16px 0px',
    display: 'flex',
    maxWidth: '180px',
    maxHeight: '180px',
  },
  info: {
    textAlign: 'center'
  },
  author: {
    margin: '0px',
    fontSize: '30px',
  },
  desc: {
    margin: '12px 0px',
    fontWeight: 500,
  },
  list: {
    display: 'flex',
    justifyContent: 'center',
    listStyle: 'none',
    padding: '0',
    margin: '10px 0',
  },
  container: {
    marginRight: '5px',
    height: '24px',
    width: '24px',
  },
  icon: {
    color: theme.palette.secondary.main,
    fontSize: '21px',
    lineHeight: '24px',
  }
});

type ClassNames = keyof typeof styles;

interface IProps {
    photo: string;
    author: string;
    description: string;
};

type IStyledProps = IProps & WithStyles<ClassNames>;

const SiteDetail = withStyles<{} & ClassNames>(styles)<IProps>(
  (props: IStyledProps) => {
    const { classes, src, author, description, github, twitter, linkedin, mail } = props;
    return (
      <div className={classes.root}>
        <Paper className={classes.photo}>
          <img src={props.photo} />
        </Paper>
        <div className={classes.info}>
            <h3 className={classes.author}>{props.author}</h3>
            <h4 className={classes.desc}>{props.description}</h4>
            <ul className={classes.list}>
                <li className={classes.container}>
                    <a href={twitter}>
                      <FontaAwesome name='fab fa-twitter' className={classes.icon}/>
                    </a>
                </li>
                <li className={classes.container}>
                    <a href={github}>
                      <FontaAwesome name='fab fa-github-alt' className={classes.icon}/>
                    </a>
                </li>
                <li className={classes.container}>
                    <a href={linkedin}>
                      <FontaAwesome name='fab fa-linkedin' className={classes.icon}/>
                    </a>
                </li>
                <li className={classes.container}>
                    <a href={`mailto:${mail}`}>
                      <FontaAwesome name='fab fa-envelope-o' className={classes.icon}/>
                    </a>
                </li>
            </ul>
        </div>
      </div>
    );
  }
);

export {
    IProps,
    SiteDetail
};
