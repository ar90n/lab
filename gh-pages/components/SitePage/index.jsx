import React from 'react'
import moment from 'moment'
import { RouteHandler, Link } from 'react-router'
import { prefixLink } from 'gatsby-helpers'
import access from 'safe-access'
import { config } from 'config'
import SiteSidebar from '../SiteSidebar'
import { AppBar } from 'material-ui'
import spacing from 'material-ui/styles/spacing';
import withWidth, {SMALL,MEDIUM, LARGE} from 'material-ui/utils/withWidth'
//import './style.css';

const styles ={
  appBar: {
    top: 0,
  },
  drawer: {
    zIndex: 1100,
    position: 'fixed',
    height: '100%',
  },
  main: {
    [SMALL]: {
      position: 'fixed',
      margin: `${spacing.desktopGutter * 2}px ${spacing.desktopGutter * 3}px`,
    },
    [MEDIUM]: {
      position: 'fixed',
      margin: `${spacing.desktopGutter * 2}px ${spacing.desktopGutter * 3}px`,
    },
    [LARGE]: {
      position: 'fixed',
      margin: spacing.desktopGutter,
    }
  },
  footer: {
    //    backgroundColor: grey900,
    textAlign: 'center',
  },
};


class SitePage extends React.Component {
    render() {
        const {route} = this.props
        const post = route.page.data

        let { drawerOpen } = true;//this.state;
        let docked = false;
        let showMenuIconButton = true;
        const drawerStyle = styles.drawer;
        const mainStyle = styles.main[LARGE];
        return (
            <div>
              <AppBar
                style = {styles.appBar}
                onLeftIconButtonTouchTap={this.onTouchTapLeftIconButton}
                title={config.siteTitle}
                iconElementRight={<div />}
                //showMenuIconButton={showMenuIconButton}
              />
              <div>
                <SiteSidebar style={drawerStyle} open={drawerOpen} docked={docked} onChange={this.onChangePage}/>
                <div style={mainStyle}>
                   <h1>{ post.title }</h1>
                   <div dangerouslySetInnerHTML={ {    __html: post.body} } />
                </div>
              </div>
            </div>
            );
    }
}

SitePage.propTypes = {
    post: React.PropTypes.object.isRequired,
    pages: React.PropTypes.array,
}

export default SitePage
