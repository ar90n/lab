import React, {PropTypes} from 'react'
import { Link } from 'react-router'
import sortBy from 'lodash/sortBy'
import moment from 'moment'
import Helmet from 'react-helmet'
import { prefixLink } from 'gatsby-helpers'
import access from 'safe-access'
import { config } from 'config'
import SitePost from '../components/SitePost'
import SiteSidebar from '../components/SiteSidebar'
import { AppBar } from 'material-ui'
import spacing from 'material-ui/styles/spacing';
import withWidth, {SMALL,MEDIUM, LARGE} from 'material-ui/utils/withWidth'

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

class SiteIndex extends React.Component {

  static contextTypes = {
    muiTheme: PropTypes.object,
    router: PropTypes.object,
  };

  state = {
      drawerOpen: false,
  };

  onTouchTapLeftIconButton = () => {
      this.onChangeRequestDrawer( !this.state.drawerOpen )
  };

  onChangeRequestDrawer = (open) => {
      this.setState({
          drawerOpen: open,
      });
  };

  onChangePage = (event, value) => {
      console.log( value );
      this.context.router.push(value);
      this.onChangeRequestDrawer( false );
  }

  render () {
    const pageLinks = []
        // Sort pages.
    const sortedPages = sortBy(this.props.route.pages, page => access(page, 'data.date'),
        ).reverse()
    sortedPages.forEach((page) => {
      if (access(page, 'file.ext') === 'md' && access(page, 'data.layout') === 'post') {
        const title = access(page, 'data.title') || page.path
        const description = access(page, 'data.description')
        const datePublished = access(page, 'data.date')
        const category = access(page, 'data.category')

        pageLinks.push(
          <div className="blog-post">
            <time dateTime={moment(datePublished).format('MMMM D, YYYY')}>
              { moment(datePublished).format('MMMM YYYY') }
            </time>
            <span style={{ padding: '5px' }} />
            <span className="blog-category">{ category }</span>
            <h2><Link style={{ borderBottom: 'none' }} to={prefixLink(page.path)} > { title } </Link></h2>
            <p dangerouslySetInnerHTML={{ __html: description }} />
            <Link className="readmore" to={prefixLink(page.path)}> Read
                      </Link>
          </div>,
                )
      }
    })

    // Needed to overlap the examples
    const appStyle = Object.assign( styles.appBar, {zIndex: this.context.muiTheme.zIndex.appBar + 1})
    const mainStyle = styles.main[this.props.width];
    const drawerStyle = styles.drawer;

    let { drawerOpen } = this.state;
    let docked = false;
    let showMenuIconButton = true;
    if (this.props.width === LARGE) {
        docked = true;
        drawerOpen = true;
        showMenuIconButton = false;

        styles.drawer = {
            zIndex: styles.appBar.zIndex - 1,
        };
        styles.footer.paddingLeft = 256;
    }

    return (
      <div>
        <Helmet title={config.siteTitle} />
        <AppBar
          style = {appStyle}
          onLeftIconButtonTouchTap={this.onTouchTapLeftIconButton}
          title={config.siteTitle}
          iconElementRight={<div />}
          showMenuIconButton={showMenuIconButton}
        />
        <div>
          <SiteSidebar style={drawerStyle} open={drawerOpen} docked={docked} onChange={this.onChangePage}/>
          <div style={mainStyle}>
            { pageLinks }
          </div>
        </div>
      </div>
    )
  }
}

SiteIndex.propTypes = {
  route: React.PropTypes.object,
}

export default withWidth()(SiteIndex);
