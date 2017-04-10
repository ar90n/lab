import React from 'react'
import { RouteHandler, Link } from 'react-router'
import { prefixLink } from 'gatsby-helpers'
import { config } from 'config'
import SiteNav from '../SiteNav'
import SiteLinks from '../SiteLinks'
import { Drawer, Divider } from 'material-ui'

const styles = {
  detail: {
    display: 'flex',
    margin: '8px 0px 8px 16px',
    alignItems: 'center',
  },
  author_desc: {
    margin: '0px 0px 0px 8px',
  },
  author: {
  },
  desc: {
    fontSize: 'small',
  },
}

const BlogDetail = props => (
  <div style={styles.detail}>
    <img src={'https://avatars0.githubusercontent.com/u/2285892'} width="75" height="75" />
    <div style={styles.author_desc}>
      <p style={styles.author} >{ config.siteAuthor }</p>
      <p style={styles.desc} >{ config.siteDescr }</p>
    </div>
  </div>
)

const SiteSidebar = props => (
  <Drawer {...props} containerStyle={{ position: 'auto' }}>
    <BlogDetail {...props} />
    <Divider />
    <SiteNav {...props} />
    <SiteLinks {...props} />
  </Drawer>
)

SiteSidebar.propTypes = {
  children: React.PropTypes.any,
  location: React.PropTypes.object,
}

export default SiteSidebar
