import React from 'react'
import { prefixLink } from 'gatsby-helpers'
import { config } from 'config'
import { AppBar } from 'material-ui'

const SiteAppbar = ({ style, onLeftIconButtonTouchTap, showMenuIconButton }) => {
    const appStyle = Object.assign({}, style, { top: 0 })
    return (
        <AppBar style={appStyle} onLeftIconButtonTouchTap={onLeftIconButtonTouchTap} showMenuIconButton={showMenuIconButton} title={config.siteTitle} iconElementRight={< div />}/>
    )
}

export default SiteAppbar
