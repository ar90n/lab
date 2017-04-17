import React from 'react'
import { RouteHandler, Link } from 'react-router'
import { prefixLink } from 'gatsby-helpers'
import { config } from 'config'
import SiteNav from '../SiteNav'
import SiteLinks from '../SiteLinks'
import { Drawer, Divider, Subheader } from 'material-ui'
import * as colors from 'material-ui/styles/colors';
import {IconButton} from 'material-ui';
import {HardwareKeyboardArrowLeft} from 'material-ui/svg-icons'

const styles = {
    detail: {
        display: 'flex',
        margin: '8px 0px 8px 16px',
        alignItems: 'center'
    },
    author_desc: {
        margin: '0px 0px 0px 8px'
    },
    author: {
        margin: '8px 0px'
    },
    desc: {
        margin: '8px 0px',
        fontSize: 'small'
    },
    drawer: {
        height: '100%'
    },
    overlayTitle: {
        width: '100%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        padding: '0px 16px'
    }
}

const SiteDetail = props => (
    <div style={styles.detail}>
        <img src={'https://avatars0.githubusercontent.com/u/2285892'} width="75" height="75"/>
        <div style={styles.author_desc}>
            <div style={styles.author}>{config.siteAuthor}</div>
            <div style={styles.desc}>{config.siteDescr}</div>
        </div>
    </div>
)

const SiteSidebar = ({ style, containerStyle, open, docked, onChange }) => {
    const style2 = Object.assign({}, style, styles.drawer)
    const containerStyle2 = Object.assign({},containerStyle, {
        backgroundColor: null,
        boxShadow: null
    })
    return (
        <Drawer style={style2} containerStyle={containerStyle2} open={open} docked={docked} >
            <Subheader>Author</Subheader>
            <SiteDetail/>
            <Divider/>
            <Subheader>Conents</Subheader>
            <SiteNav onChange={onChange}/>
            <Divider/>
            <Subheader>Links</Subheader>
            <SiteLinks/>
        </ Drawer>
    )
}

const SiteOverlaySidebar = ({ style, containerStyle, appbarHeight, open, docked, onChange, onClose }) => {
    const style2 = Object.assign({}, style, styles.drawer)
    const titleStyle = Object.assign({}, styles.overlayTitle,{height: appbarHeight})

    return (
        <Drawer style={style2} containerStyle={containerStyle} open={open} docked={docked} >
            <div style={titleStyle}>
                <div>{config.siteTitle}</div>
                <IconButton>
                    <HardwareKeyboardArrowLeft onTouchTap={onClose}/>
                 </IconButton>
            </div>
            <Divider/>
            <Subheader>Conents</Subheader>
            <SiteNav onChange={onChange}/>
            <Divider/>
            <Subheader>Links</Subheader>
            <SiteLinks/>
        </ Drawer>
    )
}

export {
    SiteSidebar,
    SiteOverlaySidebar
};
