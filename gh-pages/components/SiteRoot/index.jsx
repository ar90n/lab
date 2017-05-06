import React, { PropTypes } from 'react'
import Helmet from 'react-helmet'
import { config } from 'config'
import SitePost from '../SitePost'
import { SiteSidebar, SiteOverlaySidebar } from '../SiteSidebar'
import SiteAppbar from '../SiteAppbar'
import SiteContents from '../SiteContents'
import spacing from 'material-ui/styles/spacing'
import * as colors from 'material-ui/styles/colors'
import withWidth, { SMALL } from 'material-ui/utils/withWidth'

const styles = {
appbar: {
        position: 'fixed',
    },
    drawer: {
        position: 'fixed',
    },
    drawerContainer: {
        width: '256px'
    },
    main: {
        marginTop: `${ spacing.desktopGutter * 2 }px`,
        marginBottom: `${ spacing.desktopGutter * 2 }px`
    },
    footer: {
        //    backgroundColor: grey900,
        textAlign: 'center'
    },
};

class SiteRoot extends React.Component {
    static contextTypes = {
        muiTheme: PropTypes.object,
        router: PropTypes.object
    };

    //
    //SiteIndex.propTypes = {
    //    route : React.PropTypes.object
    //}
    constructor( props )
    {
        super( props );
        this.props = props;
        const forSmallDisplay = props.width === SMALL
        this.state = {
            drawerOpen: !forSmallDisplay,
            overlayDrawerOpen: false,
            forSmallDisplay
        };
    }

    onTouchTapLeftIconButton = ( ) => {
        const value = this.state.forSmallDisplay
            ? this.state.overlayDrawerOpen
            : this.state.drawerOpen;
        this.onChangeRequestDrawer( !value )
    };

    onChangeRequestDrawer = ( open ) => {
        const key = this.state.forSmallDisplay
            ? 'overlayDrawerOpen'
            : 'drawerOpen';
        this.setState({ [ key ]: open });
    };

    onChangePage = ( event, value ) => {
        this.onChangeRequestDrawer( false );
        if(!this.context.router.isActive(value))
        {
            this.context.router.push( value );
        }
    }

    onCloseOverlayDrawer = ( ) => {
        this.setState({ overlayDrawerOpen: false });
    }

    componentWillMount( ) {
        document.body.style.backgroundColor = colors[config.siteBackgroundColor];
    }

    componentWillReceiveProps( nextProps ) {
        const forSmallDisplay = nextProps.width === SMALL
        const drawerOpen = !forSmallDisplay && ( this.state.forSmallDisplay || this.state.drawerOpen )
const overlayDrawerOpen = forSmallDisplay && this.state.overlayDrawerOpen
    this.setState({ drawerOpen, overlayDrawerOpen, forSmallDisplay });
    }

    componentWillUnmount( ) {
        document.body.style.backgroundColor = null;
    }

    render( ) {
        let { drawerOpen, overlayDrawerOpen, forSmallDisplay } = this.state;
        const appStyle = Object.assign({}, styles.appbar, {
            zIndex: this.context.muiTheme.zIndex.appBar + 1
        })
        const drawerStyle = styles.drawer;
        const overlayDrawerStyle = Object.assign({}, styles.drawer, {
            zIndex: appStyle.zIndex + 1
        })
        const drawerContainerStyle = Object.assign({},styles.drawerContainer,{
            paddingTop: this.context.muiTheme.appBar.height
        })
        const overlayDrawerContainerStyle = styles.drawerContainer
        const mainStyle = Object.assign({}, styles.main, {
            paddingLeft: ( drawerOpen && !forSmallDisplay ) ? parseInt( styles.drawerContainer.width ) : 0,
            paddingTop: this.context.muiTheme.appBar.height,
            marginLeft: forSmallDisplay ? '8px': `${ spacing.desktopGutter * 3 }px`,
            marginRight: forSmallDisplay ? '8px': `${ spacing.desktopGutter * 3 }px`,
        })

        return (
            <div>
                <Helmet title={config.siteTitle}/>
                <SiteAppbar style={appStyle} onLeftIconButtonTouchTap={this.onTouchTapLeftIconButton} showMenuIconButton={forSmallDisplay}/>
                <SiteSidebar style={drawerStyle} containerStyle={drawerContainerStyle} open={drawerOpen} onChange={this.onChangePage}/>
                <SiteOverlaySidebar style={overlayDrawerStyle} containerStyle={overlayDrawerContainerStyle} appbarHeight={this.context.muiTheme.appBar.height} open={overlayDrawerOpen} docked={false} onChange={this.onChangePage} onClose={this.onCloseOverlayDrawer}/>
                <SiteContents style={mainStyle}>
                    {this.props.children}
                </SiteContents>
            </ div>
        )
    }
}

const options = {
    largeWidth: 992,
    mediumWidth: 992
};
export default withWidth( options )( SiteRoot );
