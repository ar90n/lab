import React from 'react'
import ReactGA from 'react-ga'
import {config} from 'config'
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import injectTapEventPlugin from 'react-tap-event-plugin';

ReactGA.initialize(config.googleAnalyticsId);
injectTapEventPlugin();

exports.onRouteUpdate = (state, page, pages) => {
  ReactGA.pageview(state.pathname);
};
    //</MuiThemeProvider>

exports.wrapRootComponent = (root) => () => {
  return (
    <MuiThemeProvider>
      {root()}
    </MuiThemeProvider>
  )
}
