import React from 'react'
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import injectTapEventPlugin from 'react-tap-event-plugin';

injectTapEventPlugin();

exports.wrapRootComponent = (root) => () => {
  return (
    <MuiThemeProvider>
      {root()}
    </MuiThemeProvider>
  )
};
