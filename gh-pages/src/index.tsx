import React from 'react'
import ReactDOM from 'react-dom'
import { MuiThemeProvider, createMuiTheme } from 'material-ui/styles';
import 'typeface-roboto'

// Your top level component
import Root from './Root'

const theme = createMuiTheme({
  palette: {
    primary: {
        light: "#eeeeee",
        main: "#bdbdbd",
        dark: "#616161",
        contrastText: "#fff",
    },
    background: {
        paper: "#fff",
        default: "#eeeeee"
    },
  },
});

const withThemedRoot = () => (
  <MuiThemeProvider theme={theme}>
    <Root />
  </MuiThemeProvider>
)

// Export your top level component as JSX (for static rendering)
export default withThemedRoot

// Render your app
if (typeof document !== 'undefined') {
  const renderMethod = module.hot ? ReactDOM.render : ReactDOM.hydrate || ReactDOM.render
  const render = Comp => {
    renderMethod(<Comp />, document.getElementById('root'))
  }

  // Render!
  render(withThemedRoot)
}
