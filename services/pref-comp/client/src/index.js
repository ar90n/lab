// @flow
import React from 'react';
import ReactDOM from 'react-dom';
import {MobxRouter, startRouter} from 'mobx-router';
import {Provider} from 'mobx-react';

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import injectTapEventPlugin from 'react-tap-event-plugin';

import store from './store';
import create_views from './views';
import { App } from './components';
import constants from './assets/constants';

injectTapEventPlugin();
startRouter(create_views(store), store);

ReactDOM.render(
  <MuiThemeProvider>
    <Provider store={store}>
      <App title={constants.title}>
        <MobxRouter/>
      </App>
    </Provider>
  </MuiThemeProvider>, document.getElementById('root')
);
