// @flow
import React from 'react';
import ReactDOM from 'react-dom';
import {MobxRouter, startRouter} from 'mobx-router';
import {Provider} from 'mobx-react';

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import injectTapEventPlugin from 'react-tap-event-plugin';

import store from './store';
import views from './views';
import { App, JapanMap } from './components';
import constants from './assets/constants';

injectTapEventPlugin();
startRouter(views, store);

ReactDOM.render(
  <MuiThemeProvider>
    <Provider store={store}>
      <App title={constants.title}>
        <MobxRouter/>
        <JapanMap />
      </App>
    </Provider>
  </MuiThemeProvider>, document.getElementById('root')
);
