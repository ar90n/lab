// @flow
import React from 'react';
import {Route} from 'mobx-router';

import MapView from './MapView';

function create_views( store ) {
  const views = {
    map: new Route({
      path: '/',
      component: <MapView />,
      onEnter: () => {
        store.info.updateCategory();
      }
    }),
    default: new Route({
      path: '/(.*)',
      component: <div />,
      onEnter: () => {
        store.router.goTo( views.map );
      }
    })
  }

  return views;
}

export default create_views;
