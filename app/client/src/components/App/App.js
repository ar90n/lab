// @flow
import React from 'react';
import {observer, inject} from 'mobx-react';
//import DevTools from 'mobx-react-devtools';

import {AppBar, FlatButton } from 'material-ui';

function App( props ) {
  return (
      <div>
        <AppBar
        title={ props.title }
        iconElementLeft={<div />}
        iconElementRight={<div />}
        />
        { props.children }
      </div>
    );
}

//@inject('state')
export default observer( App );
