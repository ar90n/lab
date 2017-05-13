// @flow
import React from 'react';
import {observer} from 'mobx-react';

import {AppBar, IconButton} from 'material-ui';

import './style.css'

function App( props ) {
  const right_elements = (
    <IconButton
      iconClassName="muidocs-icon-custom-github"
      href="https://github.com/ar90n/lab/tree/master/services/pref-comp"
    />
  );

  return (
    <div>
      <AppBar
      title={ props.title }
      iconElementLeft={<div />}
      iconElementRight={right_elements}
    />
      { props.children }
    </div>
  );
}

export default observer( App );
