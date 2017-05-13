// @flow
import React from 'react';
import {observer,inject} from 'mobx-react';
import {CircularProgress,Paper} from 'material-ui';

import {Category,JapanMap} from '../components';

const styles = {
  loading: {
    display: 'flex',
    justifyContent: 'center',
    position: 'fixed',
    alignItems: 'center',
    flexDirection: 'row',
    width: '100%',
    height: '100%'
  },
  loadingText: {
    textAlign: 'center',
    marginLeft: '36px',
    fontSize: '36px'
  },
  category: {
    marginLeft: '16px',
  },
  mainPaper: {
    margin: '16px'
  }
};

function MapView( props ) {
    if( (props.store.info.category_state !== 'fulfilled' ) || (props.store.info.value === []) ) {
        return (
          <div style={styles.loading}>
            <CircularProgress size={64} thickness={5}/>
            <p style={styles.loadingText} >Loading ...</p>
          </div>
        );
    }
    else {
        return (
          <Paper style={styles.mainPaper} zDepth={2}>
            <div style={styles.category}>
              <Category
                store={props.store.info} 
                onChangeCategory={(e,i,v)=>props.store.info.selectCategory(v)}
                onChangeYear={(e,i,v)=>props.store.info.selectYear(v)}
              />
            </div>
            <JapanMap store={props.store.info}/>
          </Paper>
        );
    }
}

export default inject('store')(observer( MapView ));
