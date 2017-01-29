// @flow
import React from 'react';
import {observer} from 'mobx-react';
import {SelectField, MenuItem} from 'material-ui';

import './loader-animation.css'

const styles = {
  field: {
    marginRight: '10px'
  },
  loader: {
    display: 'inline-block',
    position: 'relative',
    lineHeight: '72px',
    verticalAlign: 'text-bottom'
  }
};

function Category( {store, onChangeCategory, onChangeYear} ) {
  const has_year = ( store.value_state === 'fulfilled' ) && store.value.every( ( v ) => v.hasOwnProperty( 'year' ) );
  return (
    <div>
        <SelectField
          floatingLabelText="カテゴリ"
          value={store.selected_category_index}
          onChange={onChangeCategory}
          style={styles.field}
        >
          {store.category.map((v,i) => <MenuItem value={i} key={i} primaryText={v.title}/>)}
        </SelectField>
        {(() => has_year ? (
          <SelectField
            floatingLabelText="年度"
            value={store.selected_year_index}
            onChange={onChangeYear}
            style={styles.field}
          >
            {store.value.map((v,i) => <MenuItem value={i} key={i} primaryText={v.year}/>)}
          </SelectField>
        ) : null )()}
        {(() => ( store.value_state !== 'fulfilled' ) ? (
          <div style={styles.loader} >
            <span className="Loader-ellipsis" >
              <span className="Loader-ellipsisDot">.</span>
              <span className="Loader-ellipsisDot">.</span>
              <span className="Loader-ellipsisDot">.</span>
            </span>
          </div>
        ) : null )()}
    </div>
  )
}

export default observer( Category );
