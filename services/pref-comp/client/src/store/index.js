// @flow
import {observable, action, computed} from 'mobx';
import {RouterStore} from 'mobx-router';
import {fromPromise} from 'mobx-utils';
import endpoints from '../assets/endpoints';

class PrefInfoStore {
  //global store
  @observable value_store: any;
  @observable category_store: any;

  //local store
  @observable selected_category_index: number;
  @observable selected_year_index: number;

  constructor() {
    this.value_store = fromPromise( fetch(endpoints['area']['GET'] ).then( (res) => res.json()), [] );
    this.category_store = fromPromise( fetch(endpoints['category']['GET'] ).then((res)=>res.json()), [] );

    this.selected_category_index = 0;
    this.selected_year_index = 0;
  }

  @action
  updateCategory() {
    this.category_store = fromPromise( fetch(endpoints['category']['GET']).then( ( res ) => res.json() ), this.category );
    this.selected_category_index = 0;
  }

  @action
  selectCategory( index: number ) {
    if( this.category.length <= index ) {
      return;
    }

    this.selected_category_index = index;
    const resource = this.category[ index ].resource;
    const api_url = endpoints[resource]['GET'];
    this.value_store = fromPromise( fetch(api_url).then( (res) => res.json()), this.value );
    this.selected_year_index = 0;
  }

  @action
  selectYear( index: number ) {
    if( this.value.length <= index ) {
      return;
    }

    this.selected_year_index = index;
  }

  @computed
  get value_state() {
    return this.value_store.state;
  }

  @computed
  get value() {
    return this.value_store.value;
  }

  @computed
  get current_value() {
    return this.value[ this.selected_year_index ];
  }

  @computed
  get category_state() {
    return this.category_store.state;
  }

  @computed
  get category() {
    return this.category_store.value;
  }

  @computed
  get current_category() {
    return this.category[ this.selected_category_index ];
  }
}

const store = {
  info : new PrefInfoStore(),
  router: new RouterStore()
};

export default store;
