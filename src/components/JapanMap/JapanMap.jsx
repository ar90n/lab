// @flow
import React from 'react';
import {observer, inject} from 'mobx-react';
//import DevTools from 'mobx-react-devtools';
import * as d3 from 'd3';
import * as topojson from 'topojson';

import * as japan from './japan'

function flatten(array) {
  return Array.isArray(array) ? [].concat.apply([], array.map(flatten)) : array;
}

class JapanMap extends React.Component {
  japanTotalArea: number;
  japanPrefFeatures: any;
  projection: any;
  svg: any;

  constructor( props, context ) {
    super( props, context );
    this.japanPrefFeatures = topojson.feature( japan, japan.objects.japan ).features;
    this.japanTotalArea = japan.objects.japan.area;
    this.projection = d3.geoMercator()
        .center([137, 34])
        .translate([1250, 750])
        .scale(2400);
  }

  calcPrefTransform( feature, ratios ) {
    const flat_coordinates = flatten( feature.geometry.coordinates );
    const num_of_coordinates = flat_coordinates.length / 2;

    const [accX, accY] = flat_coordinates.reduce( (a,b,i) => {
        a[i%2] += b;
        return a;
    },[0,0]);
    const center = [ accX / num_of_coordinates, accY / num_of_coordinates ];
    const proj_center = this.projection( center );

    const area_ratio = this.japanTotalArea * ratios[ feature.properties.id ] / feature.properties.area;
    const scale = Math.sqrt( area_ratio );

    return d3.zoomIdentity
             .translate( proj_center[0], proj_center[1] )
             .scale( scale )
             .translate( -proj_center[0], -proj_center[1] );
  }

  updateMap( info ) {
    const acc_value = Object.values( info.info_records ).reduce( ( acc, val ) => acc + val, 0.0 );
    const ratios = Object.keys( info.info_records ).reduce( ( obj, key ) => {
        obj[key] = info.info_records[ key ] / acc_value;
        return obj;
    },{});

    const path = d3.geoPath().projection(this.projection);
    const svg = d3.select('svg').selectAll('path')
        .data( this.japanPrefFeatures )
        .enter()
        .append( 'path' )
        .attr( 'd', path )
        .attr( 'stroke', '#333' )
        .attr( 'transform', ( feature ) => this.calcPrefTransform( feature, ratios) )
        .style( 'fill', '#FFF3d5' );
  }

  componentDidMount() {
      this.updateMap( this.props.store.info );
  }

  componentDidUpdate( props ) {
      this.updateMap( props.store.info );
  }

  render() {
    const width = this.props.width;
    const height = this.props.height;
    return <svg viewBox="0 0 2500 1500" ></svg>;
  }
}
export default inject( 'store' )( observer( JapanMap ) );
