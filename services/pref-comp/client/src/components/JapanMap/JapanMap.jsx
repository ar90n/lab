// @flow
import React from 'react';
import {reaction} from 'mobx';
import {observer} from 'mobx-react';
import * as d3 from 'd3';
import * as topojson from 'topojson';

import * as japan from './japan'

const styles = {
  reference: {
    textAlign: 'right'
  }
}

@observer
class JapanMap extends React.Component {
  japanTotalArea: number;
  japanPrefFeatures: any;
  projection: any;
  svg: any;

  constructor( props: any, context: any ) {
    super( props, context );
    this.japanPrefFeatures = topojson.feature( japan, japan.objects.japan ).features;
    this.japanTotalArea = japan.objects.japan.area;
    this.projection = d3.geoMercator()
        .rotate([-138, -32, -35])
        .translate([1250, 750])
        .scale(4000);
  }

  calcPrefTransform( feature: any, ratios: any ) {
    const proj_center = this.projection( feature.properties.centroid );
    const area_ratio = this.japanTotalArea * ratios[ feature.properties.id ] / feature.properties.area;
    const scale = Math.sqrt( area_ratio );

    return d3.zoomIdentity
             .translate( proj_center[0], proj_center[1] )
             .scale( scale )
             .translate( -proj_center[0], -proj_center[1] );
  }

  updateMap( value ) {
    const acc_value = Object.values( value ).reduce( ( acc, val ) => acc + val, 0.0 );
    const ratios = Object.keys( value ).reduce( ( obj, key ) => {
        obj[key] = value[ key ] / acc_value;
        return obj;
    },{});

    const data = ( [...Array( this.japanPrefFeatures.length ).keys()].sort( (l,r) => {
      return value[this.japanPrefFeatures[r].properties.id] - value[this.japanPrefFeatures[l].properties.id];
    })).map( (i) => this.japanPrefFeatures[i] );

    const path = d3.geoPath().projection(this.projection);
    d3.select( this.svg ).selectAll('path')
      .data( data, ( d ) => d.properties.id )
      .enter()
       .append( 'path' )
       .attr( 'd', path )
       .attr( 'stroke', '#333' )
       .attr( 'transform', ( feature ) => this.calcPrefTransform( feature, ratios) )
       .style( 'fill', '#FFF3d5' );

    d3.select( this.svg ).selectAll('path')
      .transition()
        .duration( 750 )
        .attr( 'transform', ( feature ) => this.calcPrefTransform( feature, ratios) );
  }

  componentDidMount() {
    this.updateMap( this.props.store.current_value.value );
    reaction(
      () => this.props.store.current_value.value,
      value => this.updateMap( value )
    );
  }

  render() {
    return (
      <div>
        <svg viewBox="0 0 2500 1300" ref={d => this.svg = d}></svg>
        <p style={styles.reference}>
          本地図は地球地図日本を加工して作成しています<br />
          出典：地球地図日本(<a href="http://www.gsi.go.jp/kankyochiri/gm_jpn.html">http://www.gsi.go.jp/kankyochiri/gm_jpn.html</a>)
        </p>
      </div>
    );
  }
}
export default JapanMap;
