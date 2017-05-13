from functools import reduce
import json

def to_val( x ):
    [a,b,c] = map( float , x.split(':') )
    return round( ( a + (b / 60.0) + (c / 3600.0 ) ) * 1000 ) / 1000.0

def to_cenv( x ):
    return {
        'centroid': [to_val(x[1]),to_val(x[2])],
        'population_centroid': [to_val(x[3]),to_val(x[4])]
    }

with open( 'center.txt' ) as f:
    center_json = reduce(lambda acc,x: {**acc, **{x[0]: to_cenv(x)}}, json.load( f ), {} )

japan_json = None
with open( 'japan.json' ) as f:
    japan_json = json.load( f )
    use_pop = ['北海道','東京都', '鹿児島県', '沖縄県', '香川県' ]
    for i in range(0,len(japan_json['objects']['japan']['geometries'])):
        pref = japan_json['objects']['japan']['geometries'][i]['properties']['nam_ja']
        japan_json['objects']['japan']['geometries'][i]['properties']['centroid'] = center_json[pref]['population_centroid'] if pref in use_pop else center_json[pref]['population_centroid']

with open( 'japan.json', 'w' ) as f:
    json.dump( japan_json, f )
