from bottle import route, run, static_file

from resaspy import Resaspy
import os

api_key = os.environ['RESAS_API_KEY']
cache_name = os.environ['RESAS_CACHE_NAME'] if 'RESAS_CACHE_NAME' in os.environ else None
resaspy = Resaspy( api_key, cache_name )

@route('/static/js/<filepath:re:.*\.js>')
def js(filepath):
    return static_file(filepath, root="./client/build/static/js")

@route('/')
@route('/index.html')
def index():
    return static_file("index.html", root="./client/build/")

@route('/api/category')
def category():
    from bottle import response
    from json import dumps
    rv = [
        {
            'title': '総面積',
            'resource': 'area'
        },
        {
            'title': '林業総収入',
            'resource': 'forestry_income'
        }
    ]
    response.content_type = 'application/json'
    return dumps( rv )

@route('/api/area')
def area():
    from bottle import response
    from json import dumps
    result = {
      'value': {
          '26':4612.190000,
          '41':2440.680000,
          '43':7409.350000,
          '37':1876.720000,
          '23':5172.480000,
          '9':6408.090000,
          '19':4465.270000,
          '25':4017.380000,
          '10':6362.280000,
          '4':7282.220000,
          '22':7777.420000,
          '8':6097.060000,
          '47':2281.120000,
          '6':9323.150000,
          '30':4724.690000,
          '42':4132.090000,
          '5':11637.540000,
          '33':7114.500000,
          '40':4986.400000,
          '21':10621.290000,
          '2':9645.590000,
          '27':1905.140000,
          '20':13561.560000,
          '44':6340.710000,
          '24':5774.400000,
          '34':8479.450000,
          '1':78421.260000,
          '28':8400.960000,
          '12':5157.650000,
          '16':4247.610000,
          '13':2190.930000,
          '11':3797.750000,
          '35':6112.300000,
          '7':13783.740000,
          '17':4186.090000,
          '18':4190.490000,
          '38':5676.110000,
          '29':3690.940000,
          '32':6708.240000,
          '3':15275.010000,
          '31':3507.050000,
          '36':4146.650000,
          '46':9186.940000,
          '15':12584.100000,
          '39':7103.930000,
          '45':7735.310000,
          '14':2415.830000
      }
    }

    response.content_type = 'application/json'
    return dumps( result )

@route('/api/forestry_income')
def forestry_income():
    from bottle import response
    from json import dumps
    tmp = {}
    for code in range( 1, 48 ):
        res = resaspy.forestry.income.for_stacked( code, '-' )['result']
        pref_code = res['prefCode']
        for year in res['years']:
            year_key = year['year']
            if not year_key in tmp:
                tmp[year_key] = {}
            tmp[year_key][pref_code] = year['income']

    result = []
    for year,value in zip( tmp.keys(), tmp.values() ):
        result.append({'year': int(year), 'value': value })
 
    response.content_type = 'application/json'
    return dumps( result )

run(host='0.0.0.0', port=8080, debug=True)
