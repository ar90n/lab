from bottle import route, run, static_file

from resaspy import Resaspy
import os

api_key = os.environ['RESAS_API_KEY']
cache_name = os.environ['RESAS_CACHE_NAME'] if 'RESAS_CACHE_NAME' in os.environ else None
resaspy = Resaspy( api_key, cache_name )

@route('/static/<filepath:path>')
def js(filepath):
    return static_file(filepath, root="./client/build/static/")

@route('/')
@route('/index.html')
def index():
    return static_file("index.html", root="./client/build/")

def create_response( payload ):
    from bottle import response
    from json import dumps
    response.content_type = 'application/json'
    response.headers['Access-Control-Allow-Origin'] = '*'
    response.headers['Access-Control-Allow-Methods'] = 'GET, POST, PUT, OPTIONS'
    response.headers['Access-Control-Allow-Headers'] = 'Origin, Accept, Content-Type, X-Requested-With, X-CSRF-Token'
    return dumps( payload )

def convert_to_payload( tmp ):
    payload = []
    for year,value in zip( tmp.keys(), tmp.values() ):
        payload.append({'year': (year), 'value': value })

    return payload

def fetch_manicipality_resource( fetcher ):
    tmp = {}
    for pref_code in range( 1, 48 ):
        res = fetcher( pref_code )
        for data in res['data']:
            year_key = data['year']
            if not year_key in tmp:
                tmp[year_key] = {}
            tmp[year_key][pref_code] = data['value']

    return tmp

@route('/api/category')
def category():
    payload = [
        {
            'title': '総面積',
            'resource': 'area'
        },
        {
            'title': '林業総収入',
            'resource': 'forestry_income'
        },
        {
            'title': '海面漁獲物販売金額',
            'resource': 'fishery_sales'
        },
        {
            'title': '海面養殖物販売金額',
            'resource': 'aquaculture_sales'
        },
        {
            'title': '企業数',
            'resource': 'companies'
        },
        {
            'title': '事業所数',
            'resource': 'plants'
        },
        {
            'title': '創業比率',
            'resource': 'foundation'
        },
        {
            'title': '一人当たり地方税',
            'resource': 'taxes'
        },
        {
            'title': '有効求人倍率',
            'resource': 'job'
        },
        {
            'title': '製造品出荷額',
            'resource': 'manufacture'
        },
        {
            'title': '従業者数（事業所単位）',
            'resource': 'employee'
        },
        {
            'title': '付加価値額（企業単位）',
            'resource': 'value'
        },
        {
            'title': '労働生産性（企業単位）',
            'resource': 'labor'
        },
        {
            'title': '黒字赤字企業比率',
            'resource': 'surplus'
        },
        {
            'title': '一人当たり賃金',
            'resource': 'wages'
        },
        {
            'title': '年間商品販売額',
            'resource': 'sales'
        }
    ]

    return create_response( payload )

@route('/api/area')
def area():
    payload = {
      'value': {
          '26':4612.190000, '41':2440.680000, '43':7409.350000, '37':1876.720000, '23':5172.480000, '9':6408.090000, '19':4465.270000, '25':4017.380000,
          '10':6362.280000, '4':7282.220000, '22':7777.420000, '8':6097.060000, '47':2281.120000, '6':9323.150000, '30':4724.690000, '42':4132.090000,
          '5':11637.540000, '33':7114.500000, '40':4986.400000, '21':10621.290000, '2':9645.590000, '27':1905.140000, '20':13561.560000, '44':6340.710000,
          '24':5774.400000, '34':8479.450000, '1':78421.260000, '28':8400.960000, '12':5157.650000, '16':4247.610000, '13':2190.930000, '11':3797.750000,
          '35':6112.300000, '7':13783.740000, '17':4186.090000, '18':4190.490000, '38':5676.110000, '29':3690.940000, '32':6708.240000, '3':15275.010000,
          '31':3507.050000, '36':4146.650000, '46':9186.940000, '15':12584.100000, '39':7103.930000, '45':7735.310000, '14':2415.830000
      }
    }

    return create_response( [ payload ] )

@route('/api/companies')
def companies():
    fetcher = lambda pref_code: resaspy.municipality.company.per_year( pref_code, '-', '-', '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/plants')
def plants():
    fetcher = lambda pref_code: resaspy.municipality.plant.per_year( pref_code, '-', '-', '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/foundation')
def foundation():
    fetcher = lambda pref_code: resaspy.municipality.foundation.per_year( pref_code, '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/taxes')
def taxes():
    fetcher = lambda pref_code: resaspy.municipality.taxes.per_year( pref_code, '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/job')
def job():
    fetcher = lambda pref_code: resaspy.municipality.job.per_year( pref_code, '-', '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/manufacture')
def manufacture():
    fetcher = lambda pref_code: resaspy.municipality.manufacture.per_year( pref_code, '-', '-', '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/employee')
def employee():
    fetcher = lambda pref_code: resaspy.municipality.employee.per_year( pref_code, '-', '-', '-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/value')
def value():
    fetcher = lambda pref_code: resaspy.municipality.value.per_year( year=2012, pref_code=pref_code, city_code='-', sic_code='-', simc_code='-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/labor')
def labor():
    fetcher = lambda pref_code: resaspy.municipality.labor.per_year( year=2012, pref_code=pref_code, city_code='-', sic_code='-', simc_code='-' )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/surplus')
def surplus():
    tmp = {}
    for pref_code in range( 1, 48 ):
        res = resaspy.municipality.surplus.per_year( year=2012, pref_code=pref_code, city_code='-', sic_code='-', simc_code='-' )['result']
        for data in res['data']:
            if data['name'] != 'surplus':
                continue

            for years in data['years']:
                year_key = years['year']
                if not year_key in tmp:
                    tmp[year_key] = {}
                tmp[year_key][pref_code] = years['value']

    payload = convert_to_payload( tmp )
    return create_response( payload )

@route('/api/wages')
def wages():
    fetcher = lambda pref_code: resaspy.municipality.wages.per_year( pref_code, sic_code='-', simc_code='-', wages_age=1 )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/sales')
def sales():
    fetcher = lambda pref_code: resaspy.municipality.sales.per_year( pref_code, '-', '-', '-', 1 )['result']
    payload = convert_to_payload( fetch_manicipality_resource( fetcher ) )
    return create_response( payload )

@route('/api/forestry_income')
def forestry_income():
    tmp = {}
    for pref_code in range( 1, 48 ):
        res = resaspy.forestry.income.for_stacked( pref_code, '-' )['result']
        for year in res['years']:
            year_key = year['year']
            if not year_key in tmp:
                tmp[year_key] = {}
            tmp[year_key][pref_code] = year['income']

    payload = convert_to_payload( tmp )
    return create_response( payload )

@route('/api/fishery_sales')
def fishery_sales():
    tmp = {}
    for pref_code in range( 1, 48 ):
        res = resaspy.fishery.sea.total_sales( pref_code, '-' )['result']
        if res is not None:
            for year in res['years']:
                year_key = year['year']
                if not year_key in tmp:
                    tmp[year_key] = {}
                tmp[year_key][pref_code] = year['value']
        else:
            for year_key in tmp.keys():
                tmp[year_key][pref_code] = 0

    payload = convert_to_payload( tmp )
    return create_response( payload )

@route('/api/aquaculture_sales')
def aquaculture_sales():
    tmp = {}
    for pref_code in range( 1, 48 ):
        res = resaspy.fishery.sea.aquaculture_total_sales( pref_code, '-' )['result']
        if res is not None:
            for year in res['years']:
                year_key = year['year']
                if not year_key in tmp:
                    tmp[year_key] = {}
                tmp[year_key][pref_code] = year['value']
        else:
            for year_key in tmp.keys():
                tmp[year_key][pref_code] = 0

    payload = convert_to_payload( tmp )
    return create_response( payload )

run(host='0.0.0.0', port=8080, debug=True)
