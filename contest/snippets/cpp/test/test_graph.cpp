#include <algorithm>
#include <bitset>
#include <complex>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <new>
#include <numeric>
#include <ostream>
#include <queue>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <typeinfo>
#include <utility>
#include <valarray>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <random>
#include <ratio>
#include <regex>

template< typename T1, typename T2 >
std::ostream& operator <<( std::ostream& _os, std::pair< T1, T2 > const& _p )
{
    _os << "(" << _p.first << "," << _p.second << ")";
    return _os;
}

template< typename ContainerType,
          class = typename ContainerType::size_type >
std::ostream& operator <<( std::ostream& _os, ContainerType const& _c )
{
    using vc = typename ContainerType::value_type;
    std::for_each( std::begin( _c ),
                   std::end( _c ),
                   [&_os]( vc const v ) { _os << v << std::endl;; } );
    return _os;
}

//------------------------------------------------------------------------------

std::vector< cost > bellman_ford( graph const& _graph, vertex const _origin )
{
    int const max_bound = std::numeric_limits<int>::max() / 2;
    std::vector< cost > res( _graph.size(), max_bound );
    if( res.size() <= static_cast< std::vector< vertex >::size_type >( _origin ) )
    {
        return res;
    }

    bool is_changed = false;
    res[ _origin ] = 0;
    for( graph::size_type i = 0; i <= _graph.size(); ++i )
    {
        is_changed = false;
        for( graph::size_type from = 0; from < _graph.size(); ++from)
        {
            for( auto const& kv : _graph[from] )
            {
                vertex const to = kv.first;
                cost const c    = kv.second;
                cost const nc   = res[ from ] + c;
                if( nc < res[ to ] )
                {
                    res[ to ] = nc;
                    is_changed = true;
                }
            }
        }
    }

    if( is_changed )
    {
        throw std::runtime_error( "has cycle" );
    }

    return res;
}

std::vector< vertex > bfs( graph const& _graph, vertex const _origin )
{
    std::vector< vertex > res;
    if( res.size() <= static_cast< std::vector< vertex >::size_type >( _origin ) )
    {
        return res;
    }

    std::vector< bool > visited( _graph.size(), false );
    std::queue< vertex > queue;

    queue.push( _origin );
    while( !queue.empty() )
    {
        vertex const v = queue.front();
        queue.pop();

        if( visited[v] )
        {
            continue;
        }
        visited[v] = true;

        res.emplace_back( v );
        std::for_each( std::begin( _graph[v] ),
                       std::end( _graph[v] ),
                       [&queue]( graph::value_type::value_type const& _kv ) { queue.push( _kv.first ); });
    }

    return res;
}

std::vector< vertex > dfs( graph const& _graph, vertex const _origin )
{
    std::vector< vertex > res;
    if( res.size() <= static_cast< std::vector< vertex >::size_type >( _origin ) )
    {
        return res;
    }

    std::vector< bool > visited( _graph.size(), false );
    std::stack< vertex > stack;

    stack.push( _origin );
    while( !stack.empty() )
    {
        vertex const v = stack.top();
        stack.pop();

        if( visited[v] )
        {
            continue;
        }
        visited[v] = true;

        res.emplace_back( v );
        std::for_each( std::begin( _graph[v] ),
                       std::end( _graph[v] ),
                       [&stack]( graph::value_type::value_type const& _kv ) { stack.push( _kv.first ); });
    }

    return res;
}

std::vector< cost > dijkstra( graph const& _graph, vertex const _origin )
{
    using vertex_cost = std::tuple< vertex, cost >;

    int const max_bound = std::numeric_limits<int>::max() / 2;
    std::vector< cost > res( _graph.size(), max_bound );
    if( res.size() <= static_cast< std::vector< vertex >::size_type >( _origin ) )
    {
        return res;
    }

    std::vector< bool > visited( _graph.size(), false );
    auto cmp_func = []( vertex_cost const& _ln, vertex_cost const& _rn ) { return std::get<1>(_ln) > std::get<1>(_rn); };
    std::priority_queue< vertex_cost, std::vector< vertex_cost >, std::function< bool(vertex_cost,vertex_cost) > > queue( cmp_func );

    queue.push( vertex_cost( _origin, 0 ) );
    while(!queue.empty())
    {
        vertex const v = std::get<0>( queue.top() );
        cost const c   = std::get<1>( queue.top() );
        queue.pop();

        if( visited[v] )
        {
            continue;
        }
        visited[v] = true;

        res[v] = c;
        for( auto const& n : _graph[v] )
        {
            vertex const nv = std::get<0>( n );
            cost const nc   = std::get<1>( n );
            queue.push( vertex_cost( nv, c + nc ) );
        }
    }

    return res;
}

std::vector< std::vector< cost > > edmonds_karp( graph const& _graph, vertex const _source, vertex const _sink )
{
    if( ( _graph.size() <= static_cast< graph::size_type >( _source ) )
     || ( _graph.size() <= static_cast< graph::size_type >( _sink ) ) )
    {
        throw std::runtime_error( "invalid input" );
    }

    graph residual( _graph );
    std::vector< std::vector< cost > > flow( _graph.size(), std::vector< cost >( _graph.size(), 0 ) );
    while( true )
    {
        std::vector< vertex > path = get_path( residual, _source, _sink );
        if( path.size() == 0 )
        {
            break;
        }

        cost path_flow = std::numeric_limits< cost >::max();
        for( std::vector<vertex>::size_type i = 0; i < (path.size() - 1); ++i )
        {
            vertex const from = path[i];
            vertex const to   = path[i+1];
            path_flow = std::min( path_flow, residual[from][to] );
        }

        for( std::vector<vertex>::size_type i = 0; i < (path.size() - 1); ++i )
        {
            vertex const from = path[i];
            vertex const to   = path[i+1];

            flow[from][to] +=  path_flow;
            flow[to][from] -=  path_flow;

            residual[from][to] -= path_flow;
            if( residual[from][to] <= 0 )
            {
                residual[from].erase(to);
            }

            residual[to][from] += path_flow;
            if( residual[to][from] <= 0 )
            {
                residual[to].erase(from);
            }
        }
    }

    return flow;
}


std::vector< vertex > get_path( graph const& _graph, vertex const _src, vertex const _dst )
{
    std::vector< vertex > res;
    if( _graph.size() <= static_cast< std::vector< vertex >::size_type >( _src )
     || _graph.size() <= static_cast< std::vector< vertex >::size_type >( _dst ) )
    {
        return res;
    }

    std::vector< int > visited( _graph.size(), -1 );
    std::queue< std::tuple< vertex, vertex > > queue;

    queue.push( std::make_tuple( _src, _src ) );
    while( !queue.empty() )
    {
        vertex const from = std::get<0>( queue.front() );
        vertex const to   = std::get<1>( queue.front() );
        queue.pop();

        if( 0 <= visited[to] )
        {
            continue;
        }
        visited[to] = from;

        if( to == _dst )
        {
            vertex c = to;
            res.emplace_back( c );
            while( c != _src )
            {
                c = visited[ c ];
                res.emplace_back( c );
            }

            std::reverse( std::begin( res ), std::end( res ) );
            break;
        }

        std::for_each( std::begin( _graph[to] ),
                       std::end( _graph[to] ),
                       [&queue,to]( graph::value_type::value_type const& _kv ) { queue.push( std::make_tuple( to, _kv.first ) ); });
    }

    return res;
}

using vertex   = int;
using cost     = int;
using edge     = std::tuple< vertex, vertex, cost >;
using graph    = std::vector< std::unordered_map< vertex, cost > >;

std::vector< edge > get_edges( graph const& _graph )
{
    std::vector< edge > res;
    for( graph::size_type from = 0; from < _graph.size(); ++from )
    {
        std::for_each( std::begin( _graph[from] ),
                       std::end( _graph[from] ),
                       [&res,from]( graph::value_type::value_type const& _kv ){ res.emplace_back( edge( from, _kv.first, _kv.second ) ); } );
    }

    return res;
}

std::vector< cost > get_costs( graph const& _graph )
{
    std::vector< cost > res;
    for( auto const& neighbors : _graph )
    {
        std::for_each( std::begin( neighbors ),
                       std::end( neighbors ),
                       [&res]( graph::value_type::value_type const& _kv ){ res.emplace_back( _kv.second ); } );
    }

    return res;
}

graph build_dgraph( int const _num_of_vertices, std::vector< edge > const& _edges )
{
    graph res( _num_of_vertices );
    for( auto const& edge : _edges )
    {
        vertex const from = std::get<0>( edge );
        vertex const to   = std::get<1>( edge );
        cost const c      = std::get<2>( edge );
        res[from][to] = c;
    }

    return res;
}

graph build_graph( int const _num_of_vertices, std::vector< edge > const& _edges )
{
    std::vector< edge > edges( _edges );
    for( auto const& edge : _edges )
    {
        vertex const from = std::get<0>( edge );
        vertex const to   = std::get<1>( edge );
        cost const c      = std::get<2>( edge );
        edges.emplace_back( std::make_tuple( to, from, c ) );
    }

    return build_dgraph( _num_of_vertices, edges );
}

graph transpose( graph const& _graph )
{
    graph res( _graph.size() );
    for( graph::size_type to = 0; to < _graph.size(); ++to )
    {
        std::for_each( std::begin( _graph[to] ),
                       std::end( _graph[to] ),
                       [&res,to]( graph::value_type::value_type const& _kv ){
                           vertex const from = std::get<0>( _kv );
                           cost const c      = std::get<1>( _kv );
                           res[from][to] = c;
                       });
    }

    return res;
}

std::vector< int > get_out_degree( graph const& _graph )
{
    std::vector< int > res;
    std::transform( std::begin( _graph ),
                    std::end( _graph ),
                    std::back_inserter( res ),
                    []( graph::value_type const& _neighbors ){ return _neighbors.size(); });

    return res;
}

std::vector< int > get_in_degeree( graph const& _graph )
{
    graph tmp = transpose( _graph );
    return get_out_degree( tmp );
}

graph prim( graph const& _graph, vertex const _origin )
{
    std::vector< edge > edges;
    std::vector< bool > visited( _graph.size(), false );
    auto cmp_func = []( edge const& _ln, edge const& _rn ) { return std::get<2>(_ln) > std::get<2>(_rn); };
    std::priority_queue< edge, std::vector< edge >, std::function< bool(edge,edge) > > queue( cmp_func );

    visited[_origin] = true;
    for( auto const& kv : _graph[_origin] )
    {
        vertex const v = kv.first;
        cost const c   = kv.second;
        queue.push( edge( _origin, v, c ) );
    }

    while( !queue.empty() )
    {
        edge const e = queue.top();
        queue.pop();

        vertex const from = std::get<1>( e );
        if( visited[from] )
        {
            continue;
        }
        visited[from] = true;

        edges.emplace_back( e );
        for( auto const& n : _graph[from] )
        {
            vertex const to = std::get<0>( n );
            cost const c    = std::get<1>( n );
            queue.push( edge( from, to, c ) );
        }
    }

    graph res = build_graph( _graph.size(), edges );
    return res;
}

std::vector< vertex > top_sort( graph const& _graph )
{
    std::vector< vertex > res;
    std::vector< int > status( _graph.size(), 0 );

    std::function< void( vertex const) > visit_func = [&]( vertex const v ) {
        if( status[v] == 1 )
        {
            throw std::runtime_error("has cycle");
        }
        else if( status[v] == 0 )
        {
            status[v] = 1;
            for( auto const& kv : _graph[v] )
            {
                visit_func( kv.first );
            }
            status[v] = 2;
            res.emplace_back( v );
        }
    };

    for( graph::size_type v = 0; v < _graph.size(); ++v )
    {
        if( status[v] == 0 )
        {
            visit_func(v);
        }
    }
    std::reverse( std::begin( res ), std::end( res ) );
    return res;
}

std::vector< std::vector< cost > > warshall_floyd( graph const& _graph )
{
    int const max_bound = std::numeric_limits< int >::max() / 2;
    std::vector< std::vector< cost > > res( _graph.size(), std::vector< cost >( _graph.size(), max_bound ) );

    for( graph::size_type from = 0; from < _graph.size(); ++from )
    {
        res[ from ][ from ] = 0;
        for( auto const& kv : _graph[ from ] )
        {
            vertex const to = kv.first;
            cost const c    = kv.second;
            res[ from ][ to ] = c;
        }
    }

    for( graph::size_type k = 0; k < _graph.size(); ++k )
    {
        for( graph::size_type from = 0; from < _graph.size(); ++from )
        {
            for( graph::size_type to = 0; to < _graph.size(); ++to )
            {
                res[ from ][ to ] = std::min( res[ from ][ to ], res[ from ][ k ] + res[ k ][ to ] );
            }
        }
    }

    return res;
}

//------------------------------------------------------------------------------

int main( int args, char* argv[] )
{
    std::vector< edge > const e{ {0,1,40}, {0,2,15}, {1,0,40}, {1,2,20}, {1,3,10}, {1,4,25}, {1,5,6}, {2,0,15}, {2,1,20}, {2,3,100}, {3,1,10}, {3,2,100}, {4,1,25}, {4,5,8}, {5,1,6}, {5,4,8} };
    auto const g = build_dgraph( 6, e );
    auto const sssp = bellman_ford( g, 0 );
    auto const sssp1 = dijkstra( g, 0 );
    auto const gg = warshall_floyd( g );
    std::cout << sssp << std::endl;
    std::cout << sssp1 << std::endl;

    std::vector< edge > const e1{ {0,3,0}, {0,4,0}, {1,3,0}, {2,4,0}, {2,7,0}, {3,5,0}, {3,6,0}, {3,7,0}, {4,6,0} };
    auto const g1 = build_dgraph( 8, e1 );
    auto const rrr = top_sort( g1 );
    std::cout << rrr << std::endl;;

    std::vector< edge > const e2{ {0,1,4}, {0,2,8}, {1,3,8}, {1,2,11}, {2,4,7}, {2,5,1}, {3,4,2}, {3,6,7}, {3,7,4}, {4,5,6}, {5,7,2}, {6,7,14}, {6,8,9}, {7,8,10} };
    auto const g2 = build_graph( 9, e2 );
    auto const rrr2 = prim( g2, 0 );
    std::cout <<  rrr2 << std::endl;;

    std::vector< edge > const e3{ {0,1,16}, {0,2,13}, {1,2,10}, {1,3,12}, {2,1,4}, {2,4,14}, {3,2,9}, {3,5,20}, {4,3,7}, {4,5,4} };
    auto const g3 = build_dgraph( 6, e3 );
    auto const mf = edmonds_karp( g3, 0, 5 );
    std::cout << mf << std::endl;;

    //0,0 1,35 2,15 3,45 4,49 5,41
}