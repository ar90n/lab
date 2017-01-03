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
