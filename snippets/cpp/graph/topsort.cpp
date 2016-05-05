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
