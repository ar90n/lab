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
