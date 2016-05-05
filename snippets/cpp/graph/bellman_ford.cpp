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
