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
