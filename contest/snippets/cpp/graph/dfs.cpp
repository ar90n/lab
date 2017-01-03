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
