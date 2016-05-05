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
