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
