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

