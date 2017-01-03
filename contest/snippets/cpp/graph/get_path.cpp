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
