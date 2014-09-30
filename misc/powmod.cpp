#include <iostream>

int powmod( int a, int k, int m )
{
    if( k == 0 )
    {
        return 1;
    }

    long long t = powmod( a, k / 2, m );

    int res = ( t * t ) % m;
    if( k % 2 == 1 )
    {
        res = ( res * a ) % m;
    }

    return res;
}

int main(int argc, char const* argv[])
{
    int res = powmod( 3, ( 1 << 31 ) - 1 , 10000 );
    std::cout << res << std::endl;
    return 0;
}
