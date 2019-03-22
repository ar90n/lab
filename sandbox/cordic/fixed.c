#include <stdio.h>
#include <math.h>

typedef unsigned int uint32_t;
typedef int int32_t;
typedef unsigned long long  uint64_t;
typedef long long  int64_t;

uint32_t thetas[] = {51472, 30386, 16055, 8160, 4091, 2047, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1};
uint32_t hypot_length = 39797;

int32_t cos_cordic( int32_t angle )
{
    int i;
    int32_t x = -hypot_length;
    int32_t y = 0;
    int32_t acc_theta = 0;
    int32_t sign;

    if( angle < ( ( 1 << 16 ) * ( M_PI ) ) )
    {
    }
    else
    {
        angle = ( 2 << 16 ) * ( M_PI  ) - angle;
    }

    angle -= ( 1 << 16 ) * ( M_PI / 2.0 );
    for( i = 0; i < sizeof( thetas ) / sizeof( thetas[0] ); i++ )
    {
        int32_t x1 = x;
        int32_t y1 = y;

        if( acc_theta < angle )
        {
            acc_theta += thetas[ i ];
            x -= y1 >> i;
            y += x1 >> i;
        }
        else
        {
            acc_theta -= thetas[ i ];
            x += y1 >> i;
            y -= x1 >> i;
        }
    }

    return y;
}

int main(int argc, char const* argv[])
{
    int i;
    for( i = 0; i < 200; i++ )
    {
        uint32_t theta;

        theta = (int32_t)( ( M_PI / 100.0 * i ) * ( 1 << 16 ) + 0.5 );
        printf("%d %f\n",theta,cos_cordic( theta ) / 65535.0);
    }
    return 0;
}
