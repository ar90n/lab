#include <iostream>
#include <math.h>

#define MULF(X,Y) (int32_t )((( long long )X * Y ) >> FRACS )
#define FRACS (16)
#define DOUBLE2FIXED( X ) ((uint32_t)( ( 1 << FRACS ) * X ))

class Occilator
{
    private:
        int32_t x1;
        int32_t x2;
        int32_t k;
        double freq;
    public:
        Occilator();
        ~Occilator();
        void start();
        void setFreq( double );
        void setFreq( uint32_t );
        int32_t getNext();
};

Occilator::Occilator()
{
    x1 = 0;
    x2 = 0;
    freq = 0;
}

Occilator::~Occilator( )
{
}

void Occilator::start()
{
    x1 = ( 1 << FRACS );
    x2 = k >> 1;
}

void Occilator::setFreq( double f )
{
    freq =  f;
    k = DOUBLE2FIXED( cos( 2 * M_PI * f ) );
}

int32_t Occilator::getNext()
{
    int32_t tmp1;
    int32_t tmp2;
    int32_t tmp3;

    tmp1 = MULF( k , x1 );
    tmp1 += MULF( k , x2 );

    tmp2 = x1;
    x1 = tmp1 - x2;
    x2 = tmp1 + x1;

    int32_t p;
    int32_t g;

    p = MULF( x1,x1 ) - MULF( ( (( (long long )k - ( 1 << FRACS ) ) << (FRACS >> 1 ))/(( k + ( 1 << FRACS ) ) >> ( FRACS >> 1 )  )) , MULF( x2,x2 ));
    g = ( 3 << ( FRACS - 1 ) ) - p;

    x1 = MULF( g,x1);
    x2 = MULF( g,x2);

    return x1;
}

int main(int argc, char const* argv[])
{
    Occilator os;

    os.setFreq( 0.0001 );
    os.start();

    for( int i = 0; i < 40000; i++ )
    {
        std::cout << os.getNext()  / ( 1.0 * (1 << 16) )<< std::endl;
    }

    return 0;
}
