#include <iostream>
#include <math.h>

class Occilator
{
    private:
        double x1;
        double x2;
        double k;
        double kk;
        double amp;
        double freq;
    public:
        Occilator();
        ~Occilator();
        void start();
        void setFreq( double );
        void setAmp( double );
        double getNext();
};

Occilator::Occilator()
{
    x1 = 0;
    x2 = 0;
    freq = 0;
    amp = 1.0;
}

Occilator::~Occilator( )
{
}

void Occilator::start()
{
    x1 = 1.0;
    x2 = k ;
}

void Occilator::setFreq( double f )
{
    freq =  f;
    k = cos( 2 * M_PI * f );
    kk = k * k;
}

void Occilator::setAmp( double a )
{
    amp = a;
}

double Occilator::getNext()
{
    double nx;
    double ny;
    double tmp;

    tmp = ( 1.0 - kk );
    nx = k * x1 + tmp * x2;
    ny = -x1 + k * x2;
    x1 = nx;
    x2 = ny;

    double p;
    double g;

    p =( x1 * x1 ) + tmp * ( x2 * x2 );
    g = 1.5 - p;

    x1 = g * x1;
    x2 = g * x2;

    return amp * x1;
}

int main(int argc, char const* argv[])
{
    Occilator os1;
    Occilator os2;

    os1.setFreq( 0.005 );
    os1.setAmp( 0.1 );
    os1.start();

    os2.setFreq( 0.1 );
    os2.setAmp( 3.5 );
    os2.start();

    for( int i = 0; i < 400; i++ )
    {
        std::cout << os1.getNext() * os2.getNext()  << std::endl;
    }

    return 0;
}
