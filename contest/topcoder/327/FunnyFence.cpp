#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

class FunnyFence
{
    public:
        int getLength ( string s )
        {
		int n = s.size();
		char last = 'x';
		int pos = 0;
		int interval = -1;
		for( int i = 0; i < n; i++ )
		{
			if( last == s[i] )
			{
				interval = max( interval,i - pos );
				pos = i;
			}
			last = s[i];
		}
		interval = max( interval,n - pos );

		return interval;
        }

};



// BEGIN CUT HERE
#define ARRSIZE(x) (sizeof(x) / sizeof(x[0]))

template<typename T> void print ( T a )
{
    cerr << a;
}

static void print ( long long a )
{
    cerr << a << "L";
}

static void print ( string a )
{
    cerr << '"' << a << '"';
}

template <typename T> void print ( vector <T> a )
{
    cerr << "{";
    for ( int i = 0; i != a.size(); i++ )
    {
        if ( i != 0 )
        {
            cerr << ", ";
        }
        print ( a[i] );
    }
    cerr << "}";
}

template <typename T> void printerror ( T have, T need )
{
    cerr << "\tExpected: ";
    print ( need );
    cerr << endl;
    cerr << "\tReceived: ";
    print ( have );
    cerr << endl;
}

template <typename T> void eq ( int n, T have, T need )
{
    if ( have == need )
    {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    }
    else
    {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror ( have, need );
    }
}

template <typename T> void eq ( int n, vector <T> have, vector <T> need )
{
    if ( have.size() != need.size() )
    {
        cerr << "Test Case #" << n << "...FAILED: ";
        cerr << "returned " << have.size() << " elements; expected " << need.size() << " elements." << endl;
        printerror ( have, need );
        return;
    }

    for ( int i = 0; i < have.size(); i++ )
    {
        if ( have[i] != need[i] )
        {
            cerr << "Test Case #" << n << "...FAILED: ";
            cerr << "expected and returned array differ in position " << i << "." << endl;
            printerror ( have, need );
            return;
        }
    }

    cerr << "Test Case #" << n << "...PASSED" << endl;
}

static void eq ( int n, double have, double need )
{
    if ( fabs ( have - need ) < 1e-9 ||
         ( fabs ( need ) >= 1 && fabs ( ( have - need ) / need ) < 1e-9 ) )
    {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    }
    else
    {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror ( have, need );
    }
}

static void eq ( int n, string have, string need )
{
    if ( have == need )
    {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    }
    else
    {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror ( have, need );
    }
}

int main ( int argc, char *argv[] )
{
    {
        string s = "|-|-|";
        int expected = 5;
        FunnyFence theObject;
        eq ( 0, theObject.getLength ( s ), expected );
    }
    {
        string s = "-|-|-|-";
        int expected = 7;
        FunnyFence theObject;
        eq ( 1, theObject.getLength ( s ), expected );
    }
    {
        string s = "||||||";
        int expected = 1;
        FunnyFence theObject;
        eq ( 2, theObject.getLength ( s ), expected );
    }
    {
        string s = "|-||-|-";
        int expected = 4;
        FunnyFence theObject;
        eq ( 3, theObject.getLength ( s ), expected );
    }
    {
        string s = "|-|---|-|---|-|";
        int expected = 5;
        FunnyFence theObject;
        eq ( 4, theObject.getLength ( s ), expected );
    }
    {
        string s = "|||-||--|--|---|-||-|-|-|--||---||-||-||-|--||";
        int expected = 8;
        FunnyFence theObject;
        eq ( 5, theObject.getLength ( s ), expected );
    }

    return 0;
}
// END CUT HERE
