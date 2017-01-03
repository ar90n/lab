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

class Chocolate {
public:
int minSplitNumber(int width, int height, int nTiles)
{
    unsigned long long  maxArea = (unsigned long long )width * (unsigned long long )height;

    if( maxArea < ( unsigned long long )nTiles )
    {
        return -1;
    }

    if( maxArea == (unsigned long long )nTiles )
    {
        return 0;
    }

    if( ( ( nTiles % width ) == 0 ) ||
        ( ( nTiles % height ) == 0 ) )
    {
        return 1;
    }

    int minlength = min( width, height );
    int maxlength = max( width, height );
    int m = (int)( sqrt( (double)1e9 ) + 0.5 );

    minlength = min( minlength, m );
    minlength = min( maxlength, m );
    for( int i = 1; i <= minlength; i++ )
    {
        if( ( ( nTiles % i ) == 0 ) &&
            ( ( nTiles / i ) <= maxlength ) )
        {
            return 2;
        }
    }

    return -1;
}

};



// BEGIN CUT HERE
#define ARRSIZE(x) (sizeof(x) / sizeof(x[0]))

template<typename T> void print(T a)
{
    cerr << a;
}

static void print(long long a)
{
    cerr << a << "L";
}

static void print(string a)
{
    cerr << '"' << a << '"';
}

template <typename T> void print(vector <T> a)
{
    cerr << "{";
    for (int i = 0; i != a.size(); i++) {
        if (i != 0) {
            cerr << ", ";
        }
        print(a[i]);
    }
    cerr << "}";
}

template <typename T> void printerror(T have, T need)
{
    cerr << "\tExpected: ";
    print(need);
    cerr << endl;
    cerr << "\tReceived: ";
    print(have);
    cerr << endl;
}

template <typename T> void eq(int n, T have, T need)
{
    if (have == need) {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    } else {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror(have, need);
    }
}

template <typename T> void eq(int n, vector <T> have, vector <T> need)
{
    if (have.size() != need.size()) {
        cerr << "Test Case #" << n << "...FAILED: ";
        cerr << "returned " << have.size() << " elements; expected " << need.size() << " elements." << endl;
        printerror(have, need);
        return;
    }

    for (int i = 0; i < have.size(); i++) {
        if (have[i] != need[i]) {
            cerr << "Test Case #" << n << "...FAILED: ";
            cerr << "expected and returned array differ in position " << i << "." << endl;
            printerror(have, need);
            return;
        }
    }

    cerr << "Test Case #" << n << "...PASSED" << endl;
}

static void eq(int n, double have, double need)
{
    if (fabs(have - need) < 1e-9 ||
        (fabs(need) >= 1 && fabs((have - need) / need) < 1e-9)) {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    } else {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror(have, need);
    }
}

static void eq(int n, string have, string need)
{
    if (have == need) {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    } else {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror(have, need);
    }
}

int main(int argc, char *argv[])
{
    {
        int width = 5;
        int height = 4;
        int nTiles = 12;
        int expected = 1;
        Chocolate theObject;
        eq(0, theObject.minSplitNumber(width, height, nTiles), expected);
    }
    {
        int width = 12;
        int height = 10;
        int nTiles = 120;
        int expected = 0;
        Chocolate theObject;
        eq(1, theObject.minSplitNumber(width, height, nTiles), expected);
    }
    {
        int width = 2;
        int height = 2;
        int nTiles = 1;
        int expected = 2;
        Chocolate theObject;
        eq(2, theObject.minSplitNumber(width, height, nTiles), expected);
    }
    {
        int width = 17;
        int height = 19;
        int nTiles = 111;
        int expected = -1;
        Chocolate theObject;
        eq(3, theObject.minSplitNumber(width, height, nTiles), expected);
    }

    {
        int width = 17;
        int height = 200;
        int nTiles = 111;
        int expected = 2;
        Chocolate theObject;
        eq(4, theObject.minSplitNumber(width, height, nTiles), expected);
    }

    {
        int width = 1e9 - 1;
        int height = 1e9- 1;
        int nTiles = 1e8;
        int expected = 2;
        Chocolate theObject;
        eq(5, theObject.minSplitNumber(width, height, nTiles), expected);
    }

    return 0;
}
// END CUT HERE
