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

class ReturnToHome {
public:
double goHome(int X, int Y, int D, int T)
{
    double dist = hypot( X, Y );
    double time;
    int count;

    count = (int)( dist / D );

    time = dist;
    time = min( time,  ( dist - D * count ) + count * T );
    time = min( time,  fabs(( dist - D * ( count + 1 ) )) + ( count + 1) * T );

    if(  0 < count )
        time = min( time,  (double)( count + 1) * T );
    else
        time = min( time,  2.0 * T );


    return time;
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
        int X = 6;
        int Y = 8;
        int D = 5;
        int T = 3;
        double expected = 6.0;
        ReturnToHome theObject;
        eq(0, theObject.goHome(X, Y, D, T), expected);
    }
    {
        int X = 3;
        int Y = 4;
        int D = 6;
        int T = 3;
        double expected = 4.0;
        ReturnToHome theObject;
        eq(1, theObject.goHome(X, Y, D, T), expected);
    }
    {
        int X = 400;
        int Y = 300;
        int D = 150;
        int T = 10;
        double expected = 40.0;
        ReturnToHome theObject;
        eq(2, theObject.goHome(X, Y, D, T), expected);
    }
    {
        int X = 318;
        int Y = 445;
        int D = 1200;
        int T = 800;
        double expected = 546.9451526432975;
        ReturnToHome theObject;
        eq(3, theObject.goHome(X, Y, D, T), expected);
    }
    {
        int X = 6;
        int Y = 8;
        int D = 3;
        int T = 2;
        double expected = 7.0;
        ReturnToHome theObject;
        eq(4, theObject.goHome(X, Y, D, T), expected);
    }
    {
        int X = 10;
        int Y = 10;
        int D = 1000;
        int T = 5;
        double expected = 10.0;
        ReturnToHome theObject;
        eq(5, theObject.goHome(X, Y, D, T), expected);
    }

    return 0;
}
// END CUT HERE
