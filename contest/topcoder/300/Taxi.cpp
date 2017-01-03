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

class Taxi {
public:
double maxDis(int maxX, int maxY, int taxiDis)
{
    if( ( maxX + maxY ) < taxiDis )
    {
        return -1;
    }

    if( ( maxX >= taxiDis ) || ( taxiDis <= maxY ) )
    {
        return taxiDis;
    }

    int ln = max( maxX, maxY );
    int a = ln;
    int b = taxiDis - ln;
    return hypot( a, b );
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
        int maxX = 10;
        int maxY = 3;
        int taxiDis = 3;
        double expected = 3.0;
        Taxi theObject;
        eq(0, theObject.maxDis(maxX, maxY, taxiDis), expected);
    }
    {
        int maxX = 10;
        int maxY = 3;
        int taxiDis = 24;
        double expected = -1.0;
        Taxi theObject;
        eq(1, theObject.maxDis(maxX, maxY, taxiDis), expected);
    }
    {
        int maxX = 7;
        int maxY = 10;
        int taxiDis = 13;
        double expected = 10.44030650891055;
        Taxi theObject;
        eq(2, theObject.maxDis(maxX, maxY, taxiDis), expected);
    }
    {
        int maxX = 4;
        int maxY = 4;
        int taxiDis = 7;
        double expected = 5.0;
        Taxi theObject;
        eq(3, theObject.maxDis(maxX, maxY, taxiDis), expected);
    }
    {
        int maxX = 976421;
        int maxY = 976421;
        int taxiDis = 1000000;
        double expected = 976705.6560100387;
        Taxi theObject;
        eq(4, theObject.maxDis(maxX, maxY, taxiDis), expected);
    }

    return 0;
}
// END CUT HERE
