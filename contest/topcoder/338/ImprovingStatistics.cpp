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
#include <float.h>
using namespace std;

class ImprovingStatistics {
public:
int howManyGames(int played, int won)
{
    long long lwon = won;
    long long lplayed = played;

    if( 99 * lplayed <= 100 * lwon )
    {
        return -1;
    }

    double rate = (double)( 100 * lwon ) / lplayed;
    double nokori = (floor( rate  + 1.0 - DBL_MIN )) - rate;
    double tasu = ( 100 * lwon - floor( rate + 1.0 - DBL_MIN ) * lplayed ) / ( floor( rate )  - 99 );

    return ceil( tasu );
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
        int played = 10;
        int won = 8;
        int expected = 1;
        ImprovingStatistics theObject;
        eq(0, theObject.howManyGames(played, won), expected);
    }
    {
        int played = 100;
        int won = 80;
        int expected = 6;
        ImprovingStatistics theObject;
        eq(1, theObject.howManyGames(played, won), expected);
    }
    {
        int played = 47;
        int won = 47;
        int expected = -1;
        ImprovingStatistics theObject;
        eq(2, theObject.howManyGames(played, won), expected);
    }
    {
        int played = 99000;
        int won = 0;
        int expected = 1000;
        ImprovingStatistics theObject;
        eq(3, theObject.howManyGames(played, won), expected);
    }
    {
        int played = 1000000000;
        int won = 470000000;
        int expected = 19230770;
        ImprovingStatistics theObject;
        eq(4, theObject.howManyGames(played, won), expected);
    }

    return 0;
}
// END CUT HERE
