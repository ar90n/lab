//i referred to m-kobayashi
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

class Touchdown {
int memo[ 1 << 15 ];
public:
int howMany(int yardsToGo, vector<int> plays)
{
    memset( memo, 0x00, sizeof(int) * ( 1 << 15  ));
    int ret = doit( 0, 0, 0, 0, 4, yardsToGo, plays );
    return ( ret == ( 1 << 29 ) ) ? -1 : ret;
}

int doit( int n, int total, int current, int mask, int cnt, int yardsToGo, vector<int> &plays )
{
    if( ( yardsToGo  <= total ) && ( total <= ( yardsToGo + 10 ) ) )
    {
        return n;
    }

    if( 10 <= current )
    {
        if( memo[ mask ] == 0 )
        {
            memo[ mask ] = doit( n, total, 0, mask, 4, yardsToGo, plays );
        }
        return memo[ mask ] ;
    }

    if( cnt == 0 )
    {
        return 1 << 29;
    }

    int ret = 1 << 29;
    for( int i = 0; i < plays.size(); i ++ )
    {
        if( ( mask  & ( 1 << i ) ) == 0 )
        {
            ret = min (ret , doit( n + 1, total + plays[i], current + plays[i], mask | ( 1 << i ), cnt - 1,  yardsToGo, plays ));
        }
    }

    return ret;
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
        int yardsToGo = 25;
        int plays_array[] = {2, 2, 3, 2, 3, 3, 1, 2, 1, 4, 2};
        vector<int> plays(plays_array, plays_array + ARRSIZE(plays_array));
        int expected = 11;
        Touchdown theObject;
        eq(0, theObject.howMany(yardsToGo, plays), expected);
    }
    {
        int yardsToGo = 60;
        int plays_array[] = {20, 20, 33, 39, 59, 59};
        vector<int> plays(plays_array, plays_array + ARRSIZE(plays_array));
        int expected = -1;
        Touchdown theObject;
        eq(1, theObject.howMany(yardsToGo, plays), expected);
    }
    {
        int yardsToGo = 13;
        int plays_array[] = {4, 4, 4, 2, 2, 1, 1};
        vector<int> plays(plays_array, plays_array + ARRSIZE(plays_array));
        int expected = 4;
        Touchdown theObject;
        eq(2, theObject.howMany(yardsToGo, plays), expected);
    }
    {
        int yardsToGo = 25;
        int plays_array[] = {7, 4, 4, 3, 1, 1, 1, 1, 1, 1, 1};
        vector<int> plays(plays_array, plays_array + ARRSIZE(plays_array));
        int expected = 11;
        Touchdown theObject;
        eq(3, theObject.howMany(yardsToGo, plays), expected);
    }
    {
        int yardsToGo = 12;
        int plays_array[] = {2, 2, 2, 2, 2, 2};
        vector<int> plays(plays_array, plays_array + ARRSIZE(plays_array));
        int expected = -1;
        Touchdown theObject;
        eq(4, theObject.howMany(yardsToGo, plays), expected);
    }
    return 0;
}
// END CUT HERE
