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

class RGBStreet {
public:
int dfs( vector<int> &cost, int n, int color )
{
    if( cost.size() < n )
    {
        return 0;
    }

    int offset = 3 * n + color;
    int c;
    switch( color )
    {
        case 0:
            c = dfs( cost, n + 1, 1 );
            c = min( c, dfs( cost, n + 1, 2 ) ) ;
            break;
        case 1:
            c = dfs( cost, n + 1, 0 );
            c = min( c, dfs( cost, n + 1, 2 ) ) ;
            break;
        case 2:
            c = dfs( cost, n + 1, 0 );
            c = min( c, dfs( cost, n + 1, 1 ) ) ;
            break;
    }
    return cost[ offset ] + c;
}
int estimateCost(vector<string> houses)
{
    int n = houses.size();
    vector< int > cost;

    for( int i = 0; i < n; i++ )
    {
        stringstream ss;

        ss << houses[i];
        for( int j = 0; j < 3; j++ )
        {
            int val;
            ss >> val;

            cost.push_back( val );
        }
    }

    int c;
    c = dfs( cost, 0, 0 );
    c = min( c, dfs( cost, 0, 1 ) );
    c = min( c, dfs( cost, 0, 2 ) );

    return c;
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
        string houses_array[] = {"1 100 100", "100 1 100", "100 100 1"};
        vector<string> houses(houses_array, houses_array + ARRSIZE(houses_array));
        int expected = 3;
        RGBStreet theObject;
        eq(0, theObject.estimateCost(houses), expected);
    }
    {
        string houses_array[] = {"1 100 100", "100 100 100", "1 100 100"};
        vector<string> houses(houses_array, houses_array + ARRSIZE(houses_array));
        int expected = 102;
        RGBStreet theObject;
        eq(1, theObject.estimateCost(houses), expected);
    }
    {
        string houses_array[] = {"26 40 83", "49 60 57", "13 89 99"};
        vector<string> houses(houses_array, houses_array + ARRSIZE(houses_array));
        int expected = 96;
        RGBStreet theObject;
        eq(2, theObject.estimateCost(houses), expected);
    }
    {
        string houses_array[] = {"30 19 5", "64 77 64", "15 19 97", "4 71 57", "90 86 84", "93 32 91"};
        vector<string> houses(houses_array, houses_array + ARRSIZE(houses_array));
        int expected = 208;
        RGBStreet theObject;
        eq(3, theObject.estimateCost(houses), expected);
    }
    {
        string houses_array[] = {"71 39 44", "32 83 55", "51 37 63", "89 29 100", "83 58 11", "65 13 15", "47 25 29", "60 66 19"};
        vector<string> houses(houses_array, houses_array + ARRSIZE(houses_array));
        int expected = 253;
        RGBStreet theObject;
        eq(4, theObject.estimateCost(houses), expected);
    }

    return 0;
}
// END CUT HERE
