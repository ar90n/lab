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

int dp[ 3000 ][ 1111 ];

class ElectiveSystem {
public:
int maximalGoodness(vector<string> values, int D, int K)
{
    stringstream ss;
    for( int i = 0; i < values.size(); i++ )
    {
        ss << values[ i ] ;
    }

    string tmp;
    ss >> tmp;
    vector< int > vals( tmp.size() );
    for( int i = 0; i < vals.size(); i ++ )
    {
        vals[ i ] = tmp[i] - 'a' + 1;
    }

    int offset = 1500;
    int current_score = 0;
    memset( dp, 0x00, sizeof( int ) * 3000 * 1111 );

    for( int beg = 0; beg < vals.size(); beg++ )
    {
        current_score += vals[ beg ] - ( ( 0 <= ( beg - D ) ) ? vals[ beg - D ] : 0  );
        for( int k = 1; k <= K; k++ )
        {
            dp[ offset + beg ][ k ] = current_score + dp[ offset + beg - D ][ k - 1 ];
            dp[ offset + beg ][ k ] = max( dp[ offset + beg ][ k ], dp[ offset + beg - 1 ][ k ] );
        }
    }

    return dp[ offset + vals.size() - 1][ K ];
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
        string values_array[] = {"acacca"};
        vector<string> values(values_array, values_array + ARRSIZE(values_array));
        int D = 4;
        int K = 1;
        int expected = 10;
        ElectiveSystem theObject;
        eq(0, theObject.maximalGoodness(values, D, K), expected);
    }
    {
        string values_array[] = {"cab", "cca"};
        vector<string> values(values_array, values_array + ARRSIZE(values_array));
        int D = 2;
        int K = 2;
        int expected = 10;
        ElectiveSystem theObject;
        eq(1, theObject.maximalGoodness(values, D, K), expected);
    }
    {
        string values_array[] = {"abcdefghijkl"};
        vector<string> values(values_array, values_array + ARRSIZE(values_array));
        int D = 100;
        int K = 100;
        int expected = 78;
        ElectiveSystem theObject;
        eq(2, theObject.maximalGoodness(values, D, K), expected);
    }
    {
        string values_array[] = {"yptcsevnuzlsrfjxurpslztlinhddelpitmvaezowjcfjjfgmf", "q"};
        vector<string> values(values_array, values_array + ARRSIZE(values_array));
        int D = 2;
        int K = 18;
        int expected = 598;
        ElectiveSystem theObject;
        eq(3, theObject.maximalGoodness(values, D, K), expected);
    }
    {
        string values_array[] = {"zzhhhhhh", "zz"};
        vector<string> values(values_array, values_array + ARRSIZE(values_array));
        int D = 7;
        int K = 2;
        int expected = 152;
        ElectiveSystem theObject;
        eq(4, theObject.maximalGoodness(values, D, K), expected);
    }

    return 0;
}
// END CUT HERE
