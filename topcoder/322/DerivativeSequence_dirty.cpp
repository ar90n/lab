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

class DerivativeSequence {
public:
vector<int> derSeq(vector<int> a, int n)
{
    int m = a.size();
    vector< vector< int > > de( 21, vector< int > ( 21 ) );

    copy( a.begin(), a.end(), de[0].begin() );

    for( int j = 1; j <= n; j++ )
    {
        for( int i = 1; i <= m - j; i++ )
        {
            de[j][ i - 1 ] = de[j - 1][ i ] - de[j - 1][ i - 1 ];
        }
    }

    vector< int > result( m - n );
    for( int i = 0; i < result.size(); i++ )
    {
        result[ i ] = de[ n ][i];
    }

    return result;
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
        int a_array[] = {5,6,3,9,-1};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int n = 1;
        int expected_array[] = {1, -3, 6, -10 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        DerivativeSequence theObject;
        eq(0, theObject.derSeq(a, n), expected);
    }
    {
        int a_array[] = {5,6,3,9,-1};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int n = 2;
        int expected_array[] = {-4, 9, -16 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        DerivativeSequence theObject;
        eq(1, theObject.derSeq(a, n), expected);
    }
    {
        int a_array[] = {5,6,3,9,-1};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int n = 4;
        int expected_array[] = {-38 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        DerivativeSequence theObject;
        eq(2, theObject.derSeq(a, n), expected);
    }
    {
        int a_array[] = {4,4,4,4,4,4,4,4};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int n = 3;
        int expected_array[] = {0, 0, 0, 0, 0 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        DerivativeSequence theObject;
        eq(3, theObject.derSeq(a, n), expected);
    }
    {
        int a_array[] = {-100,100};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int n = 0;
        int expected_array[] = {-100, 100 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        DerivativeSequence theObject;
        eq(4, theObject.derSeq(a, n), expected);
    }

    return 0;
}
// END CUT HERE
