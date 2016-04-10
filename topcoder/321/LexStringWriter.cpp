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

class LexStringWriter {
public:
int minMoves(string s)
{
    vector< pair< int, int > > dp(27);
    pair< int, int > last;

    dp[ 0 ] = pair<int, int>( 0, 0 );
    last = pair< int, int >( 0, 0 );
    for( int i = 1; i < dp.size(); i++ )
    {
        int left;
        int right;
        int count;
        char current = ( i - 1 ) + 'a';

        left = -1;
        right = -1;
        count = 0;
        for( int j = 0; j < s.size(); j++ )
        {
            if( s[ j ] == current )
            {
                if( left == - 1 )
                {
                    left = j;
                }

                right = j;
                count ++;
            }
        }

        if( left == -1 )
        {
            dp[ i ] = dp[ i - 1 ];
            continue;
        }

        int range = abs( right - left );
        int left_right = min( abs( left - last.first ) + dp[ i - 1 ].first, abs( left - last.second ) + dp[ i - 1].second ) + range +  count;
        int right_left = min( abs( right - last.first ) + dp[ i - 1 ].first, abs( right - last.second ) + dp[ i - 1].second ) + range +  count;

        dp[ i ] = pair< int, int >( left_right, right_left );
        last = pair< int, int >( right, left );
    }


    return min( dp[ 26 ].first, dp[ 26 ].second );

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
        string s = "aaa";
        int expected = 5;
        LexStringWriter theObject;
        eq(0, theObject.minMoves(s), expected);
    }
    {
        string s = "ba";
        int expected = 4;
        LexStringWriter theObject;
        eq(1, theObject.minMoves(s), expected);
    }
    {
        string s = "abba";
        int expected = 9;
        LexStringWriter theObject;
        eq(2, theObject.minMoves(s), expected);
    }
    {
        string s = "acbbc";
        int expected = 12;
        LexStringWriter theObject;
        eq(3, theObject.minMoves(s), expected);
    }

    return 0;
}
// END CUT HERE
