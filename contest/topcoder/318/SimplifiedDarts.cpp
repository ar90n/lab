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

class SimplifiedDarts {
public:
double tryToWin(int W, int N, int P1, int P2)
{
    vector< vector< double > > dp( W + 1, vector<double>(N + 1) );
    double prob_short = P1 / 100.0;
    double prob_long = P2 / 100.0;

    for( int i = 0; i < dp.size() ;i++)
    {
        for(int j = 0; j < dp[i].size();j++)
        {
            dp[i][j] = 0.0;
        }
    }

    for( int i = 0; i < dp.size();i++)
    {
        dp[i][0] = 0.0;
    }

    for( int i = 0; i < dp[0].size() ;i++)
    {
        dp[0][i] = 1.0;
    }

    for( int i = 1; i < dp.size(); i++ )
    {
        for( int j = 1; j < dp[i].size(); j++ )
        {
            double probably;
            double prob2 = ( ( i - 2 ) < 0 ) ? 1.0 : dp[ i - 2 ][ j - 1 ];
            double prob3 = ( ( i - 3 ) < 0 ) ? 1.0 : dp[ i - 3 ][ j - 1 ];

            probably = prob_short * prob2 + ( 1.0 - prob_short ) * dp[i][j - 1];
            probably = max( probably, prob_long * prob3 + ( 1.0 - prob_long ) * dp[i][j -1] );

            dp[i][j] = probably;
        }
    }

    return dp[ W ][ N ] * 100.0;
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
        int W = 5;
        int N = 2;
        int P1 = 50;
        int P2 = 25;
        double expected = 12.5;
        SimplifiedDarts theObject;
        eq(0, theObject.tryToWin(W, N, P1, P2), expected);
    }
    {
        int W = 6;
        int N = 3;
        int P1 = 90;
        int P2 = 20;
        double expected = 73.30000000000001;
        SimplifiedDarts theObject;
        eq(1, theObject.tryToWin(W, N, P1, P2), expected);
    }
    {
        int W = 30;
        int N = 384;
        int P1 = 3;
        int P2 = 1;
        double expected = 18.344490479047746;
        SimplifiedDarts theObject;
        eq(2, theObject.tryToWin(W, N, P1, P2), expected);
    }
    {
        int W = 999;
        int N = 333;
        int P1 = 0;
        int P2 = 100;
        double expected = 100.0;
        SimplifiedDarts theObject;
        eq(3, theObject.tryToWin(W, N, P1, P2), expected);
    }
    {
        int W = 1000;
        int N = 333;
        int P1 = 0;
        int P2 = 100;
        double expected = 0.0;
        SimplifiedDarts theObject;
        eq(4, theObject.tryToWin(W, N, P1, P2), expected);
    }

    return 0;
}
// END CUT HERE
