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

class PickCoins {
public:
int maxNumber(vector<string> square)
{
    int n = square.size();
    int m = square[0].size();
    int dp[n][m];
    int max_score;

    memset( dp,0x00, sizeof( int ) * ( n ) * ( m ) );

    max_score = 0;
    for( int i = 0; i < n; i ++ )
    {
        for( int j = 0; j < m; j ++ )
        {
            dp[i][j] = max(dp[ max( i - 1, 0 )][j] , dp[i][ max(j - 1,0 )]) + ( square[i][j] == 'C' );
            max_score = max( dp[i][j], max_score );
        }
    }

    return max_score;
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
        string square_array[] = {".", "C", "."};
        vector<string> square(square_array, square_array + ARRSIZE(square_array));
        int expected = 1;
        PickCoins theObject;
        eq(0, theObject.maxNumber(square), expected);
    }
	{
        string square_array[] = {"...C..C"};
        vector<string> square(square_array, square_array + ARRSIZE(square_array));
        int expected = 2;
        PickCoins theObject;
        eq(1, theObject.maxNumber(square), expected);
    }
	{
        string square_array[] = {".CC.", "C..C", ".CC.", ".C.C"};
        vector<string> square(square_array, square_array + ARRSIZE(square_array));
        int expected = 4;
        PickCoins theObject;
        eq(2, theObject.maxNumber(square), expected);
    }
	{
        string square_array[] = {".C..C", "....C", "..CC.", "...C.", "C...C"};
        vector<string> square(square_array, square_array + ARRSIZE(square_array));
        int expected = 5;
        PickCoins theObject;
        eq(3, theObject.maxNumber(square), expected);
    }
	{
        string square_array[] = {"CCCCC", "CCCCC", "CCCCC"};
        vector<string> square(square_array, square_array + ARRSIZE(square_array));
        int expected = 7;
        PickCoins theObject;
        eq(4, theObject.maxNumber(square), expected);
    }
	{
        string square_array[] = {"C"};
        vector<string> square(square_array, square_array + ARRSIZE(square_array));
        int expected = 1;
        PickCoins theObject;
        eq(5, theObject.maxNumber(square), expected);
    }

    return 0;
}
// END CUT HERE
