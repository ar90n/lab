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

class Arrows {
public:

int longestArrow(string s)
{
    int n = s.size();
    int lc;
    int rc;
    int sc;
    int dc;
    int len;

    lc = rc = sc = dc = 0;
    len = -1;
    for( int i = 0; i < n; i++ )
    {

        if( s[i] == '<' )
        {
            sc = dc = rc =  0;
            lc = 1;
        }
        else if ( s[i] == '-' )
        {
            if( dc != 0 )
            {
                lc = rc = dc = 0;
            }
            sc++;
        }
        else if( s[i] == '=' )
        {
            if( sc != 0 )
            {
                lc = rc = sc = 0;
            }
            dc++;
        }
        else if( s[i] == '>' )
        {
            lc = 0;
            rc = 1;
        }

        if( rc == 1  )
        {
            len = max( len , max( lc,rc ) + max( sc,dc ) );
            rc = lc = sc = dc =  0;
        }
        else if( lc == 1 )
        {
            len = max( len , max( lc,rc ) + max( sc,dc ) );
        }
    }

    return len;
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
        string s = "<--->--==>";
        int expected = 4;
        Arrows theObject;
        eq(0, theObject.longestArrow(s), expected);
    }
    {
        string s = "<<<<<<<<<<";
        int expected = 1;
        Arrows theObject;
        eq(1, theObject.longestArrow(s), expected);
    }
    {
        string s = "----==-";
        int expected = -1;
        Arrows theObject;
        eq(2, theObject.longestArrow(s), expected);
    }
    {
        string s = "<----=====>";
        int expected = 6;
        Arrows theObject;
        eq(3, theObject.longestArrow(s), expected);
    }

    return 0;
}
// END CUT HERE
