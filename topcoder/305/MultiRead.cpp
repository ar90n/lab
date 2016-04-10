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

class MultiRead {
public:
int minCycles(string trace, int procs)
{
    int n = trace.size();
    int rnum = 0;
    int cycle = 0;

    for( int i = 0; i < n; i++ )
    {
        if( trace[ i ]  == 'R' )
        {
            rnum++;
        }

        if( trace[ i ] == 'W' )
        {
            cycle++;
            cycle += (rnum > 0 ) ? 1 : 0;
            rnum = 0;
        }
        else if( rnum == procs )
        {
            cycle++;
            rnum = 0;
        }
    }
    if( rnum > 0 )
    {
        cycle++;
    }

    return cycle;
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
        string trace = "RWWRRR";
        int procs = 3;
        int expected = 4;
        MultiRead theObject;
        eq(0, theObject.minCycles(trace, procs), expected);
    }
    {
        string trace = "RWWRRRR";
        int procs = 3;
        int expected = 5;
        MultiRead theObject;
        eq(1, theObject.minCycles(trace, procs), expected);
    }
    {
        string trace = "WWWWW";
        int procs = 5;
        int expected = 5;
        MultiRead theObject;
        eq(2, theObject.minCycles(trace, procs), expected);
    }
    {
        string trace = "RRRRRRRRRR";
        int procs = 4;
        int expected = 3;
        MultiRead theObject;
        eq(3, theObject.minCycles(trace, procs), expected);
    }
    {
        string trace = "RWRRWWRWRWRRRWWRRRRWRRWRRWRRRRRRRRRWRWRWRRRRWRRRRR";
        int procs = 4;
        int expected = 30;
        MultiRead theObject;
        eq(4, theObject.minCycles(trace, procs), expected);
    }

    return 0;
}
// END CUT HERE
