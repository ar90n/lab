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

class CommonMultiples {
public:

int gcd( int a, int b )
{
    int r = a % b;

    if( r == 0 )
        return b;
    else
        return gcd( b, r );
}
int countCommMult(vector<int> a, int lower, int upper)
{
    int n = a.size();

    long long  comul = a[0];
    for( int i = 1; i < n ; i ++ )
    {
        int codiv = gcd( comul, a[i] );
        comul = comul * a[i] / codiv;

        if( upper < comul )
            return 0;
    }

    long long beg  = comul * (int)( ceil( (double)lower / comul ) );
    if( upper < beg )
        return 0;

    int num = ( upper - beg ) / comul + 1;

    return num;
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
        int a_array[] = {1,2,3};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int lower = 5;
        int upper = 15;
        int expected = 2;
        CommonMultiples theObject;
        eq(0, theObject.countCommMult(a, lower, upper), expected);
    }
    {
        int a_array[] = {1,2,4,8,16,32,64};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int lower = 128;
        int upper = 128;
        int expected = 1;
        CommonMultiples theObject;
        eq(1, theObject.countCommMult(a, lower, upper), expected);
    }
    {
        int a_array[] = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,49};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int lower = 1;
        int upper = 2000000000;
        int expected = 0;
        CommonMultiples theObject;
        eq(2, theObject.countCommMult(a, lower, upper), expected);
    }
    {
        int a_array[] = {1,1,1};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        int lower = 1;
        int upper = 2000000000;
        int expected = 2000000000;
        CommonMultiples theObject;
        eq(3, theObject.countCommMult(a, lower, upper), expected);
    }

    return 0;
}
// END CUT HERE
