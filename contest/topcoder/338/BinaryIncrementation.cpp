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

class BinaryIncrementation {
public:
string plusOne(string x)
{
    int zpos = x.find_last_of( "0" );
    if( zpos == string::npos )
    {
        return "1" + string( "000000000000000000000000000000").substr( 0, x.size() );
    }
    return x.replace( zpos, x.size() - zpos, "1" + string( "000000000000000000000000000000000000000000000000000000000000000").substr( 0, x.size() - zpos - 1 ) );
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
        string x = "10011";
        string expected = "10100";
        BinaryIncrementation theObject;
        eq(0, theObject.plusOne(x), expected);
    }
    {
        string x = "10000";
        string expected = "10001";
        BinaryIncrementation theObject;
        eq(1, theObject.plusOne(x), expected);
    }
    {
        string x = "1111";
        string expected = "10000";
        BinaryIncrementation theObject;
        eq(2, theObject.plusOne(x), expected);
    }
    {
        string x = "1";
        string expected = "10";
        BinaryIncrementation theObject;
        eq(3, theObject.plusOne(x), expected);
    }
    {
        string x = "101010101010101010101010101010";
        string expected = "101010101010101010101010101011";
        BinaryIncrementation theObject;
        eq(4, theObject.plusOne(x), expected);
    }

    return 0;
}
// END CUT HERE
