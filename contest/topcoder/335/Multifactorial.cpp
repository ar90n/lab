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

class Multifactorial {
public:
string calcMultiFact(int n, int k)
{
    unsigned long long res = 1;

    while( 0 < n )
    {
        unsigned long long last = res;
        if( res <= ( 1000000000000000000 / n ) )
        {
            res *= n;
            n -= k;
        }
        else
        {
            return "overflow";
        }
    }

    stringstream ss;
    string res_str;
    ss << res;
    ss >> res_str;

    return res_str;
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
        int n = 14;
        int k = 3;
        string expected = "12320";
        Multifactorial theObject;
        eq(0, theObject.calcMultiFact(n, k), expected);
    }
    {
        int n = 5;
        int k = 4;
        string expected = "5";
        Multifactorial theObject;
        eq(1, theObject.calcMultiFact(n, k), expected);
    }
    {
        int n = 1000;
        int k = 2;
        string expected = "overflow";
        Multifactorial theObject;
        eq(2, theObject.calcMultiFact(n, k), expected);
    }
    {
        int n = 2000000000;
        int k = 1900000000;
        string expected = "200000000000000000";
        Multifactorial theObject;
        eq(3, theObject.calcMultiFact(n, k), expected);
    }
    {
        int n = 1000;
        int k = 256;
        string expected = "84232704000";
        Multifactorial theObject;
        eq(4, theObject.calcMultiFact(n, k), expected);
    }
    {
        int n = 2000000000;
        int k = 1;
        string expected = "overflow";
        Multifactorial theObject;
        eq(5, theObject.calcMultiFact(n, k), expected);
    }

    return 0;
}
// END CUT HERE
