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

class lower
{
    public:
    char operator()( int a ) const { return tolower( a ); };
};

class PostOffice {
public:
int matchAddress(string address1, string address2)
{
    address1.erase( remove( address1.begin(), address1.end(), ' ' ), address1.end() );
    transform( address1.begin(), address1.end(), address1.begin(), lower() );

    address2.erase( remove( address2.begin(), address2.end(), ' ' ), address2.end() );
    transform( address2.begin(), address2.end(), address2.begin(), lower() );

    int n = 0;
    string::iterator it1 = address1.begin();
    string::iterator it2 = address2.begin();
    while( ( *it1 == *it2 ) && ( it1 != address1.end() ) && ( it2 != address2.end() ) ) {n++;++it1;++it2;}

    return  ( ( n == address1.size() ) && ( n == address2.size() ) ) ? -1 : n;
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
        string address1 = "145 West 44th Street";
        string address2 = "145 west 44 th street";
        int expected = -1;
        PostOffice theObject;
        eq(0, theObject.matchAddress(address1, address2), expected);
    }
    {
        string address1 = " wall street ";
        string address2 = "WallStreet";
        int expected = -1;
        PostOffice theObject;
        eq(1, theObject.matchAddress(address1, address2), expected);
    }
    {
        string address1 = " Wall Street";
        string address2 = "  Waal Street";
        int expected = 2;
        PostOffice theObject;
        eq(2, theObject.matchAddress(address1, address2), expected);
    }
    {
        string address1 = "                                                 A";
        string address2 = "a                                                 ";
        int expected = -1;
        PostOffice theObject;
        eq(3, theObject.matchAddress(address1, address2), expected);
    }
    {
        string address1 = "Wall";
        string address2 = "WallStreet";
        int expected = 4;
        PostOffice theObject;
        eq(4, theObject.matchAddress(address1, address2), expected);
    }

    return 0;
}
// END CUT HERE
