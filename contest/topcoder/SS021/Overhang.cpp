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

class Overhang {
public:
int howManyCards(double goalLength)
{
    int cards;
    for( cards = 0;  ; cards++ )
    {
        if( goalLength <= 0.0 )
        {
            break;
        }

        goalLength -= 1.0 / ( cards + 2 );
    }

    return cards;
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
        double goalLength = 0.5;
		int expected = 1;
        Overhang theObject;
        eq(0, theObject.howManyCards(goalLength), expected);
    }
	{
        double goalLength = 1.00;
		int expected = 3;
        Overhang theObject;
        eq(1, theObject.howManyCards(goalLength), expected);
    }
	{
        double goalLength = 5.19;
		int expected = 273;
        Overhang theObject;
        eq(2, theObject.howManyCards(goalLength), expected);
    }
	{
        double goalLength = 0.00;
		int expected = 0;
        Overhang theObject;
        eq(3, theObject.howManyCards(goalLength), expected);
    }
	{
        double goalLength = 6.83;
		int expected = 1411;
        Overhang theObject;
        eq(4, theObject.howManyCards(goalLength), expected);
    }
	{
        double goalLength = 10.00;
		int expected = 33616;
        Overhang theObject;
        eq(5, theObject.howManyCards(goalLength), expected);
    }

    return 0;
}
// END CUT HERE
