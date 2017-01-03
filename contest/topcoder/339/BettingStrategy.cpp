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

class BettingStrategy {
public:
int finalSum(int initSum, string outcome)
{
    int num_w = count( outcome.begin(), outcome.end(), 'W' );
    int num_l = outcome.substr( outcome.find_last_of( 'W' ) + 1, outcome.size() ).size();


    return initSum + num_w - ( ( 1 <<  min( num_l, (int)floor( log( initSum + num_w ) / log( 2.0 ) )  )  ) - 1 );
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
        int initSum = 12;
        string outcome = "WWWWWWWW";
        int expected = 20;
        BettingStrategy theObject;
        eq(0, theObject.finalSum(initSum, outcome), expected);
    }
    {
        int initSum = 15;
        string outcome = "LLLWLLLL";
        int expected = 1;
        BettingStrategy theObject;
        eq(1, theObject.finalSum(initSum, outcome), expected);
    }
    {
        int initSum = 20;
        string outcome = "WLWLWLWL";
        int expected = 23;
        BettingStrategy theObject;
        eq(2, theObject.finalSum(initSum, outcome), expected);
    }
    {
        int initSum = 925;
        string outcome = "WLLLLLWLLLLLLWWWWWLWLLWLLLLLLWL";
        int expected = 934;
        BettingStrategy theObject;
        eq(3, theObject.finalSum(initSum, outcome), expected);
    }

    return 0;
}
// END CUT HERE
