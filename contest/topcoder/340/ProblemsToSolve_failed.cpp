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

class ProblemsToSolve {
public:
int minNumber(vector<int> pleasantness, int variety)
{
    int min_val = pleasantness[0];
    int max_val = pleasantness[0];

    for( int i = 1;i < pleasantness.size(); i++ )
    {
        min_val = min( min_val, pleasantness[ i ] );
        max_val = max( max_val, pleasantness[ i ] );
        int diff = max_val - min_val;

        if( variety <= diff )
        {

            return ( i + 1 ) / 2 + 1;
        }

    }

    return pleasantness.size();
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
        int pleasantness_array[] = {1, 2, 3};
        vector<int> pleasantness(pleasantness_array, pleasantness_array + ARRSIZE(pleasantness_array));
        int variety = 2;
        int expected = 2;
        ProblemsToSolve theObject;
        eq(0, theObject.minNumber(pleasantness, variety), expected);
    }
    {
        int pleasantness_array[] = {1, 2, 3, 4, 5};
        vector<int> pleasantness(pleasantness_array, pleasantness_array + ARRSIZE(pleasantness_array));
        int variety = 4;
        int expected = 3;
        ProblemsToSolve theObject;
        eq(1, theObject.minNumber(pleasantness, variety), expected);
    }
    {
        int pleasantness_array[] = {10, 1, 12, 101};
        vector<int> pleasantness(pleasantness_array, pleasantness_array + ARRSIZE(pleasantness_array));
        int variety = 100;
        int expected = 3;
        ProblemsToSolve theObject;
        eq(2, theObject.minNumber(pleasantness, variety), expected);
    }
    {
        int pleasantness_array[] = {10, 1};
        vector<int> pleasantness(pleasantness_array, pleasantness_array + ARRSIZE(pleasantness_array));
        int variety = 9;
        int expected = 2;
        ProblemsToSolve theObject;
        eq(3, theObject.minNumber(pleasantness, variety), expected);
    }
    {
        int pleasantness_array[] = {6, 2, 6, 2, 6, 3, 3, 3, 7};
        vector<int> pleasantness(pleasantness_array, pleasantness_array + ARRSIZE(pleasantness_array));
        int variety = 4;
        int expected = 2;
        ProblemsToSolve theObject;
        eq(4, theObject.minNumber(pleasantness, variety), expected);
    }

    return 0;
}
// END CUT HERE
