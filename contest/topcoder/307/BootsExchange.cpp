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

class BootsExchange {
public:
int leastAmount(vector<int> left, vector<int> right)
{
    int n = left.size();
    vector< int > hist( 1001 );
    fill( hist.begin(), hist.end(), 0 );

    for( int i = 0; i < n; i++ )
    {
        hist[ left[i] ]++;
        hist[ right[i] ]--;
    }

    int sum = 0;
    for( int i = 0; i < 1001; i++ )
    {
        if( hist[i] > 0 )
        {
            sum += hist[i];
        }
    }

    return sum;
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
        int left_array[] = {1, 3, 1};
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int right_array[] = {2, 1, 3};
        vector<int> right(right_array, right_array + ARRSIZE(right_array));
        int expected = 1;
        BootsExchange theObject;
        eq(0, theObject.leastAmount(left, right), expected);
    }
    {
        int left_array[] = {1, 3};
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int right_array[] = {2, 2};
        vector<int> right(right_array, right_array + ARRSIZE(right_array));
        int expected = 2;
        BootsExchange theObject;
        eq(1, theObject.leastAmount(left, right), expected);
    }
    {
        int left_array[] = {1, 2, 3, 4, 5, 6, 7};
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int right_array[] = {2, 4, 6, 1, 3, 7, 5};
        vector<int> right(right_array, right_array + ARRSIZE(right_array));
        int expected = 0;
        BootsExchange theObject;
        eq(2, theObject.leastAmount(left, right), expected);
    }

    return 0;
}
// END CUT HERE
