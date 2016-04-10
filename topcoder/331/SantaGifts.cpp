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

class SantaGifts {
public:
vector<string> distribute(vector<string> gifts, int N)
{
    int n = min( 4 * N, (int)gifts.size());
    vector< string > presents( N, "" );

    for( int i = 0; i < n; i++ )
    {
        presents[( i % N )] += ( ( i < N ) ? "" : " " ) + gifts[i];
    }

    return presents;
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
        string gifts_array[] = {"ball", "plane", "robot", "puzzle"};
        vector<string> gifts(gifts_array, gifts_array + ARRSIZE(gifts_array));
        int N = 3;
        string expected_array[] = {"ball puzzle", "plane", "robot"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        SantaGifts theObject;
        eq(0, theObject.distribute(gifts, N), expected);
    }
    {
        string gifts_array[] = {"ball", "plane", "robot", "puzzle", "bike"};
        vector<string> gifts(gifts_array, gifts_array + ARRSIZE(gifts_array));
        int N = 1;
        string expected_array[] = {"ball plane robot puzzle"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        SantaGifts theObject;
        eq(1, theObject.distribute(gifts, N), expected);
    }
    {
        string gifts_array[] = {"ball", "ball", "plane", "plane"};
        vector<string> gifts(gifts_array, gifts_array + ARRSIZE(gifts_array));
        int N = 2;
        string expected_array[] = {"ball plane", "ball plane"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        SantaGifts theObject;
        eq(2, theObject.distribute(gifts, N), expected);
    }
    {
        string gifts_array[] = {"ball", "plane", "robot"};
        vector<string> gifts(gifts_array, gifts_array + ARRSIZE(gifts_array));
        int N = 5;
        string expected_array[] = {"ball", "plane", "robot", "", ""};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        SantaGifts theObject;
        eq(3, theObject.distribute(gifts, N), expected);
    }

    return 0;
}
// END CUT HERE
