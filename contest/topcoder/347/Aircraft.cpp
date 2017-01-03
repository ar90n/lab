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

class Aircraft {
public:
string nearMiss(vector<int> p1, vector<int> v1, vector<int> p2, vector<int> v2, int R)
{
    int a[3];
    int b[3];
    int nome = 0;
    int deno = 0;

    for( int i = 0; i < 3; i ++ )
    {
        a[i] = p1[i] - p2[i];
        b[i] = v1[i] - v2[i];

        nome += a[i] * b[i];
        deno += b[i] * b[i];
    }

    double r = 0.0;
    for( int i = 0; i < 3; i++ )
    {
        r += a[i] * a[i];
    }
    r = sqrt( r );
    if( r <= R )
    {
        return "YES";
    }

    double t = -nome / (double)deno;
    if( t < 0.0 )
    {
        return "NO";
    }

    r = 0.0;
    for( int i = 0; i < 3; i++ )
    {
        r += a[i] * a[i] + 2.0 * a[i] * b[i] * t + b[i] * b[i] * t * t;
    }
    r = sqrt( r );

    return ( r <= R ) ? "YES" : "NO";
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
        int p1_array[] = {15,50,5};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {25,1,0};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {161,102,9};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {-10,-10,-1};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 10;
        string expected = "YES";
        Aircraft theObject;
        eq(0, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {0,0,0};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {2,2,0};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {9,0,5};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {-2,2,0};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 5;
        string expected = "YES";
        Aircraft theObject;
        eq(1, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {0,0,0};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {-2,2,0};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {9,0,5};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {2,2,0};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 5;
        string expected = "NO";
        Aircraft theObject;
        eq(2, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {-2838,-7940,-2936};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {1,1,-2};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {532,3850,9590};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {1,0,-3};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 3410;
        string expected = "YES";
        Aircraft theObject;
        eq(3, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {-8509,9560,345};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {-89,-33,62};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {-5185,-1417,2846};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {-58,24,26};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 8344;
        string expected = "YES";
        Aircraft theObject;
        eq(4, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {-7163,-371,-2459};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {-59,-41,-14};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {-2398,-426,-5487};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {-43,27,67};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 5410;
        string expected = "NO";
        Aircraft theObject;
        eq(5, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {1774,-4491,7810};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {-12,19,-24};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {2322,3793,9897};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {-12,19,-24};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 10000;
        string expected = "YES";
        Aircraft theObject;
        eq(6, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }
    {
        int p1_array[] = {3731,8537,5661};
        vector<int> p1(p1_array, p1_array + ARRSIZE(p1_array));
        int v1_array[] = {-70,71,32};
        vector<int> v1(v1_array, v1_array + ARRSIZE(v1_array));
        int p2_array[] = {8701,-1886,-5115};
        vector<int> p2(p2_array, p2_array + ARRSIZE(p2_array));
        int v2_array[] = {28,-13,7};
        vector<int> v2(v2_array, v2_array + ARRSIZE(v2_array));
        int R = 9766;
        string expected = "NO";
        Aircraft theObject;
        eq(7, theObject.nearMiss(p1, v1, p2, v2, R), expected);
    }

    return 0;
}
// END CUT HERE
