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

class Sortness {
public:
double getSortness(vector<int> a)
{
    int n = a.size();
    int sum = 0;;

    for( int i = 0; i < n; i ++ )
    {
        for( int j = 0; j < n; j++ )
        {
            if( j < i )
            {
                if( a[i] < a[j] )
                {
                    sum++;
                }
            }
            else
            {
                if( a[j] < a[i] )
                {
                    sum++;
                }
            }
        }
    }

    return (double)sum / n;
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
        int a_array[] = {3,2,1,4,6,7,5,8};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        double expected = 1.25;
        Sortness theObject;
        eq(0, theObject.getSortness(a), expected);
    }
    {
        int a_array[] = {1,2,3};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        double expected = 0.0;
        Sortness theObject;
        eq(1, theObject.getSortness(a), expected);
    }
    {
        int a_array[] = {5,4,3,2,1};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        double expected = 4.0;
        Sortness theObject;
        eq(2, theObject.getSortness(a), expected);
    }
    {
        int a_array[] = {1,5,8,7,9,6,10,12,11,3,4,2};
        vector<int> a(a_array, a_array + ARRSIZE(a_array));
        double expected = 5.166666666666667;
        Sortness theObject;
        eq(3, theObject.getSortness(a), expected);
    }

}
// END CUT HERE
