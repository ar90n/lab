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

class StandInLine {
public:
vector<int> reconstruct(vector<int> left)
{
    int num = left.size();

    vector< int > result( num );
    fill( result.begin(),  result.end(), -1 );
    for( int i = 0; i < num; i++ )
    {
        int spaces = left[i];
        int j = 0;
        while( 1 )
        {
            if( result[j] == -1 )
            {
                if( spaces == 0 )
                {
                    break;
                }
                spaces--;
            }
            j++;
        }
        result[j] = i + 1;
    }

    return result;
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
        int left_array[] = {2, 1, 1, 0 };
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int expected_array[] = {4, 2, 1, 3 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        StandInLine theObject;
        eq(0, theObject.reconstruct(left), expected);
    }
    {
        int left_array[] = {0, 0, 0, 0, 0 };
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int expected_array[] = {1, 2, 3, 4, 5 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        StandInLine theObject;
        eq(1, theObject.reconstruct(left), expected);
    }
    {
        int left_array[] = {5, 4, 3, 2, 1, 0 };
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int expected_array[] = {6, 5, 4, 3, 2, 1 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        StandInLine theObject;
        eq(2, theObject.reconstruct(left), expected);
    }
    {
        int left_array[] = {6, 1, 1, 1, 2, 0, 0 };
        vector<int> left(left_array, left_array + ARRSIZE(left_array));
        int expected_array[] = {6, 2, 3, 4, 7, 5, 1 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        StandInLine theObject;
        eq(3, theObject.reconstruct(left), expected);
    }

    return 0;
}
// END CUT HERE
