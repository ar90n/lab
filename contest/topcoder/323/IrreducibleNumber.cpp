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

class IrreducibleNumber {
public:
int getIrreducible(vector<int> A)
{
    set< int > sums;

    for( int i = 0; i < A.size(); i++ )
    {
        sums.insert( A[ i ] );
        for( int j = i + 1; j < A.size(); j++ )
        {
            sums.insert( A[ i ] + A[ j ] );
            for( int k = j + 1; k < A.size(); k ++ )
            {
                sums.insert( A[ i ] + A[ j ] + A[ k ]  );
            }
        }
    }

    int val = 1;
    set<int>::iterator it = sums.begin();
    while( it != sums.end() )
    {
        if( *it != val )
        {
            break;
        }
        val++;
        it++;
    }

    return val;
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
        int A_array[] = {1,1};
        vector<int> A(A_array, A_array + ARRSIZE(A_array));
        int expected = 3;
        IrreducibleNumber theObject;
        eq(0, theObject.getIrreducible(A), expected);
    }
    {
        int A_array[] = {1,2};
        vector<int> A(A_array, A_array + ARRSIZE(A_array));
        int expected = 4;
        IrreducibleNumber theObject;
        eq(1, theObject.getIrreducible(A), expected);
    }
    {
        int A_array[] = {1,3};
        vector<int> A(A_array, A_array + ARRSIZE(A_array));
        int expected = 2;
        IrreducibleNumber theObject;
        eq(2, theObject.getIrreducible(A), expected);
    }
    {
        int A_array[] = {4, 1, 3};
        vector<int> A(A_array, A_array + ARRSIZE(A_array));
        int expected = 2;
        IrreducibleNumber theObject;
        eq(3, theObject.getIrreducible(A), expected);
    }

    return 0;
}
// END CUT HERE
