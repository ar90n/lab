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

class ChangingString {
public:
int distance(string A, string B, int K)
{
    int n = A.size();
    int sum = 0;;
    vector< int > dists;

    for( int i =0; i < n; i++ )
    {
        int dist = min( abs( A[i] -B [i] ), ( A[i] - B[i] + 26 ) );

        sum += dist;
        dists.push_back( dist );
    }
    sort( dists.begin(), dists.end(), greater<int>()  );

    for( int i = 0; i < K; i++ )
    {
        sum += ( dists[i] == 0 ) ? 1 : - dists[i] ;
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
        string A = "ab";
        string B = "ba";
        int K = 2;
        int expected = 0;
        ChangingString theObject;
        eq(0, theObject.distance(A, B, K), expected);
    }
    {
        string A = "aa";
        string B = "aa";
        int K = 2;
        int expected = 2;
        ChangingString theObject;
        eq(1, theObject.distance(A, B, K), expected);
    }
    {
        string A = "aaa";
        string B = "baz";
        int K = 1;
        int expected = 1;
        ChangingString theObject;
        eq(2, theObject.distance(A, B, K), expected);
    }
    {
        string A = "fdfdfdfdfdsfabasd";
        string B = "jhlakfjdklsakdjfk";
        int K = 8;
        int expected = 24;
        ChangingString theObject;
        eq(3, theObject.distance(A, B, K), expected);
    }
    {
        string A = "aa";
        string B = "bb";
        int K = 2;
        int expected = 0;
        ChangingString theObject;
        eq(4, theObject.distance(A, B, K), expected);
    }

    return 0;
}
// END CUT HERE
