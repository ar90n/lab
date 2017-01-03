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

class GrasslandFencer {
public:

double tri_area( int a, int b, int c )
{
    double p = ( a + b + c ) / 2.0;
    return  sqrt(p * ( p - a ) * ( p - b ) * ( p - c ));
}
double maximalFencedArea(vector<int> fences)
{
    int n = fences.size();
    vector< double > dp( 1 << n );
    fill( dp.begin(), dp.end(), -1.0 );

    dp[0] = 0;
    for(int mask = 0; mask < ( 1 << n );mask++)
    {
        if( dp[mask] != -1.0 )
        {
            for( int i = 0; i < n;i++ )
            {
                for( int j = i + 1; j < n; j++ )
                {
                    for( int k = j + 1; k < n; k++ )
                    {
                        int current = ( 1 << i ) | ( 1 << j ) | ( 1 << k );
                        if( ( ( mask & current ) == 0  ) && ( fences[i] + fences[j] > fences[k] ) )
                        {
                            int index = mask | current;
                            dp[ index ] = max( dp[ index ], dp[ mask ] + tri_area( fences[i], fences[j], fences[k] ) );
                        }
                    }
                }
            }
        }
    }

    return *max_element( dp.begin(), dp.end() );
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
        int fences_array[] = {3,4,5,6,7,8,9};
        vector<int> fences(fences_array, fences_array + ARRSIZE(fences_array));
        double expected = 36.754383146489694;
        GrasslandFencer theObject;
        eq(0, theObject.maximalFencedArea(fences), expected);
    }
    {
        int fences_array[] = {1,2,4,8};
        vector<int> fences(fences_array, fences_array + ARRSIZE(fences_array));
        double expected = 0.0;
        GrasslandFencer theObject;
        eq(1, theObject.maximalFencedArea(fences), expected);
    }
    {
        int fences_array[] = {7,4,4,4};
        vector<int> fences(fences_array, fences_array + ARRSIZE(fences_array));
        double expected = 6.928203230275509;
        GrasslandFencer theObject;
        eq(2, theObject.maximalFencedArea(fences), expected);
    }
    {
        int fences_array[] = {21,72,15,55,16,44,54,63,69,35,75,69,76,70,50,81};
        vector<int> fences(fences_array, fences_array + ARRSIZE(fences_array));
        double expected = 7512.322360676162;
        GrasslandFencer theObject;
        eq(3, theObject.maximalFencedArea(fences), expected);
    }

    return 0;
}
// END CUT HERE
