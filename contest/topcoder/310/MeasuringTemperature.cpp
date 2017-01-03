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

class MeasuringTemperature {
public:
double averageTemperature(vector<int> measuredValues)
{
    int n = measuredValues.size();
    int top = measuredValues[0];
    int tail = measuredValues[ n - 1 ];

    vector< int > valid_temp;
    for( int i = 0; i < n; i++ )
    {
        if( measuredValues[i] < -273 )
        {
            continue;
        }

        int max_dif = 1 << 29;
        for( int j = - 2; j <= 2; j++ )
        {
            if( j != 0 )
            {
                int m = i + j;
                if( ( 0 <= m ) && ( m < n ) )
                {
                    max_dif = min( max_dif, abs( measuredValues[ m ] - measuredValues[ i ] ) );
                }
            }
        }

        if( max_dif <= 2 )
        {
            valid_temp.push_back( measuredValues[i] );
        }
    }

    if( valid_temp.size() == 0 )
        return -300;

    int sum = 0;
    for( int i = 0; i < valid_temp.size(); i++ )
    {
        sum += valid_temp[i];
    }

    return (double) sum / valid_temp.size();
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
        int measuredValues_array[] = {9, 11, 12, 13, 15};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = 12.0;
        MeasuringTemperature theObject;
        eq(0, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {0, 0, 0, 2, 997, -1, 0};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = 0.16666666666666666;
        MeasuringTemperature theObject;
        eq(1, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {0, 0, 0, 2, -4, -1, 0};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = 0.16666666666666666;
        MeasuringTemperature theObject;
        eq(2, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {0, 0, 0, 2, -3, -1, 0};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = -0.2857142857142857;
        MeasuringTemperature theObject;
        eq(3, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {1,2,3,100,100,1,2};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = 29.857142857142858;
        MeasuringTemperature theObject;
        eq(4, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {1,2,3,4,5,6,7,10};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = 4.0;
        MeasuringTemperature theObject;
        eq(5, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {-35, -34, -34, -34, -35, 72, -34, 52, -36, -35, -36, 52, -36, -35, 981, -33};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = -34.75;
        MeasuringTemperature theObject;
        eq(6, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {-273, -273, -274, -273};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = -273.0;
        MeasuringTemperature theObject;
        eq(7, theObject.averageTemperature(measuredValues), expected);
    }
    {
        int measuredValues_array[] = {10, 20, 30, 40};
        vector<int> measuredValues(measuredValues_array, measuredValues_array + ARRSIZE(measuredValues_array));
        double expected = -300.0;
        MeasuringTemperature theObject;
        eq(8, theObject.averageTemperature(measuredValues), expected);
    }

    return 0;
}
// END CUT HERE
