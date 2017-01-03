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

class CreatePairs {
public:
int maximalSum(vector<int> data)
{
    int n = data.size();
    int i;
    int sum = 0;
    int neg_border = 0;
    int pos_border = n;

    sort( data.begin(), data.end() );
    for( int i = 0; i < n; i++ )
    {
        if( data[i] <= 0 )
            neg_border = i + 1;
        else
            break;
    }

    for( int i = n - 1; 0 <= i; i-- )
        if( 1 < data[i] )
            pos_border = i;
        else
            break;

    for( i = 0; i < neg_border - 1; i += 2 )
    {
        sum += data[i] * data[i + 1];
    }

    if( i == neg_border - 1 )
    {
        sum += data[i];
    }

    for( i = n - 1; ( pos_border < i ) ; i-=2 )
    {
        sum += data[i] * data[i - 1];
    }

    if( i == pos_border )
    {
        sum += data[ i ];
    }

    for( i = neg_border ; i < pos_border; i++ )
    {
        sum += data[i];
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
        int data_array[] = {0, 1, 2, 4, 3, 5};
        vector<int> data(data_array, data_array + ARRSIZE(data_array));
        int expected = 27;
        CreatePairs theObject;
        eq(0, theObject.maximalSum(data), expected);
    }
    {
        int data_array[] = {-1, 1, 2, 3};
        vector<int> data(data_array, data_array + ARRSIZE(data_array));
        int expected = 6;
        CreatePairs theObject;
        eq(1, theObject.maximalSum(data), expected);
    }
    {
        int data_array[] = {-1};
        vector<int> data(data_array, data_array + ARRSIZE(data_array));
        int expected = -1;
        CreatePairs theObject;
        eq(2, theObject.maximalSum(data), expected);
    }
    {
        int data_array[] = {-1, 0, 1};
        vector<int> data(data_array, data_array + ARRSIZE(data_array));
        int expected = 1;
        CreatePairs theObject;
        eq(3, theObject.maximalSum(data), expected);
    }
    {
        int data_array[] = {1, 1};
        vector<int> data(data_array, data_array + ARRSIZE(data_array));
        int expected = 2;
        CreatePairs theObject;
        eq(4, theObject.maximalSum(data), expected);
    }
    return 0;
}
// END CUT HERE
