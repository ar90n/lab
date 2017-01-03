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
#include <functional>
using namespace std;

class SpreadingNews {
public:
int minTime(vector<int> supervisors)
{
    vector < pair< int, int> > tmp_supervisors(supervisors.size() );
    vector < int > time(supervisors.size() );
    vector < int > index_table( supervisors.size() + 1 );
    int *p_index_table;

    for( int i = 0; i < supervisors.size(); i++ )
    {
        tmp_supervisors[i] =  pair< int, int >( supervisors[i], i);
    }
    sort( tmp_supervisors.begin(), tmp_supervisors.end() );

    index_table[ 0 ] = -1;
    p_index_table = &( index_table[ 1 ] );
    for( int i = 0; i < ( index_table.size() - 1 );i++)
    {
        p_index_table[ tmp_supervisors[i].second ] = i;
    }

    int i = supervisors.size() - 1;
    while( 0 < i )
    {
        int current_supervisor = p_index_table[ tmp_supervisors[i].first ];
        int from = i;
        while( p_index_table[ tmp_supervisors[i].first ] == current_supervisor )
        {
            i--;
        }
        sort( &( time[ i + 1 ] ),  &( time[ from + 1 ] ), greater<int>() );

        int max = 0;
        int n = 1;
        for( int j = i + 1; j <= from; j++ )
        {
            if( max < ( time[j] + n ) )
            {
                max =  time[j] + n;
            }
            n++;
        }
        time[ current_supervisor ] = max;
    }

    return time[0];
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
        int supervisors_array[] = {-1, 0, 0};
        vector<int> supervisors(supervisors_array, supervisors_array + ARRSIZE(supervisors_array));
        int expected = 2;
        SpreadingNews theObject;
        eq(0, theObject.minTime(supervisors), expected);
    }
    {
        int supervisors_array[] = {-1, 0, 0, 2, 2};
        vector<int> supervisors(supervisors_array, supervisors_array + ARRSIZE(supervisors_array));
        int expected = 3;
        SpreadingNews theObject;
        eq(1, theObject.minTime(supervisors), expected);
    }
    {
        int supervisors_array[] = {-1, 0, 1, 2, 3};
        vector<int> supervisors(supervisors_array, supervisors_array + ARRSIZE(supervisors_array));
        int expected = 4;
        SpreadingNews theObject;
        eq(2, theObject.minTime(supervisors), expected);
    }
    {
        int supervisors_array[] = {-1, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 8, 12, 13, 14, 16, 16, 16};
        vector<int> supervisors(supervisors_array, supervisors_array + ARRSIZE(supervisors_array));
        int expected = 7;
        SpreadingNews theObject;
        eq(3, theObject.minTime(supervisors), expected);
    }
}
// END CUT HERE
