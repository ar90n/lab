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

class MooingCows {
public:
int dissatisfaction(vector<string> farmland)
{
    int n = farmland.size();
    int m = farmland[0].size();

    vector< pair< int, int > > cow_pos;
    for( int i = 0; i < n; i++ )
    {
        for (int j = 0; j < m; j++)
        {
            if( farmland[i][j] == 'C' )
            {
                cow_pos.push_back( pair< int, int >( i, j ) );
            }
        }
    }

    int cows = cow_pos.size();
    vector< int > dissatisfaction( cows );
    fill( dissatisfaction.begin(),  dissatisfaction.end(), 0 );

    for( int i = 0; i < cows; i++ )
    {
        int from_y_pos = cow_pos[i].first;
        int from_x_pos = cow_pos[i].second;

        for( int j = i + 1; j < cows; j++ )
        {
            int to_y_pos = cow_pos[j].first;
            int to_x_pos = cow_pos[j].second;

            int diff_y = to_y_pos - from_y_pos;
            int diff_x = to_x_pos - from_x_pos;
            int tmp_dist = diff_x * diff_x + diff_y * diff_y;

            dissatisfaction[i] += tmp_dist;
            dissatisfaction[j] += tmp_dist;
        }
    }

    vector< int >::iterator iter = min_element( dissatisfaction.begin(),  dissatisfaction.end() );
    return *iter;

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
        string farmland_array[] = {"C..", ".C.", ".C."};
        vector<string> farmland(farmland_array, farmland_array + ARRSIZE(farmland_array));
        int expected = 3;
        MooingCows theObject;
        eq(0, theObject.dissatisfaction(farmland), expected);
    }
    {
        string farmland_array[] = {"CCCC", "CCCC", "CCCC"};
        vector<string> farmland(farmland_array, farmland_array + ARRSIZE(farmland_array));
        int expected = 26;
        MooingCows theObject;
        eq(1, theObject.dissatisfaction(farmland), expected);
    }
    {
        string farmland_array[] = {"C"};
        vector<string> farmland(farmland_array, farmland_array + ARRSIZE(farmland_array));
        int expected = 0;
        MooingCows theObject;
        eq(2, theObject.dissatisfaction(farmland), expected);
    }
    {
        string farmland_array[] = {"CCC....", "C......", "....C.C", ".C.CC..", "C......"};
        vector<string> farmland(farmland_array, farmland_array + ARRSIZE(farmland_array));
        int expected = 81;
        MooingCows theObject;
        eq(3, theObject.dissatisfaction(farmland), expected);
    }

    return 0;
}
// END CUT HERE
