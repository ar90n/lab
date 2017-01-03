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

class Trekking {
public:
int findCamps(string trail, vector<string> plans)
{
    int n = plans.size();
    int m = plans[0].size();
    unsigned int nights = -1;

    for( int i = 0; i < n; i ++ )
    {
        unsigned int cur_nights = 0;
        for( int j = 0; j < m; j ++ )
        {
            if( plans[i][j] == 'C' )
            {
                if( trail[j] == '.' )
                {
                    cur_nights++;
                }
                else
                {
                    cur_nights = -1;
                    break;
                }
            }
        }

        nights = min( nights , cur_nights );
    }

    return nights;
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
        string trail = "^^....^^^...";
        string plans_array[] = {"CwwCwwCwwCww", "wwwCwCwwwCww", "wwwwCwwwwCww"};
        vector<string> plans(plans_array, plans_array + ARRSIZE(plans_array));
        int expected = 2;
        Trekking theObject;
        eq(0, theObject.findCamps(trail, plans), expected);
    }
    {
        string trail = "^^^^";
        string plans_array[] = {"wwww", "wwwC"};
        vector<string> plans(plans_array, plans_array + ARRSIZE(plans_array));
        int expected = 0;
        Trekking theObject;
        eq(1, theObject.findCamps(trail, plans), expected);
    }
    {
        string trail = "^^.^^^^";
        string plans_array[] = {"wwCwwwC", "wwwCwww", "wCwwwCw"};
        vector<string> plans(plans_array, plans_array + ARRSIZE(plans_array));
        int expected = -1;
        Trekking theObject;
        eq(2, theObject.findCamps(trail, plans), expected);
    }
    {
        string trail = "^^^^....^.^.^.";
        string plans_array[] = {"wwwwCwwwwCwCwC", "wwwwCwwCwCwwwC", "wwwCwwwCwwwCww", "wwwwwCwwwCwwwC"};
        vector<string> plans(plans_array, plans_array + ARRSIZE(plans_array));
        int expected = 3;
        Trekking theObject;
        eq(3, theObject.findCamps(trail, plans), expected);
    }
    {
        string trail = "..............";
        string plans_array[] = {"CwCwCwCwCwCwCw", "CwwCwwCwwCwwCw"};
        vector<string> plans(plans_array, plans_array + ARRSIZE(plans_array));
        int expected = 5;
        Trekking theObject;
        eq(4, theObject.findCamps(trail, plans), expected);
    }

    return 0;
}
// END CUT HERE
