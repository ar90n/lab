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

class GroupWork {
public:
long long bestGroup(vector<int> p, vector<int> s)
{
    int n = p.size();
    vector< pair< int, int > > elem( n );

    for( int i = 0;i < n; i++ )
    {
        elem[ i ] = pair< int, int >( s[i], p[i] );
    }

    sort( elem.begin(), elem.end(), greater< pair< int, int > >() );

    unsigned long long  num = 0 ;
    unsigned long long  skill = 0;
    unsigned long long pro1 = 0;
    unsigned long long pro2 = 0;
    for( int i = 0; i < n; i++ )
    {
        num += elem[ i ].second;
        pro2 = num * elem[ i ].first;
        if( pro1 <= pro2 )
        {
            pro1 = pro2;
        }
    }

    return pro1;;

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
        int p_array[] = {1,2,1};
        vector<int> p(p_array, p_array + ARRSIZE(p_array));
        int s_array[] = {3,5,9};
        vector<int> s(s_array, s_array + ARRSIZE(s_array));
        long long expected = 15LL;
        GroupWork theObject;
        eq(0, theObject.bestGroup(p, s), expected);
    }
    {
        int p_array[] = {2,2,2,2};
        vector<int> p(p_array, p_array + ARRSIZE(p_array));
        int s_array[] = {5,1,1,5};
        vector<int> s(s_array, s_array + ARRSIZE(s_array));
        long long expected = 20LL;
        GroupWork theObject;
        eq(1, theObject.bestGroup(p, s), expected);
    }
    {
        int p_array[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
        vector<int> p(p_array, p_array + ARRSIZE(p_array));
        int s_array[] = {31,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
        vector<int> s(s_array, s_array + ARRSIZE(s_array));
        long long expected = 31LL;
        GroupWork theObject;
        eq(2, theObject.bestGroup(p, s), expected);
    }
    {
        int p_array[] = {1000000000,1000000000,1000000000};
        vector<int> p(p_array, p_array + ARRSIZE(p_array));
        int s_array[] = {1000,999,998};
        vector<int> s(s_array, s_array + ARRSIZE(s_array));
        long long expected = 2994000000000LL;
        GroupWork theObject;
        eq(3, theObject.bestGroup(p, s), expected);
    }

    return 0;
}
// END CUT HERE
