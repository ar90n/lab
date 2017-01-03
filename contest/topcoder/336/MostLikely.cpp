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

class MostLikely {
public:
int likelyRank(vector<int> sc, int low, int high)
{
    int nnn = sc.size();
    vector< pair< int,int> > ranks;
    sort( sc.begin(), sc.end() );

    sc.insert( sc.begin(), -( 1 << 29 ) );
    sc.push_back( 1 << 29 );

    for( int i = 0; i < ( sc.size() - 1 ); i++ )
    {
        int l = max(  sc[i], low );
        int r = min(  sc[i + 1] - 1, high );

        int num = r - l;
        if( num < 0 )
        {
            num = 0;
        }
        else
        {
            num++;
        }

        ranks.push_back( make_pair( num, nnn  + 1 - i ) );
    }
    sort( ranks.begin(), ranks.end(), greater< pair< int, int > >() );

    if( ranks[0].first == ranks[1].first )
    {
        return -1;
    }

    return ranks[0].second;
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
        int sc_array[] = {3,12,4};
        vector<int> sc(sc_array, sc_array + ARRSIZE(sc_array));
        int low = 8;
        int high = 8;
        int expected = 2;
        MostLikely theObject;
        eq(0, theObject.likelyRank(sc, low, high), expected);
    }
    {
        int sc_array[] = {3,4,5};
        vector<int> sc(sc_array, sc_array + ARRSIZE(sc_array));
        int low = 3;
        int high = 7;
        int expected = 1;
        MostLikely theObject;
        eq(1, theObject.likelyRank(sc, low, high), expected);
    }
    {
        int sc_array[] = {3,4,5};
        vector<int> sc(sc_array, sc_array + ARRSIZE(sc_array));
        int low = 2;
        int high = 5;
        int expected = -1;
        MostLikely theObject;
        eq(2, theObject.likelyRank(sc, low, high), expected);
    }

    {
        int sc_array[] = {121,120,119};
        vector<int> sc(sc_array, sc_array + ARRSIZE(sc_array));
        int low = 120;
        int high = 120;
        int expected = 2;
        MostLikely theObject;
        eq(3, theObject.likelyRank(sc, low, high), expected);
    }
    {
        int sc_array[] = {422000000, 1, 6, 6, 8, 9, 422000000};
        vector<int> sc(sc_array, sc_array + ARRSIZE(sc_array));
        int low = 0;
        int high = 843999990;
        int expected = -1;
        MostLikely theObject;
        eq(4, theObject.likelyRank(sc, low, high), expected);
    }
    return 0;
}
// END CUT HERE
