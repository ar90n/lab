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

class BusSeating {
public:
double getArrangement(string leftRow, string rightRow)
{
    int n = leftRow.size();
    vector< pair< int, int > > pos;
    vector< int > ind;

    for( int i = 0; i < n; i++ )
    {
        if( leftRow[i]  == '-' )
        {
            pos.push_back( pair< int, int >( i,  1 ) );
        }

        if( rightRow[i] == '-' )
        {
            pos.push_back( pair< int, int >( i,  2 ) );
        }
    }


    double min = 1000000000.0;
    for( int i = 0; i < pos.size(); i++ )
    {
        for( int j = i + 1; j < pos.size(); j++ )
        {
            for( int k = j + 1; k < pos.size(); k++ )
            {
                double ab;
                ab = abs( pos[i].first - pos[j].first );
                if( pos[i].second != pos[j].second )
                {
                    ab = hypot( ab, 2 );
                }

                double ac;
                ac = abs( pos[i].first - pos[k].first );
                if( pos[i].second != pos[k].second )
                {
                    ac = hypot( ac, 2 );
                }

                double bc;
                bc = abs( pos[j].first - pos[k].first );
                if( pos[j].second != pos[k].second )
                {
                    bc = hypot( bc, 2 );
                }

                double sum = ab + bc + ac;
                min = ( min < sum ) ? min : sum ;
            }
        }
    }

    return min;
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
        string leftRow = "----------";
        string rightRow = "----------";
        double expected = 4.0;
        BusSeating theObject;
        eq(0, theObject.getArrangement(leftRow, rightRow), expected);
    }
    {
        string leftRow = "XXX-X-XX-X";
        string rightRow = "-XXXX---XX";
        double expected = 4.0;
        BusSeating theObject;
        eq(1, theObject.getArrangement(leftRow, rightRow), expected);
    }
    {
        string leftRow = "XXXXXXXXXX";
        string rightRow = "-XX-XX-X--";
        double expected = 6.0;
        BusSeating theObject;
        eq(2, theObject.getArrangement(leftRow, rightRow), expected);
    }
    {
        string leftRow = "XXX-X-XX-X";
        string rightRow = "XXX-X-XX-X";
        double expected = 6.82842712474619;
        BusSeating theObject;
        eq(3, theObject.getArrangement(leftRow, rightRow), expected);
    }

    return 0;
}
// END CUT HERE
