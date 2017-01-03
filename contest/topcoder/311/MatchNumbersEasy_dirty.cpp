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

class MatchNumbersEasy {
public:
string maxNumber(vector<int> matches, int n)
{
    int min_match = *min_element( matches.begin(),  matches.end() );
    int nn = n;
    string result = "";

    while( nn >= 0 )
    {
        int keta = 0;
        int keta_index = 0;
        bool flag = false;

        for( int i = ( nn == n ) ? 1 : 0 ; i < matches.size() ; i++ )
        {
            int tmp;

            if( matches[ i ] <= nn )
            {
                flag = true;

                tmp = ( nn - matches[ i ] ) / min_match;

                if( keta <= tmp )
                {
                    keta = tmp;
                    keta_index = i;
                }
            }
        }

        if( !flag )
        {
            if( nn == n )
            {
                if( matches[ 0 ] <= nn )
                {
                    return "0";
                }
            }
            break;
        }

        stringstream ss;
        ss << keta_index;
        result += ss.str();

        nn -= matches[ keta_index ];
        getchar();
    }

    return result;
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
        int matches_array[] = {6,7,8};
        vector<int> matches(matches_array, matches_array + ARRSIZE(matches_array));
        int n = 21;
        string expected = "210";
        MatchNumbersEasy theObject;
        eq(0, theObject.maxNumber(matches, n), expected);
    }
    {
        int matches_array[] = {5,23,24};
        vector<int> matches(matches_array, matches_array + ARRSIZE(matches_array));
        int n = 30;
        string expected = "20";
        MatchNumbersEasy theObject;
        eq(1, theObject.maxNumber(matches, n), expected);
    }
    {
        int matches_array[] = {1,5,3,2};
        vector<int> matches(matches_array, matches_array + ARRSIZE(matches_array));
        int n = 1;
        string expected = "0";
        MatchNumbersEasy theObject;
        eq(2, theObject.maxNumber(matches, n), expected);
    }
    {
        int matches_array[] = {1,1,1,1,1,1,1,1,1,1};
        vector<int> matches(matches_array, matches_array + ARRSIZE(matches_array));
        int n = 50;
        string expected = "99999999999999999999999999999999999999999999999999";
        MatchNumbersEasy theObject;
        eq(3, theObject.maxNumber(matches, n), expected);
    }

    return 0;
}
// END CUT HERE
