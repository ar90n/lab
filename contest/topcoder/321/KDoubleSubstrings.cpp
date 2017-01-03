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

class KDoubleSubstrings {
public:

bool isKdouble( string sub, int k )
{
    int n = sub.size();
    int diff = 0;

    for( int i = 0; i < ( n / 2 ); i++ )
    {
        if( sub[i] != sub[ ( n / 2 )  + i ] )
        {
            diff++;
        }
    }

    return diff <= k;
}
int howMuch(vector<string> str, int k)
{
    string whole = "";
    for( int i = 0; i < str.size(); i++ )
    {
        whole += str[i];
    }

    int count = 0;
    for( int window = 2; window <= whole.size(); window+=2 )
    {
        for( int i = 0; i <= whole.size() - window; i++ )
        {
            string sub = whole.substr( i , window );

            if( isKdouble( sub, k ) )
            {
                count++;
            }
        }
    }

    return count;

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
        string str_array[] = {"aa"};
        vector<string> str(str_array, str_array + ARRSIZE(str_array));
        int k = 0;
        int expected = 1;
        KDoubleSubstrings theObject;
        eq(0, theObject.howMuch(str, k), expected);
    }
    {
        string str_array[] = {"aaaa"};
        vector<string> str(str_array, str_array + ARRSIZE(str_array));
        int k = 0;
        int expected = 4;
        KDoubleSubstrings theObject;
        eq(1, theObject.howMuch(str, k), expected);
    }
    {
        string str_array[] = {"contest", "kontest"};
        vector<string> str(str_array, str_array + ARRSIZE(str_array));
        int k = 1;
        int expected = 14;
        KDoubleSubstrings theObject;
        eq(2, theObject.howMuch(str, k), expected);
    }
    {
        string str_array[] = {"abacaba", "d", "abacaba"};
        vector<string> str(str_array, str_array + ARRSIZE(str_array));
        int k = 1;
        int expected = 34;
        KDoubleSubstrings theObject;
        eq(3, theObject.howMuch(str, k), expected);
    }
    {
        string str_array[] = {"areyouready"};
        vector<string> str(str_array, str_array + ARRSIZE(str_array));
        int k = 2;
        int expected = 18;
        KDoubleSubstrings theObject;
        eq(4, theObject.howMuch(str, k), expected);
    }

    return 0;
}
// END CUT HERE
