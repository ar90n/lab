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

class NounReform {
public:
vector<string> makePlural(vector<string> nouns)
{
    int n = nouns.size();

    for( int i = 0; i < n; i ++ )
    {
        int m = nouns[i].size();

        if( ( nouns[i][ m - 1 ] == 's' ) ||
            ( nouns[i][ m - 1 ] == 'z' ) ||
            ( nouns[i][ m - 1 ] == 'x' ) )
        {
            nouns[i] += "es";
        }
        else if( ( ( nouns[i][ m - 1 ] == 'h' ) && ( nouns[i][ m - 2 ] == 's' ) ) ||
                 ( ( nouns[i][ m - 1 ] == 's' ) && ( nouns[i][ m - 2 ] == 'e' ) ) ||
                 ( ( nouns[i][ m - 1 ] == 'h' ) && ( nouns[i][ m - 2 ] == 'c' ) ) )
        {
            nouns[i] += "es";
        }
        else if( nouns[i][ m - 1] == 'y' )
        {
            if( ( nouns[i][m - 2] == 'a' ) ||
             ( nouns[i][m - 2] == 'e' ) ||
             ( nouns[i][m - 2] == 'i' ) ||
             ( nouns[i][m - 2] == 'o' ) ||
             ( nouns[i][m - 2] == 'u' ) )
            {
                nouns[i] += "s";
            }
            else
            {
                nouns[i][m - 1] = 'i';
                nouns[i] += "es";
            }
        }
        else{
            nouns[i] += "s";
        }
    }
    return nouns;
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
        string nouns_array[] = {"box", "church", "elephant", "stereo"};
        vector<string> nouns(nouns_array, nouns_array + ARRSIZE(nouns_array));
        string expected_array[] = {"boxes", "churches", "elephants", "stereos"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        NounReform theObject;
        eq(0, theObject.makePlural(nouns), expected);
    }
    {
        string nouns_array[] = {"tray", "key", "enemy", "baby"};
        vector<string> nouns(nouns_array, nouns_array + ARRSIZE(nouns_array));
        string expected_array[] = {"trays", "keys", "enemies", "babies"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        NounReform theObject;
        eq(1, theObject.makePlural(nouns), expected);
    }
    {
        string nouns_array[] = {"a", "s", "oy", "y", "yy"};
        vector<string> nouns(nouns_array, nouns_array + ARRSIZE(nouns_array));
        string expected_array[] = {"as", "ses", "oys", "ies", "yies"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        NounReform theObject;
        eq(2, theObject.makePlural(nouns), expected);
    }

    return 0;
}
// END CUT HERE
