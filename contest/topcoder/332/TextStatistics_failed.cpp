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

class TextStatistics {
public:
double averageLength(string text)
{
    int c_cnt = 0;
    int w_cnt = 0;
    int n = text.size();
    char last = '-';
    string punc = ",.?!-0123456789 ";

    if( n == 0 )
    {
        return 0.0;
    }

    for( int i = 0;i < n; i++ )
    {
        if( punc.find( text[i] ) == string::npos )
        {
            c_cnt++;
            if( punc.find( last ) != string::npos )
            {
                w_cnt++;
            }
        }

        last = text[i];
    }

    return (double)c_cnt / w_cnt;
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
        string text = "This is div2 easy problem.";
        double expected = 4.0;
        TextStatistics theObject;
        eq(0, theObject.averageLength(text), expected);
    }
    {
        string text = "Hello, world!";
        double expected = 5.0;
        TextStatistics theObject;
        eq(1, theObject.averageLength(text), expected);
    }
    {
        string text = "Simple";
        double expected = 6.0;
        TextStatistics theObject;
        eq(2, theObject.averageLength(text), expected);
    }
    {
        string text = "";
        double expected = 0.0;
        TextStatistics theObject;
        eq(3, theObject.averageLength(text), expected);
    }
    {
        string text = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
        double expected = 50.0;
        TextStatistics theObject;
        eq(4, theObject.averageLength(text), expected);
    }

    return 0;
}
// END CUT HERE
