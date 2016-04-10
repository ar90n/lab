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

class VowelEncryptor {
public:
vector<string> encrypt(vector<string> text)
{
    int n = text.size();
    vector< string > vvv;


    for( int i = 0; i < n; i++ )
    {
        string ss = "";
        int m = text[i].size();

        for( int j = 0; j < m ; j++ )
        {
            if ( ( text[i][j] != 'a' ) &&
                 ( text[i][j] != 'i' ) &&
                 ( text[i][j] != 'u' ) &&
                 ( text[i][j] != 'e' ) &&
                 ( text[i][j] != 'o' ) )
            {
                ss += text[i][j];
            }
        }
        if( ss.empty() )
        {
            vvv.push_back( text[i] );
        }
        else
        {
            vvv.push_back( ss );
        }
    }

    return vvv;

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
        string text_array[] = {"hello", "world"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string expected_array[] = {"hll", "wrld"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        VowelEncryptor theObject;
        eq(0, theObject.encrypt(text), expected);
    }
    {
        string text_array[] = {"a", "b", "c"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string expected_array[] = {"a", "b", "c"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        VowelEncryptor theObject;
        eq(1, theObject.encrypt(text), expected);
    }
    {
        string text_array[] = {"he", "who", "is", "greedy", "is", "disgraced"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string expected_array[] = {"h", "wh", "s", "grdy", "s", "dsgrcd"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        VowelEncryptor theObject;
        eq(2, theObject.encrypt(text), expected);
    }

    return 0;
}
// END CUT HERE
