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

string table[] = { "HHLLL",
                   "LLLHH",
                   "LLHLH",
                   "LLHHL",
                   "LHLLH",
                   "LHLHL",
                   "LHHLL",
                   "HLLLH",
                   "HLLHL",
                   "HLHLL" };
class Postnet {
public:
string barcode(string zipCode)
{
    int n = zipCode.size();

    int sum = 0;
    string code = "H";
    for( int i = 0; i < n; i ++ )
    {
        int val = zipCode[ i ] - '0';
        sum += val;
        code += table[ val ];
    }
    int dest = 0;
    while( dest < sum )
    {
        dest += 10;
    }
    int cs = dest - sum;
    code += table[ cs ];
    code += "H";

    return code;
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
        string zipCode = "12345";
        string expected = "HLLLHHLLHLHLLHHLLHLLHLHLHLLHLHLH";
        Postnet theObject;
        eq(0, theObject.barcode(zipCode), expected);
    }
    {
        string zipCode = "94070";
        string expected = "HHLHLLLHLLHHHLLLHLLLHHHLLLHHLLLH";
        Postnet theObject;
        eq(1, theObject.barcode(zipCode), expected);
    }
    {
        string zipCode = "11111";
        string expected = "HLLLHHLLLHHLLLHHLLLHHLLLHHLHLHLH";
        Postnet theObject;
        eq(2, theObject.barcode(zipCode), expected);
    }
    {
        string zipCode = "12348";
        string expected = "HLLLHHLLHLHLLHHLLHLLHHLLHLLLHLHH";
        Postnet theObject;
        eq(3, theObject.barcode(zipCode), expected);
    }

    return 0;
}
// END CUT HERE
