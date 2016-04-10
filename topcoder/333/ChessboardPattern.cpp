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

class ChessboardPattern {
public:
vector<string> makeChessboard(int rows, int columns)
{
    vector< string > res;

    for( int i = 0; i < rows; i++ )
    {
        string row = "";
        for( int j = 0; j < columns; j++ )
        {
            row += "X."[ ( rows + i + j ) & 1 ];
        }
        res.push_back( row );
    }

    return res;
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
        int rows = 8;
        int columns = 8;
        string expected_array[] = {"X.X.X.X.", ".X.X.X.X", "X.X.X.X.", ".X.X.X.X", "X.X.X.X.", ".X.X.X.X", "X.X.X.X.", ".X.X.X.X"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ChessboardPattern theObject;
        eq(0, theObject.makeChessboard(rows, columns), expected);
    }
    {
        int rows = 1;
        int columns = 20;
        string expected_array[] = {".X.X.X.X.X.X.X.X.X.X"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ChessboardPattern theObject;
        eq(1, theObject.makeChessboard(rows, columns), expected);
    }
    {
        int rows = 5;
        int columns = 1;
        string expected_array[] = {".", "X", ".", "X", "."};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ChessboardPattern theObject;
        eq(2, theObject.makeChessboard(rows, columns), expected);
    }
    {
        int rows = 5;
        int columns = 13;
        string expected_array[] = {".X.X.X.X.X.X.", "X.X.X.X.X.X.X", ".X.X.X.X.X.X.", "X.X.X.X.X.X.X", ".X.X.X.X.X.X."};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ChessboardPattern theObject;
        eq(3, theObject.makeChessboard(rows, columns), expected);
    }

    return 0;
}
// END CUT HERE
