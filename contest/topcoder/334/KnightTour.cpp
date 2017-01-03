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

class KnightTour {
public:
string checkTour(vector<string> cells)
{
    int board[6][6] = {};
    int last_letter;
    int last_digit;
    int current_letter;
    int current_digit;

    last_letter = cells[0][0] - 'A';
    last_digit  = cells[0][1] - '1';
    board[ last_letter ][last_digit ] = 1;
    for(int i = 1; i < cells.size(); i++ )
    {
        current_letter = cells[i][0] - 'A';
        current_digit = cells[i][1] - '1';

        int diff_digit = current_digit - last_digit;
        int diff_letter = current_letter - last_letter;
        int dist = abs( diff_letter ) + abs( diff_digit );

        if( ( dist != 3 ) || ( diff_digit == 0 ) || ( diff_letter == 0 ) )
        {
            return "Invalid";
        }

        if( board[ current_letter ][ current_digit ] != 0 )
        {
            return "Invalid";
        }

        board[ current_letter ][ current_digit ] = 1;
        last_letter = current_letter;
        last_digit = current_digit;

    }

    current_letter = cells[0][0] - 'A';
    current_digit = cells[0][1] - '1';
    int diff_digit = current_digit - last_digit;
    int diff_letter = current_letter - last_letter;
    int dist = abs( diff_letter ) + abs( diff_digit );

    if( ( dist != 3 ) || ( diff_digit == 0 ) || ( diff_letter == 0 ) )
    {
        return "Invalid";
    }

    return "Valid";

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
        string cells_array[] = {"A1", "B3", "A5", "C6", "E5", "F3", "D2", "F1", "E3", "F5", "D4", "B5", "A3", "B1", "C3", "A2", "C1", "E2", "F4", "E6", "C5", "A6", "B4", "D5", "F6", "E4", "D6", "C4", "B6", "A4", "B2", "D1", "F2", "D3", "E1", "C2"};
        vector<string> cells(cells_array, cells_array + ARRSIZE(cells_array));
        string expected = "Valid";
        KnightTour theObject;
        eq(0, theObject.checkTour(cells), expected);
    }
    {
        string cells_array[] = {"A1", "C2", "E3", "F5", "D4", "B3", "A1", "C2", "E3", "F5", "D4", "B3", "A1", "C2", "E3", "F5", "D4", "B3", "A1", "C2", "E3", "F5", "D4", "B3", "A1", "C2", "E3", "F5", "D4", "B3", "A1", "C2", "E3", "F5", "D4", "B3"};
        vector<string> cells(cells_array, cells_array + ARRSIZE(cells_array));
        string expected = "Invalid";
        KnightTour theObject;
        eq(1, theObject.checkTour(cells), expected);
    }
    {
        string cells_array[] = {"D4", "F5", "D6", "B5", "A3", "B1", "D2", "F1", "E3", "D1", "F2", "E4", "F6", "D5", "B6", "A4", "B2", "C4", "A5", "C6", "E5", "F3", "E1", "C2", "A1", "B3", "C5", "E6", "F4", "E2", "C3", "A2", "C1", "D3", "B4", "A6"};
        vector<string> cells(cells_array, cells_array + ARRSIZE(cells_array));
        string expected = "Invalid";
        KnightTour theObject;
        eq(2, theObject.checkTour(cells), expected);
    }
    {
        string cells_array[] = {"D4", "F5", "D6", "B5", "A3", "B1", "D2", "F1", "E3", "D1", "F2", "E4", "F6", "D5", "B6", "A4", "B2", "C4", "A5", "C6", "E5", "F3", "E1", "C2", "A1", "B3", "C5", "A6", "B4", "A2", "C3", "E2", "C1", "D3", "F4", "E6"};
        vector<string> cells(cells_array, cells_array + ARRSIZE(cells_array));
        string expected = "Valid";
        KnightTour theObject;
        eq(3, theObject.checkTour(cells), expected);
    }
    {
        string cells_array[] = {"C5", "D3", "F2", "D1", "B2", "A4", "B6", "D5", "C3", "E4", "F6", "B3", "A1", "C2", "E1", "F3", "E5", "C6", "A5", "C4", "A3", "B1", "D2", "F1", "E3", "F5", "D6", "B5", "D4", "E6", "F4", "E2", "C1", "A2", "B4", "A6"};
        vector<string> cells(cells_array, cells_array + ARRSIZE(cells_array));
        string expected = "Invalid";
        KnightTour theObject;
        eq(4, theObject.checkTour(cells), expected);
    }

    return 0;
}
// END CUT HERE
