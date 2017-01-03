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

class CyclesInPermutations {
public:
int maxCycle(vector<int> board)
{
    vector<bool> state( board.size());
    for( int i = 0; i < state.size() ; i++ )
    {
        state[i] = false;
    }

    int max_num = 0;
    for( int ii = 0; ii < board.size(); ii++ )
    {
        for( int i = 0; i < state.size() ; i++ )
        {
         state[i] = false;
        }

        int start = ii;
        int current = ii;
        int num = 0;
        while( state[ current ] != true )
        {
            num++;
            state[ current ] = true;
            current = board[ current ] - 1;
        }

        if( start == current )
        {
            if( max_num < num)
            {
                max_num = num;
            }
        }
    }

    return max_num;
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
        int board_array[] = {3,2,4,1,6,5};
        vector<int> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 3;
        CyclesInPermutations theObject;
        eq(0, theObject.maxCycle(board), expected);
    }
    {
        int board_array[] = {1,2,3,4,5,6};
        vector<int> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 1;
        CyclesInPermutations theObject;
        eq(1, theObject.maxCycle(board), expected);
    }
    {
        int board_array[] = {5,1,2,3,4};
        vector<int> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 5;
        CyclesInPermutations theObject;
        eq(2, theObject.maxCycle(board), expected);
    }
    {
        int board_array[] = {5,7,14,6,16,10,8,17,11,12,18,3,4,13,2,15,9,1,20,19};
        vector<int> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 11;
        CyclesInPermutations theObject;
        eq(3, theObject.maxCycle(board), expected);
    }

    return 0;
}
// END CUT HERE
