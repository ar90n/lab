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

class BattleshipChecker {
public:

int bs( vector< string > &board,  int i, int j )
{
    int size = 1;

    board[ i ][ j ] = '.';
    if( ( i + 1 ) < board[0].size() && board[ i + 1 ][ j ] == 'X' )
    {
        for( int k = 1; board[ i + k ][ j ] == 'X'; k++ )
        {
            board[ i + k ][ j ] = '.';
            size++;
        }
    }
    else if( (j + 1 ) < board.size() && board[ i ][ j + 1] == 'X' )
    {
        for( int k = 1; board[ i ][ j + k ] == 'X'; k++ )
        {
            board[ i ][ j + k ] = '.';
            size++;
        }
    }

    return size;
}

string checkBoard(vector<string> board)
{
    int n = board.size();
    int m = board[0].size();

    int ships[] = { 4, 3, 2, 1 };
    for(int i = 0; i < n; i++ )
    {
        for( int j = 0;j < m; j++ )
        {
            if( board[ i ][ j ] == 'X' )
            {
                int size = bs( board, i, j );

                if( size <= 4 )
                {
                    ships[ size ] --;
                    cout << size << ' ' << ships[size] << endl;
                }
                else
                {
                    return "REJECTED";
                }
            }
        }
    }

    return "";
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
        string board_array[] = {"......X...", ".XXX..X...", "......X...", "X.X...X...", "X.........", "...XX.X...", "......X...", ".XX...X...", "..........", ".X.X..X..."};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        string expected = "ACCEPTED, 5 POINTS";
        BattleshipChecker theObject;
        eq(0, theObject.checkBoard(board), expected);
    }
    {
        string board_array[] = {"X.X.X.X...", "......X...", ".XX...X...", "......X...", "......X..X", "...X..X...", "...X..X...", "......X...", "..XX..X...", "......X..."};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        string expected = "REJECTED";
        BattleshipChecker theObject;
        eq(1, theObject.checkBoard(board), expected);
    }
    {
        string board_array[] = {".....XX...", ".XX.......", "..........", ".X....XXX.", ".X........", ".....X....", "..X..X....", ".....X....", "...X......", "X.....XXXX"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        string expected = "REJECTED";
        BattleshipChecker theObject;
        eq(2, theObject.checkBoard(board), expected);
    }
    {
        string board_array[] = {".....XX..X", ".XX......X", "..........", ".X....XXX.", ".X........", ".....X..X.", "..X..X....", ".....X....", "...X......", "X.....XXXX"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        string expected = "REJECTED";
        BattleshipChecker theObject;
        eq(3, theObject.checkBoard(board), expected);
    }
    {
        string board_array[] = {"X.......X.", "...XXXX...", ".X......X.", "....XX....", ".........X", ".........X", ".....XXX..", ".........X", "..X......X", "..X......X"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        string expected = "ACCEPTED, 0 POINTS";
        BattleshipChecker theObject;
        eq(4, theObject.checkBoard(board), expected);
    }
    {
        string board_array[] = {"X.......X.", "...XXXX...", ".X......X.", "....XX....", "...X.....X", "...X.....X", ".....XXX..", ".........X", ".........X", ".........X"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        string expected = "REJECTED";
        BattleshipChecker theObject;
        eq(5, theObject.checkBoard(board), expected);
    }

    return 0;
}
// END CUT HERE
