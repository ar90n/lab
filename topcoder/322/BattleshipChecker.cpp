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

vector< bool > rows( 10 );
vector< bool > cols( 10 );

class BattleshipChecker {
public:

int bs( vector< string > &board,  int i, int j )
{
    int size = 1;

    board[ i ][ j ] = '.';

    cols[ i ] = false;
    rows[ j ] = false;

    if( ( i + 1 ) < board.size() &&
        ( j + 1 ) < board[0].size() &&
        board[ i + 1 ][ j + 1 ] == 'X' )
    {
        return - 1;
    }

    if( 0 <= ( j - 1 ) &&
        ( i + 1 ) < board.size() &&
        board[ i + 1 ][ j - 1 ] == 'X' )
    {
        return - 1;
    }


    if( ( i + 1 ) < board.size() && board[ i + 1 ][ j ] == 'X' )
    {
        if( ( j + 1 ) < board[0].size() && board[ i ][ j + 1 ] == 'X' )
        {
            return -1;
        }

        int tmp_size = bs( board, i + 1, j );
        if( tmp_size < 0 )
        {
            return -1;
        }

        return 1 + tmp_size;
    }
    else if( (j + 1 ) < board[0].size() && board[ i ][ j + 1] == 'X' )
    {
        if( ( i + 1 ) < board.size() && board[ i + 1 ][ j ] == 'X' )
        {
            return -1;
        }

        int tmp_size = bs( board, i, j + 1 );
        if( tmp_size < 0 )
        {
            return -1;
        }

        return 1 + tmp_size;
    }

    return 1;
}

string checkBoard(vector<string> board)
{
    int n = board.size();
    int m = board[0].size();

    int ships[] = { 0, 4, 3, 2, 1 };

    for( int i = 0; i < rows.size(); i++ )
    {
        rows[ i ] = true;
    }

    for( int i = 0; i < cols.size(); i++ )
    {
        cols[ i ] = true;
    }

    for(int i = 0; i < n; i++ )
    {
        for( int j = 0;j < m; j++ )
        {
            if( board[ i ][ j ] == 'X' )
            {
                int size = bs( board, i, j );

                if( 0 < size && size <= 4 )
                {
                    ships[ size ] --;
                }
                else
                {
                    return "REJECTED";
                }
            }
        }
    }

    for( int i = 0; i <= 4; i++ )
    {
        if( ships[ i ] != 0 )
        {
            return "REJECTED";
        }
    }

    int score = 0;
    for( int i = 0; i < rows.size(); i++ )
    {
        if( rows[ i ] )
        {
            score++;
        }
    }

    for( int i = 0; i< cols.size(); i++ )
    {
        if( cols[ i ] )
        {
            score++;
        }
    }

    ostringstream ss;
    string res;
    ss << "ACCEPTED, "  << score << " POINTS";
    return ss.str();
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
