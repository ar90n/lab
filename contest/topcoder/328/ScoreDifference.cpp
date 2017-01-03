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

class ScoreDifference {
public:
int memo[ 1 << 16 ];
int b[16];
int score1;
int score2;

int isMovable( int mask,int i )
{
    if( ( i == 5 ) ||
        ( i == 6 ) ||
        ( i == 9 ) ||
        ( i == 10 ) )
    {
        return   ( ( ( 1 << ( i - 1 ) ) | ( 1 << ( i + 1 ) ) | ( 1 << ( i - 4 ) ) | ( 1 << ( i + 4 ) ) ) & mask ) != 0;
    }
    else
    {
        return 1;
    }

}


int min_max( int turn,  int mask,int depth )
{
 //   printf("%d %x %d\n",turn,mask,depth );
//    getchar();
    if( memo[ mask ] != -1 )
        return memo[ mask ];
    if( depth == 0 )
    {
        return 0;
    }

    if( turn == 1 )
    {
        int cc = -1;
        for( int i = 0; i < 16; i++ )
        {
            int nm = mask | ( 1 << i );
            if( ( mask ^ nm )  != 0 )
            {
                if( isMovable( mask,i ) )
                {
                int ccc;
                ccc = b[ i ] +  min_max( 0, nm,depth - 1);
                cc = max( cc, ccc );
                }
            }
        }
        if( memo[ mask ] == -1 )
            memo[ mask ] = cc;
        return cc;
    }
    else
    {
        int cc = 1 << 29;
        int mm;
        for( int i = 0; i < 16; i++ )
        {
            int nm = mask | ( 1 << i );
            if( ( mask ^ nm ) != 0 )
            {
                if( isMovable( mask,i ) )
                {
                int ccc;
                ccc = -b[i] + min_max( 1, nm,depth - 1);
                cc = min( cc, ccc );
                }
            }
        }

        if( memo[ mask ] == -1 )
            memo[ mask ] = cc;
        return cc;
    }
}

int maximize(vector<string> board)
{
    int mask = 0;

    for( int i = 0; i < 4; i++)
    {
        stringstream ss;
        ss << board[i];
        for( int j = 0; j < 4; j ++ )
        {
            ss >> b[ i * 4 + j ];
        }
    }
    memset( memo,0xff,sizeof( int ) * ( 1 << 16 ) );

    int res = min_max( 1, mask, 16 );

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
        string board_array[] = {"12 4 5 13", "3 14 16 9", "11 6 15 8", "2 1 7 10"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 2;
        ScoreDifference theObject;
        eq(0, theObject.maximize(board), expected);
    }
    {
        string board_array[] = {"15 16 11 12", "10 13 4 7", "1 2 8 6", "9 5 3 14"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 6;
        ScoreDifference theObject;
        eq(1, theObject.maximize(board), expected);
    }
    {
        string board_array[] = {"6 8 1 16", "10 15 9 3", "2 5 7 14", "13 12 11 4"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 10;
        ScoreDifference theObject;
        eq(2, theObject.maximize(board), expected);
    }
    {
        string board_array[] = {"9 8 3 1", "10 6 15 5", "12 7 4 11", "14 13 16 2"};
        vector<string> board(board_array, board_array + ARRSIZE(board_array));
        int expected = 16;
        ScoreDifference theObject;
        eq(3, theObject.maximize(board), expected);
    }

    return 0;
}
// END CUT HERE
