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

class CarolsSinging {
public:

int doit( vector<int> lyrics_mask, int pos, int mask, int num, int lyrics )
{
    if( mask == ( ( 1 << lyrics ) - 1 ) )
    {
        return num;
    }

    int min_num = 1 << 28;
    for( int i = pos + 1; i < lyrics_mask.size(); i++ )
    {
        min_num = min( min_num, doit( lyrics_mask,i, mask | lyrics_mask[i], num + 1, lyrics ) );
    }

    return min_num;
}

int choose(vector<string> lyrics)
{
    int n = lyrics.size();
    vector<int> lyrics_mask;

    for( int i = 0; i < lyrics[0].size(); i++ )
    {
        int mask = 0;

        for( int j = 0; j < n ; j++ )
        {
            mask |= ( ( lyrics[j][i] == 'Y' ) ? 1 : 0 ) << j;
        }

        lyrics_mask.push_back( mask );
    }

    int num = 1 << 29;
    for( int i = 0; i < lyrics[0].size(); i++ )
    {
        num = min( num, doit( lyrics_mask, i, lyrics_mask[i], 1, n ) );
    }

    return num;
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
        string lyrics_array[] = {"YN", "NY"};
        vector<string> lyrics(lyrics_array, lyrics_array + ARRSIZE(lyrics_array));
        int expected = 2;
        CarolsSinging theObject;
        eq(0, theObject.choose(lyrics), expected);
    }
    {
        string lyrics_array[] = {"YN", "YY", "YN"};
        vector<string> lyrics(lyrics_array, lyrics_array + ARRSIZE(lyrics_array));
        int expected = 1;
        CarolsSinging theObject;
        eq(1, theObject.choose(lyrics), expected);
    }
    {
        string lyrics_array[] = {"YNN", "YNY", "YNY", "NYY", "NYY", "NYN"};
        vector<string> lyrics(lyrics_array, lyrics_array + ARRSIZE(lyrics_array));
        int expected = 2;
        CarolsSinging theObject;
        eq(2, theObject.choose(lyrics), expected);
    }
    {
        string lyrics_array[] = {"YNNYYY", "YYNYYY", "YNNYYN", "NYYNNN", "YYYNNN", "YYYNNY", "NYYYYY", "NYNYYY", "NNNNYY", "YYYYYY", "YNNNNN", "YYYYNY", "YYNNNN", "NNYYYN", "NNNNYY", "YYYNNN", "NYNNYN", "YNNYYN", "YYNNNY", "NYYNNY", "NNYYYN", "YNYYYN", "NNNYNY", "YYYYNN", "YYNYNN", "NYYNYY", "YYNYYN"};
        vector<string> lyrics(lyrics_array, lyrics_array + ARRSIZE(lyrics_array));
        int expected = 4;
        CarolsSinging theObject;
        eq(3, theObject.choose(lyrics), expected);
    }
    return 0;
}
// END CUT HERE
