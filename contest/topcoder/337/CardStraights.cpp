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

class CardStraights {
public:
int longestStraight(vector<int> cards)
{
    int jokers = count( cards.begin(), cards.end() , 0 );
    vector<int>::iterator it = cards.begin();
    set<int> c;
    while( it != cards.end() )
    {
        if( *it != 0 )
        {
            c.insert( *it );
        }

        it++;
    }

    set<int>::iterator cit = c.begin();
    vector< vector<int> > diff;
    while( cit != c.end() )
    {
        vector<int> tmp_diff;
        set<int>::iterator tmp_it = cit;
        int b = *tmp_it++;
        while( tmp_it != c.end() )
        {
            int a = *tmp_it++;
            tmp_diff.push_back( a - b - 1 );
            b = a;
        }
        diff.push_back( tmp_diff );
        cit++;
    }

    int max_len = jokers + ( diff.size() == 1 ) ;
    for( int i = 0; i < diff.size(); i ++ )
    {
        int len = 1;
        int tmp_jokers = jokers;
        for( int j = 0; j < diff[i].size();j++)
        {
            if( 0 <= ( tmp_jokers - diff[i][j] ) )
            {
                len += diff[i][j] + 1;
                tmp_jokers -= diff[i][j];
            }
            else
            {
                break;
            }
        }
        len += tmp_jokers;
        max_len = max( len, max_len );
    }

    return max_len;
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
        int cards_array[] = {0,6,5,10,3,0,11};
        vector<int> cards(cards_array, cards_array + ARRSIZE(cards_array));
        int expected = 5;
        CardStraights theObject;
        eq(0, theObject.longestStraight(cards), expected);
    }
    {
        int cards_array[] = {100,100,100,101,100,99,97,103};
        vector<int> cards(cards_array, cards_array + ARRSIZE(cards_array));
        int expected = 3;
        CardStraights theObject;
        eq(1, theObject.longestStraight(cards), expected);
    }
    {
        int cards_array[] = {0,0,0,1,2,6,8,1000};
        vector<int> cards(cards_array, cards_array + ARRSIZE(cards_array));
        int expected = 6;
        CardStraights theObject;
        eq(2, theObject.longestStraight(cards), expected);
    }
    {
        int cards_array[] = {1,9,5,7,3,4,0,0,0,10};
        vector<int> cards(cards_array, cards_array + ARRSIZE(cards_array));
        int expected = 10;
        CardStraights theObject;
        eq(3, theObject.longestStraight(cards), expected);
    }
    return 0;
}
// END CUT HERE
