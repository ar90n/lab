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

class IncompleteBST {
public:
string missingValues(vector<string> tree)
{
    int n = tree.size();
    int invalid_ind;
    map< int, char > map_tree;

    for( int i = 0; i < n; i++ )
    {
        char val = tree[i][0];
        int ind = atoi( &( tree[i][2] ) );

        map_tree.insert( map<int, char>::value_type( ind, val ) );

        if( val == '?' )
        {
            invalid_ind = ind;
        }
    }

    int right = 'Z';
    int left = 'A';
    int cur_ind = invalid_ind;
    int next_ind = cur_ind / 2;
    if( ( cur_ind % 2  ) == 0 )
    {
        right = map_tree[ next_ind ];

        while( ( cur_ind % 2 ) == 0 )
        {
            cur_ind = next_ind;
            next_ind /= 2;
        }

        if( 0 < next_ind )
            left = ( left < map_tree[ next_ind ] ) ? map_tree[ next_ind ] + 1 : left;
    }
    else
    {
        left = map_tree[ next_ind ] + 1;
        while( ( cur_ind % 2 ) == 1 )
        {
            cur_ind = next_ind;
            next_ind /= 2;
        }

        if( 0 < next_ind )
            right = ( map_tree[ next_ind ] < right ) ? map_tree[ next_ind ] : right;
    }

    string aa = "";
    for( char q = left; q <= right; q++ )
    {
        aa += q;
    }

    return aa;
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
        string tree_array[] = {"A 1", "? 2"};
        vector<string> tree(tree_array, tree_array + ARRSIZE(tree_array));
        string expected = "A";
        IncompleteBST theObject;
        eq(0, theObject.missingValues(tree), expected);
    }
    {
        string tree_array[] = {"B 1", "? 2"};
        vector<string> tree(tree_array, tree_array + ARRSIZE(tree_array));
        string expected = "AB";
        IncompleteBST theObject;
        eq(1, theObject.missingValues(tree), expected);
    }
    {
        string tree_array[] = {"V 1", "? 3"};
        vector<string> tree(tree_array, tree_array + ARRSIZE(tree_array));
        string expected = "WXYZ";
        IncompleteBST theObject;
        eq(2, theObject.missingValues(tree), expected);
    }
    {
        string tree_array[] = {"K 1", "K 2", "A 6", "? 12", "Y 3"};
        vector<string> tree(tree_array, tree_array + ARRSIZE(tree_array));
        string expected = "";
        IncompleteBST theObject;
        eq(3, theObject.missingValues(tree), expected);
    }
    {
        string tree_array[] = {"Z 1", "Y 2", "X 4", "W 8", "V 16", "U 32", "T 64", "S 128", "R 256", "Q 512", "P 1024", "O 2048", "N 4096", "M 8192", "L 16384", "K 32768", "J 65536", "? 131072"};
        vector<string> tree(tree_array, tree_array + ARRSIZE(tree_array));
        string expected = "ABCDEFGHIJ";
        IncompleteBST theObject;
        eq(4, theObject.missingValues(tree), expected);
    }

    return 0;
}
// END CUT HERE
