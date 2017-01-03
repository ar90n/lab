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

class tree_node
{
public:
    char val;
    vector< struct tree_node > children;
    tree_node(){ val = 0; };
    tree_node( char v )
    {
        val = v;
    };
};

class forest
{
public:
        forest(){};
        tree_node roots;
        bool append( string current_word )
        {
            tree_node *current_node = &roots;
            for( int i = 0; i < current_word.size() ;i++ )
            {
                int j;
                for( j = 0; j < current_node->children.size(); j++ )
                {
                    if( current_word[ i ] == current_node->children[j].val )
                    {
                        break;
                    }
                }
                if( j == current_node->children.size() )
                {
                    current_node->children.push_back( tree_node( current_word.c_str()[ i ] ) );
                }

                current_node = &( current_node->children[ j ] );
            }

        }
        int count_leaves()
        {
            int sum = 0;
            for( int i = 0; i < roots.children.size() ; i++ )
            {
                sum += _depth_search( roots.children[i] );
            }

            return sum;
        }
        int _depth_search( tree_node current)
        {
            if( current.children.size() == 0 )
            {
                return 1;
            }
            else
            {
                int sum = 0;
                for( int i = 0; i < current.children.size();i++)
                {
                    sum += _depth_search( current.children[ i ] );
                }
                return sum;
            }
        }
};

class PrefixFreeSets {
public:
    int maxElements(vector<string> words)
    {
        class forest wf;
        for( int i = 0; i < words.size(); i++ )
        {
            wf.append( words[i] );
        }
        return wf.count_leaves();
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
        string words_array[] = {"hello", "hi", "h", "run", "rerun", "running"};
        vector<string> words(words_array, words_array + ARRSIZE(words_array));
        int expected = 4;
        PrefixFreeSets theObject;
        eq(0, theObject.maxElements(words), expected);
    }
    {
        string words_array[] = {"a", "b", "cba", "cbc", "cbb", "ccc"};
        vector<string> words(words_array, words_array + ARRSIZE(words_array));
        int expected = 6;
        PrefixFreeSets theObject;
        eq(1, theObject.maxElements(words), expected);
    }
    {
        string words_array[] = {"a", "ab", "abc", "abcd", "abcde", "abcdef"};
        vector<string> words(words_array, words_array + ARRSIZE(words_array));
        int expected = 1;
        PrefixFreeSets theObject;
        eq(2, theObject.maxElements(words), expected);
    }
    {
        string words_array[] = {"topcoder", "topcoder", "topcoding"};
        vector<string> words(words_array, words_array + ARRSIZE(words_array));
        int expected = 2;
        PrefixFreeSets theObject;
        eq(3, theObject.maxElements(words), expected);
    }

    return 0;
}
// END CUT HERE
