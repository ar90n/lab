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

class ProbabilisticTranslator {
public:
int maximumFidelity(vector<string> text, vector<string> dictionary, vector<string> frequencies)
{
    vector< string > t;
    for( int i = 0; i < text.size(); i++ )
    {
        stringstream ss;
        ss << text[i];
        while( !ss.eof() )
        {
            string tmp;
            ss >> tmp;
            t.push_back( tmp );
            ss.ignore();
        }
    }

    multimap< string,string > dict;
    for( int i = 0; i < dictionary.size(); i++ )
    {
        stringstream ss;
        string key,val;

        for( int j = 0; j < dictionary[i].size(); j++ )
        {
            if( dictionary[i][j] == ':' )
            {
                dictionary[i][j] = ' ';
                break;
            }
        }
        ss << dictionary[i];
        ss >> key;
        ss.ignore();

        while( !ss.eof() )
        {
            ss >> val;
            dict.insert( make_pair( key, val ) );
            ss.ignore();
        }
    }

    map< pair< string, string >, int > freq;
    for( int i = 0; i < frequencies.size(); i++ )
    {
        stringstream ss;
        string from;
        string to;
        int f;
        ss << frequencies[i];
        ss >> from;
        ss >> to;
        ss >> f;

        freq.insert( make_pair( make_pair( from,to ), f ) );
    }

    int dp[2][ 55 ] = {0};
    int i;
    for( i = 1; i < t.size(); i++ )
    {
        int current_index = i & 1;
        int last_index = (i ^ 1 )&1;
        string current_char  = t[i];
        string last_char = t[i - 1];

        pair< multimap< string,string>::iterator,multimap< string,string>::iterator > cp = dict.equal_range( current_char );
        int current_score_index = 0;

        while( cp.first != cp.second )
        {
            pair< multimap< string,string>::iterator,multimap< string,string>::iterator > lp = dict.equal_range( last_char );
            int last_score_index = 0;
            string current_code = (*(cp.first)).second;
            int max_score = 0;

            while( lp.first != lp.second )
            {
                string last_code = (*(lp.first)).second;

                max_score = max( max_score , dp[ last_index ][ last_score_index ] + freq[ make_pair( last_code, current_code ) ] );
                lp.first++;
                last_score_index++;
            }

            dp[ current_index ][ current_score_index ] = max_score ;
            cp.first++;
            current_score_index++;
        }

    }

    i = ( i ^ 1 ) & 1;
    int mmm = 0;
    for( int j = 0; j < 55; j++ )
    {
        mmm = max( mmm, dp[i][j] );
    }

    return mmm;
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
        string text_array[] = {"a b c"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string dictionary_array[] = {"a : x y", "b : y z", "c : x z"};
        vector<string> dictionary(dictionary_array, dictionary_array + ARRSIZE(dictionary_array));
        string frequencies_array[] = {"y z 20", "x y 10", "z x 5"};
        vector<string> frequencies(frequencies_array, frequencies_array + ARRSIZE(frequencies_array));
        int expected = 30;
        ProbabilisticTranslator theObject;
        eq(0, theObject.maximumFidelity(text, dictionary, frequencies), expected);
    }
    {
        string text_array[] = {"a b c"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string dictionary_array[] = {"a : x y", "b : p q", "c : x y"};
        vector<string> dictionary(dictionary_array, dictionary_array + ARRSIZE(dictionary_array));
        string frequencies_array[] = {"x p 100", "x q 10", "q x 10"};
        vector<string> frequencies(frequencies_array, frequencies_array + ARRSIZE(frequencies_array));
        int expected = 100;
        ProbabilisticTranslator theObject;
        eq(1, theObject.maximumFidelity(text, dictionary, frequencies), expected);
    }
    {
        string text_array[] = {"a b", "c"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string dictionary_array[] = {"a : x y", "b : p q", "c : x y"};
        vector<string> dictionary(dictionary_array, dictionary_array + ARRSIZE(dictionary_array));
        string frequencies_array[] = {"x p 100", "x q 97", "q x 97"};
        vector<string> frequencies(frequencies_array, frequencies_array + ARRSIZE(frequencies_array));
        int expected = 194;
        ProbabilisticTranslator theObject;
        eq(2, theObject.maximumFidelity(text, dictionary, frequencies), expected);
    }
    {
        string text_array[] = {"a", "bb c"};
        vector<string> text(text_array, text_array + ARRSIZE(text_array));
        string dictionary_array[] = {"a : a a", "bb : b", "c : c", "ccc : a"};
        vector<string> dictionary(dictionary_array, dictionary_array + ARRSIZE(dictionary_array));
        string frequencies_array[] = {"y z 2", "x y 10", "z x 5"};
        vector<string> frequencies(frequencies_array, frequencies_array + ARRSIZE(frequencies_array));
        int expected = 0;
        ProbabilisticTranslator theObject;
        eq(3, theObject.maximumFidelity(text, dictionary, frequencies), expected);
    }

    return 0;
}
// END CUT HERE
