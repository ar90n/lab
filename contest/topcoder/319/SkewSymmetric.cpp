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

class SkewSymmetric {
public:
int minChanges(vector<string> M)
{
    int n = M.size();
    char buf[1000];
    const char *del = " ";
    vector< vector< int > > mat;
    char *p;


    for( int i = 0; i < n; i++ )
    {
        vector< int > tmp;
        strcpy( buf, M[i].c_str() );
        p = strtok( buf,  del );
        while( p != NULL )
        {
            tmp.push_back( atoi( p ) );
            p = strtok( NULL,  del );

        }
        mat.push_back( tmp );
    }

    int count = 0;
    int m = mat[0].size();
    for( int i = 0; i < n; i++ )
    {
        for( int j = i; j < m; j++ )
        {
            if( mat[i][j] != -mat[j][i] )
            {
                count++;
            }
        }
    }

    return count;
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
        string M_array[] = {"1 2 8", "-2 1 0", "3 99 3"};
        vector<string> M(M_array, M_array + ARRSIZE(M_array));
        int expected = 5;
        SkewSymmetric theObject;
        eq(0, theObject.minChanges(M), expected);
    }
    {
        string M_array[] = {"0 1 1 1 1 1", "-1 0 1 1 1 1", "-1 -1 0 1 1 1", "-1 -1 -1 0 1 1", "-1 -1 -1 -1 0 1", "0 0 0 0 0 0"};
        vector<string> M(M_array, M_array + ARRSIZE(M_array));
        int expected = 5;
        SkewSymmetric theObject;
        eq(1, theObject.minChanges(M), expected);
    }
    {
        string M_array[] = {"0 0 0 0", "0 0 0 0", "0 0 0 0", "0 0 0 0"};
        vector<string> M(M_array, M_array + ARRSIZE(M_array));
        int expected = 0;
        SkewSymmetric theObject;
        eq(2, theObject.minChanges(M), expected);
    }
    {
        string M_array[] = {"1 0", "0 1"};
        vector<string> M(M_array, M_array + ARRSIZE(M_array));
        int expected = 2;
        SkewSymmetric theObject;
        eq(3, theObject.minChanges(M), expected);
    }

    return 0;
}
// END CUT HERE
