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

class CsCourses {
    vector< vector< int > > valid;
public:
vector<int> getOrder(vector<int> theoreticalValue, vector<int> practicalValue, vector<int> expire, int skillBound)
{
    long long mask = 0LL;
    int t = 0;
    int p = 0;
    vector<int> c;
    doit( theoreticalValue, practicalValue, expire, skillBound, mask, c, t, p, 0 );

    if( valid.size() == 0 )
    {
	    vector<int>tmp;
	    return tmp;
    }
    sort( valid.begin(), valid.end() );
    return valid[0];
}

void doit( vector< int > &tv, vector<int> &pv, vector<int> &exp, int sb, long long mask, vector<int> c, int t, int p, int m )
{
    if( ( sb <= t ) && ( sb <= p ) )
    {
	valid.push_back( c );
        return;
    }

    for( int i = 0;i < tv.size(); i++ )
    {
        if( ( ( tv[i] - 1 ) <=  t ) &&
            ( ( pv[i] - 1 ) <=  p ) &&
            (  m < exp[i]  )  &&
            ( ( mask & ( 1 << i ) ) == 0 )  )
        {
            vector< int > tmp;
	    for( int j = 0; j < c.size(); j++ )
	    {
		tmp.push_back( c[j] );
	    }
	    tmp.push_back( i );
            doit( tv, pv, exp, sb, mask | ( 1 << i ), tmp, max( tv[i], t ), max( pv[i], p ), m  + 1 );
        }
    }

    return;
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
        int theoreticalValue_array[] = {1};
        vector<int> theoreticalValue(theoreticalValue_array, theoreticalValue_array + ARRSIZE(theoreticalValue_array));
        int practicalValue_array[] = {1};
        vector<int> practicalValue(practicalValue_array, practicalValue_array + ARRSIZE(practicalValue_array));
        int expire_array[] = {1};
        vector<int> expire(expire_array, expire_array + ARRSIZE(expire_array));
        int skillBound = 1;
        int expected_array[] = {0 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        CsCourses theObject;
        eq(0, theObject.getOrder(theoreticalValue, practicalValue, expire, skillBound), expected);
    }
    {
        int theoreticalValue_array[] = {1, 2, 1};
        vector<int> theoreticalValue(theoreticalValue_array, theoreticalValue_array + ARRSIZE(theoreticalValue_array));
        int practicalValue_array[] = {2, 1, 1};
        vector<int> practicalValue(practicalValue_array, practicalValue_array + ARRSIZE(practicalValue_array));
        int expire_array[] = {5, 5, 5};
        vector<int> expire(expire_array, expire_array + ARRSIZE(expire_array));
        int skillBound = 2;
        int expected_array[] = {2, 0, 1 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        CsCourses theObject;
        eq(1, theObject.getOrder(theoreticalValue, practicalValue, expire, skillBound), expected);
    }
    {
        int theoreticalValue_array[] = {1, 2, 1};
        vector<int> theoreticalValue(theoreticalValue_array, theoreticalValue_array + ARRSIZE(theoreticalValue_array));
        int practicalValue_array[] = {2, 1, 1};
        vector<int> practicalValue(practicalValue_array, practicalValue_array + ARRSIZE(practicalValue_array));
        int expire_array[] = {1, 1, 1};
        vector<int> expire(expire_array, expire_array + ARRSIZE(expire_array));
        int skillBound = 2;
        int expected_array[] = { };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        CsCourses theObject;
        eq(2, theObject.getOrder(theoreticalValue, practicalValue, expire, skillBound), expected);
    }
    {
        int theoreticalValue_array[] = {1, 2, 1};
        vector<int> theoreticalValue(theoreticalValue_array, theoreticalValue_array + ARRSIZE(theoreticalValue_array));
        int practicalValue_array[] = {2, 1, 1};
        vector<int> practicalValue(practicalValue_array, practicalValue_array + ARRSIZE(practicalValue_array));
        int expire_array[] = {3, 2, 1};
        vector<int> expire(expire_array, expire_array + ARRSIZE(expire_array));
        int skillBound = 2;
        int expected_array[] = {2, 1, 0 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        CsCourses theObject;
        eq(3, theObject.getOrder(theoreticalValue, practicalValue, expire, skillBound), expected);
    }
    {
        int theoreticalValue_array[] = {1, 2, 3, 4, 5, 6, 7};
        vector<int> theoreticalValue(theoreticalValue_array, theoreticalValue_array + ARRSIZE(theoreticalValue_array));
        int practicalValue_array[] = {1, 2, 3, 4, 5, 6, 7};
        vector<int> practicalValue(practicalValue_array, practicalValue_array + ARRSIZE(practicalValue_array));
        int expire_array[] = {1, 2, 3, 4, 5, 6, 7};
        vector<int> expire(expire_array, expire_array + ARRSIZE(expire_array));
        int skillBound = 7;
        int expected_array[] = {0, 1, 2, 3, 4, 5, 6 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        CsCourses theObject;
        eq(4, theObject.getOrder(theoreticalValue, practicalValue, expire, skillBound), expected);
    }
    {
        int theoreticalValue_array[] = {0, 1, 2, 2, 1};
        vector<int> theoreticalValue(theoreticalValue_array, theoreticalValue_array + ARRSIZE(theoreticalValue_array));
        int practicalValue_array[] = {0, 0, 1, 2, 1};
        vector<int> practicalValue(practicalValue_array, practicalValue_array + ARRSIZE(practicalValue_array));
        int expire_array[] = {9, 9, 9, 9, 9};
        vector<int> expire(expire_array, expire_array + ARRSIZE(expire_array));
        int skillBound = 2;
        int expected_array[] = {4, 3 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        CsCourses theObject;
        eq(5, theObject.getOrder(theoreticalValue, practicalValue, expire, skillBound), expected);
    }

    return 0;
}
// END CUT HERE
