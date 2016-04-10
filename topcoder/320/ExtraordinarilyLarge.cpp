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

class ExtraordinarilyLarge {
public:
int fact( int n )
{
    int f = 1;

    if( 12 < n )
    {
        return -1;
    }

    for( int i = 1 ; i <= n; i++ )
    {
        f *= i;
    }

    return f;
}

string compare(string x, string y)
{
    if( x[0] == '0' && x[1] == '!' )
    {
        x[0] = '1';
    }

    if( y[0] == '0' && y[1] == '!' )
    {
        y[0] = '1';
    }

    int x_pos = x.find('!');
    int y_pos = y.find('!');

    if( x_pos == -1 )
    {
        x_pos = x.length();
    }

    if( y_pos == -1 )
    {
        y_pos = y.length();
    }

    int x_val = atoi( x.substr( 0, x_pos ).c_str() );
    int y_val = atoi( y.substr( 0, y_pos ).c_str() );

    int x_fact_num = x.length() - x_pos;
    int y_fact_num = y.length() - y_pos;

    int dif = x_fact_num - y_fact_num;
    if( dif == 0 )
    {
        if( x_val == y_val )
        {
            return "x=y";
        }
        else if( x_val < y_val )
        {
            return "x<y";
        }
        else{
            return "x>y";
        }
    }
    if( 0 < dif )
    {
        x_fact_num = dif;
        y_fact_num = 0;

        int res = x_val;

        for( int jj = 0; jj < x_fact_num ; jj++ )
        {
            if( 12 < res )
            {
                return "x>y";
            }

            res = fact( res );

            if( y_val == res )
            {
                return "x=y";
            }
            else if( y_val < res )
            {
                return "x>y";
            }
        }

        return "x<y";
    }
    else
    {
        x_fact_num = 0;
        y_fact_num = -dif;

        int res = y_val;

        for( int i = 0; i < y_fact_num; i++ )
        {
            if( 12 < res )
            {
                return "x<y";
            }
            res = fact( res );

            if( res == x_val )
            {
                return "x=y";
            }
            else if( x_val < res )
            {
                return "x<y";
            }
        }

        if( res == x_val )
        {
            return "x=y";
        }
        else{
            return "x>y";
        }
    }

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
        string x = "0!";
        string y = "1";
        string expected = "x=y";
        ExtraordinarilyLarge theObject;
        eq(0, theObject.compare(x, y), expected);
    }
    {
        string x = "9!";
        string y = "999999999";
        string expected = "x<y";
        ExtraordinarilyLarge theObject;
        eq(1, theObject.compare(x, y), expected);
    }
    {
        string x = "9!!";
        string y = "999999999";
        string expected = "x>y";
        ExtraordinarilyLarge theObject;
        eq(2, theObject.compare(x, y), expected);
    }
    {
        string x = "456!!!";
        string y = "123!!!!!!";
        string expected = "x<y";
        ExtraordinarilyLarge theObject;
        eq(3, theObject.compare(x, y), expected);
    }
    {
        string x = "5!";
        string y = "120";
        string expected = "x=y";
        ExtraordinarilyLarge theObject;
        eq(4, theObject.compare(x, y), expected);
    }
    {
        string x = "1";
        string y = "2";
        string expected = "x<y";
        ExtraordinarilyLarge theObject;
        eq(5, theObject.compare(x, y), expected);
    }

    return 0;
}
// END CUT HERE
