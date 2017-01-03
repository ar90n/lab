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

class BirthNumbersValidator {
public:
vector<string> validate(vector<string> test)
{
    vector<string> ret;
    int days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    for( int i = 0; i < test.size() ; i++ )
    {
        long long tmp = atoll( test[i].c_str() );

        if( ( tmp % 11 ) != 0 )
        {
            ret.push_back( "NO" );
            continue;
        }

        int cccc = tmp % 10000;
        tmp /= 10000;
        int dd = tmp % 100;
        tmp /= 100;
        int mm = tmp % 100;
        mm = ( 12 < mm ) ? mm - 50 : mm;
        tmp /= 100;
        int yy = tmp % 100;
        tmp /= 100;


        if(( yy % 4 ) == 0 )
        {
            days[1]  = 29;
        }
        else
        {
            days[1] = 28;
        }

        if( mm < 1 || 12 < mm )
        {
            ret.push_back( "NO" );
            continue;
        }

        if( dd < 1 || days[ mm - 1 ] < dd )
        {
            ret.push_back( "NO" );
            continue;
        }

        ret.push_back("YES");
    }

    return ret;
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
        string test_array[] = {"8104121234"};
        vector<string> test(test_array, test_array + ARRSIZE(test_array));
        string expected_array[] = {"YES"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        BirthNumbersValidator theObject;
        eq(0, theObject.validate(test), expected);
    }
    {
        string test_array[] = {"8154121239"};
        vector<string> test(test_array, test_array + ARRSIZE(test_array));
        string expected_array[] = {"YES"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        BirthNumbersValidator theObject;
        eq(1, theObject.validate(test), expected);
    }
    {
        string test_array[] = {"8134120005"};
        vector<string> test(test_array, test_array + ARRSIZE(test_array));
        string expected_array[] = {"NO"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        BirthNumbersValidator theObject;
        eq(2, theObject.validate(test), expected);
    }
    {
        string test_array[] = {"8102310007", "8104121235"};
        vector<string> test(test_array, test_array + ARRSIZE(test_array));
        string expected_array[] = {"NO", "NO"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        BirthNumbersValidator theObject;
        eq(3, theObject.validate(test), expected);
    }
    {
        string test_array[] = {"0411131237"};
        vector<string> test(test_array, test_array + ARRSIZE(test_array));
        string expected_array[] = {"YES"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        BirthNumbersValidator theObject;
        eq(4, theObject.validate(test), expected);
    }

    return 0;
}
// END CUT HERE
