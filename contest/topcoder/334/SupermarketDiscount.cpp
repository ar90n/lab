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

class SupermarketDiscount {
public:
int disCount( int val )
{
    return ( val - ( 50 <= val ) * 10 );
}
int minAmount(vector<int> goods)
{
    int result = 1 << 29;
    int a = goods[0] + goods[1] + goods[2];
    int b = goods[0] + goods[1];
    int c = goods[1] + goods[2];
    int d = goods[0] + goods[2];

    result = min( result, disCount( a ));
    result = min( result, disCount( b ) + disCount( goods[2] ) );
    result = min( result, disCount( c ) + disCount( goods[0] ) );
    result = min( result, disCount( d ) + disCount( goods[1] ) );
    result = min( result, disCount( goods[0] ) +
                          disCount( goods[1] ) +
                          disCount( goods[2] ) );

    return result;

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
        int goods_array[] = {46, 62, 9};
        vector<int> goods(goods_array, goods_array + ARRSIZE(goods_array));
        int expected = 97;
        SupermarketDiscount theObject;
        eq(0, theObject.minAmount(goods), expected);
    }
    {
        int goods_array[] = {50, 62, 93};
        vector<int> goods(goods_array, goods_array + ARRSIZE(goods_array));
        int expected = 175;
        SupermarketDiscount theObject;
        eq(1, theObject.minAmount(goods), expected);
    }
    {
        int goods_array[] = {5, 31, 15};
        vector<int> goods(goods_array, goods_array + ARRSIZE(goods_array));
        int expected = 41;
        SupermarketDiscount theObject;
        eq(2, theObject.minAmount(goods), expected);
    }
    {
        int goods_array[] = {5, 3, 15};
        vector<int> goods(goods_array, goods_array + ARRSIZE(goods_array));
        int expected = 23;
        SupermarketDiscount theObject;
        eq(3, theObject.minAmount(goods), expected);
    }

    return 0;
}
// END CUT HERE
