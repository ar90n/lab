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

class FixedPoint {
public:
vector<double> find(double scale, vector<double> translate, double rotate)
{
    double c = cos( rotate );
    double s = sin( rotate );

    double a11 = -scale + c;
    double a12 = -s;
    double a21 = s;
    double a22 = -scale + c;
    double denom = scale * scale - 2 * scale * c + 1.0 ;
    double xx = ( a11 * translate[0] + a12 * translate[1] ) / denom ;
    double yy = ( a21 * translate[0] + a22 * translate[1] ) / denom ;

    vector< double > res;
    res.push_back( xx );
    res.push_back( yy );

    return res;

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
        double scale = 0.5;
        double translate_array[] = {1, 0};
        vector<double> translate(translate_array, translate_array + ARRSIZE(translate_array));
        double rotate = 1.5707963267948966;
        double expected_array[] = {-0.39999999999999997, 0.8 };
        vector<double> expected(expected_array, expected_array + ARRSIZE(expected_array));
        FixedPoint theObject;
        eq(0, theObject.find(scale, translate, rotate), expected);
    }
    {
        double scale = 0.5;
        double translate_array[] = {2, 0};
        vector<double> translate(translate_array, translate_array + ARRSIZE(translate_array));
        double rotate = 0;
        double expected_array[] = {4.0, 0.0 };
        vector<double> expected(expected_array, expected_array + ARRSIZE(expected_array));
        FixedPoint theObject;
        eq(1, theObject.find(scale, translate, rotate), expected);
    }
    {
        double scale = 0.1;
        double translate_array[] = {0, 0};
        vector<double> translate(translate_array, translate_array + ARRSIZE(translate_array));
        double rotate = 2;
        double expected_array[] = {0.0, -0.0 };
        vector<double> expected(expected_array, expected_array + ARRSIZE(expected_array));
        FixedPoint theObject;
        eq(2, theObject.find(scale, translate, rotate), expected);
    }
    {
        double scale = 0.5;
        double translate_array[] = {1, 2};
        vector<double> translate(translate_array, translate_array + ARRSIZE(translate_array));
        double rotate = 0.785398163397;
        double expected_array[] = {-2.223469992542095, 2.065452845425795 };
        vector<double> expected(expected_array, expected_array + ARRSIZE(expected_array));
        FixedPoint theObject;
        eq(3, theObject.find(scale, translate, rotate), expected);
    }

    return 0;
}
// END CUT HERE
