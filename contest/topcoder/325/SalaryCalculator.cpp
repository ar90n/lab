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

class SalaryCalculator {
public:
double calcHours(int P1, int P2, int salary)
{
    if( salary <=  200 * P1 )
    {
        return (double)salary / P1;
    }

    salary -= 200 * P1;
    return 200.0 + (double)salary/ P2;
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
        int P1 = 10;
        int P2 = 15;
        int salary = 1000;
        double expected = 100.0;
        SalaryCalculator theObject;
        eq(0, theObject.calcHours(P1, P2, salary), expected);
    }
    {
        int P1 = 10;
        int P2 = 15;
        int salary = 3000;
        double expected = 266.6666666666667;
        SalaryCalculator theObject;
        eq(1, theObject.calcHours(P1, P2, salary), expected);
    }
    {
        int P1 = 100;
        int P2 = 1;
        int salary = 1000000;
        double expected = 980200.0;
        SalaryCalculator theObject;
        eq(2, theObject.calcHours(P1, P2, salary), expected);
    }
    {
        int P1 = 17;
        int P2 = 23;
        int salary = 973546;
        double expected = 42380.260869565216;
        SalaryCalculator theObject;
        eq(3, theObject.calcHours(P1, P2, salary), expected);
    }
    {
        int P1 = 82;
        int P2 = 8;
        int salary = 12140;
        double expected = 148.0487804878049;
        SalaryCalculator theObject;
        eq(4, theObject.calcHours(P1, P2, salary), expected);
    }

    return 0;
}
// END CUT HERE
