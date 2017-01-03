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

class CarBuyer {
public:
double lowestCost(vector<string> cars, int fuelPrice, int annualDistance, int years)
{
    int n = cars.size();
    double min_cost;

    min_cost = (double)1e+10;
    for( int i = 0; i < n; i ++ )
    {
        int price;
        int tax;
        int efficiency;

        stringstream ss;
        ss << cars[i];
        ss >> price;
        ss >> tax;
        ss >> efficiency;

        double cost = price + ( tax  * years ) +  ( fuelPrice * annualDistance * years ) / (double)efficiency;
        min_cost = min( min_cost, cost );
    }
    return min_cost;
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
        string cars_array[] = {"10000 50 50", "12000 500 10", "15000 100 65", "20000 20 80", "25000 10 90"};
        vector<string> cars(cars_array, cars_array + ARRSIZE(cars_array));
        int fuelPrice = 2;
        int annualDistance = 5000;
        int years = 2;
        double expected = 10500.0;
        CarBuyer theObject;
        eq(0, theObject.lowestCost(cars, fuelPrice, annualDistance, years), expected);
    }
    {
        string cars_array[] = {"10000 50 50", "12000 500 10", "15000 100 65", "20000 20 80", "25000 10 90"};
        vector<string> cars(cars_array, cars_array + ARRSIZE(cars_array));
        int fuelPrice = 8;
        int annualDistance = 25000;
        int years = 10;
        double expected = 45200.0;
        CarBuyer theObject;
        eq(1, theObject.lowestCost(cars, fuelPrice, annualDistance, years), expected);
    }
    {
        string cars_array[] = {"8426 774 19", "29709 325 31", "30783 853 68", "20796 781 3", "27726 4 81", "20788 369 69", "17554 359 34", "12039 502 24", "6264 230 69", "14151 420 65", "25115 528 70", "22234 719 55", "2050 926 40", "18618 714 29", "173 358 57"};
        vector<string> cars(cars_array, cars_array + ARRSIZE(cars_array));
        int fuelPrice = 33;
        int annualDistance = 8673;
        int years = 64;
        double expected = 254122.44444444444;
        CarBuyer theObject;
        eq(2, theObject.lowestCost(cars, fuelPrice, annualDistance, years), expected);
    }

    return 0;
}
// END CUT HERE
