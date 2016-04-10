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

class MedianOfNumbers {
public:
int findMedian(vector<int> numbers)
{
    int n = numbers.size();

    sort( numbers.begin(), numbers.end() );
    if( n % 2 == 0 )
    {
        return -1;
    }

    return numbers[ n / 2 ];
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
        int numbers_array[] = {1, 4, 2, 5, 7};
        vector<int> numbers(numbers_array, numbers_array + ARRSIZE(numbers_array));
        int expected = 4;
        MedianOfNumbers theObject;
        eq(0, theObject.findMedian(numbers), expected);
    }
    {
        int numbers_array[] = {1, 5, 8, 3};
        vector<int> numbers(numbers_array, numbers_array + ARRSIZE(numbers_array));
        int expected = -1;
        MedianOfNumbers theObject;
        eq(1, theObject.findMedian(numbers), expected);
    }
    {
        int numbers_array[] = {7};
        vector<int> numbers(numbers_array, numbers_array + ARRSIZE(numbers_array));
        int expected = 7;
        MedianOfNumbers theObject;
        eq(2, theObject.findMedian(numbers), expected);
    }
    {
        int numbers_array[] = {7, 12};
        vector<int> numbers(numbers_array, numbers_array + ARRSIZE(numbers_array));
        int expected = -1;
        MedianOfNumbers theObject;
        eq(3, theObject.findMedian(numbers), expected);
    }
    {
        int numbers_array[] = {66, 53, 47, 86, 18, 21, 97, 92, 15};
        vector<int> numbers(numbers_array, numbers_array + ARRSIZE(numbers_array));
        int expected = 53;
        MedianOfNumbers theObject;
        eq(4, theObject.findMedian(numbers), expected);
    }
    {
        int numbers_array[] = {32, 54, 27, 4, 69, 96, 73, 1, 100, 15, 21};
        vector<int> numbers(numbers_array, numbers_array + ARRSIZE(numbers_array));
        int expected = 32;
        MedianOfNumbers theObject;
        eq(5, theObject.findMedian(numbers), expected);
    }

    return 0;
}
// END CUT HERE
