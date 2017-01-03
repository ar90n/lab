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

class Surname {
public:
int bestSurname(vector<string> surnames)
{
    int n = surnames.size();
    int res = 0;
    int ind = 0;
    for (int i = 0; i < n; i++) {
        int score = 0;
        for( int j = 0; j < surnames[i].size(); ++ j )
        {
            score += surnames[i][j] ;
        }

        if( res <  score )
        {
            ind = i;
            res = score;
        }
    }

    return ind;
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
        string surnames_array[] = {"JONES", "WILLIAMS", "SMITH", "HARPER"};
        vector<string> surnames(surnames_array, surnames_array + ARRSIZE(surnames_array));
        int expected = 1;
        Surname theObject;
        eq(0, theObject.bestSurname(surnames), expected);
    }
    {
        string surnames_array[] = {"JOHNSON", "CHU", "LOPEZ", "SCAFFIDI", "PASQUALE", "KRUMME"};
        vector<string> surnames(surnames_array, surnames_array + ARRSIZE(surnames_array));
        int expected = 4;
        Surname theObject;
        eq(1, theObject.bestSurname(surnames), expected);
    }
    {
        string surnames_array[] = {"ABA", "BAA", "CAB", "BAC"};
        vector<string> surnames(surnames_array, surnames_array + ARRSIZE(surnames_array));
        int expected = 2;
        Surname theObject;
        eq(2, theObject.bestSurname(surnames), expected);
    }
    {
        string surnames_array[] = {"BOHR", "FRANKLIN", "TESLA", "NEWTON", "EDISON", "FARNSWORTH", "GUTENBERG", "HOOVER", "MARCONI", "EINSTEIN"};
        vector<string> surnames(surnames_array, surnames_array + ARRSIZE(surnames_array));
        int expected = 5;
        Surname theObject;
        eq(3, theObject.bestSurname(surnames), expected);
    }
    {
        string surnames_array[] = {"BABBAGE", "BOOLE", "BRESENHAM", "TURING", "DIJKSTRA", "BACKUS", "FLOYD", "KERNIGHAN", "BOOCH", "NEUMANN"};
        vector<string> surnames(surnames_array, surnames_array + ARRSIZE(surnames_array));
        int expected = 7;
        Surname theObject;
        eq(4, theObject.bestSurname(surnames), expected);
    }

    return 0;
}
// END CUT HERE
