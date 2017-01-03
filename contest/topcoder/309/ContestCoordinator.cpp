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

class ContestCoordinator {
public:
double bestAverage(vector<int> scores)
{
    int n = scores.size();
    double max_ave;

    sort( scores.begin(), scores.end() );
    max_ave = 0.0;
    for(int  i = 0; i <= n / 2; i++ )
    {
        int sum = 0;
        for(int j = i; j < n - i ;j++ )
        {
            sum += scores[j];
        }
        max_ave = max( max_ave,  (double)sum / ( n - 2 * i + ( n & 1 == 0 ) ) );
    }

    return max_ave;
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
        int scores_array[] = {1};
        vector<int> scores(scores_array, scores_array + ARRSIZE(scores_array));
        double expected = 1.0;
        ContestCoordinator theObject;
        eq(0, theObject.bestAverage(scores), expected);
    }
    {
        int scores_array[] = {1,2,3,4};
        vector<int> scores(scores_array, scores_array + ARRSIZE(scores_array));
        double expected = 2.5;
        ContestCoordinator theObject;
        eq(1, theObject.bestAverage(scores), expected);
    }
    {
        int scores_array[] = {1,1,999,999,1000,1000};
        vector<int> scores(scores_array, scores_array + ARRSIZE(scores_array));
        double expected = 999.0;
        ContestCoordinator theObject;
        eq(2, theObject.bestAverage(scores), expected);
    }
    {
        int scores_array[] = {1,13,8,6,7,9};
        vector<int> scores(scores_array, scores_array + ARRSIZE(scores_array));
        double expected = 7.5;
        ContestCoordinator theObject;
        eq(3, theObject.bestAverage(scores), expected);
    }
    {
        int scores_array[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
        vector<int> scores(scores_array, scores_array + ARRSIZE(scores_array));
        double expected = 1.0;
        ContestCoordinator theObject;
        eq(4, theObject.bestAverage(scores), expected);
    }

    return 0;
}
// END CUT HERE
