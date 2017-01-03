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

class RoadsAndFools {
public:
string determineOrientation(int length, vector<int> frontSides)
{
    int n = frontSides.size();
    bool isCross;
    bool isDec;
    stringstream ss;

    int min_dist = min( frontSides[ 0 ],  length - frontSides[ 0 ] );
    int max_dist = max( frontSides[ 0 ],  length - frontSides[ 0 ] );
    int interval = max_dist - min_dist;
    int min_length = 1;
    int last_interval;
    int min_interval = max_dist - min_dist;
    ss << min( min_dist,  max_dist );
    isCross = false;
    isDec = false;
    for( int i = 1; i < n; i++ )
    {
        min_dist = min( frontSides[ i ],  length - frontSides[ i ] );
        max_dist = max( frontSides[ i ],  length - frontSides[ i ] );
        last_interval = interval;
        interval = max_dist - min_dist;

        if( isCross && ( interval <= last_interval ) )
        {
            return "NO SOLUTION";
        }


        if( !isCross )
        {
            if( interval == last_interval )
            {
                min_length++;
            }
            else if( interval < last_interval )
            {
                min_interval = interval;
                min_length = 1;
            }
        }

        if( last_interval <= interval )
        {
            if( interval == 0 )
            {
                return "NO SOLUTION";
            }
            isCross = true;
        }

        if( !isCross )
        {
            ss << ' ' << min_dist;
        }
        else
        {
            ss << ' ' << max_dist;
        }

    }

    if( min_length == 1 )
    {
        if( min_interval == 0 )
        {
            return ss.str();
        }
        else
        {
            return "MULTIPLE SOLUTIONS";
        }
    }
    if( min_length == 2 )
    {
        return ss.str();
    }

    return "NO SOLUTION";
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
        int length = 5;
        int frontSides_array[] = {1, 2, 3};
        vector<int> frontSides(frontSides_array, frontSides_array + ARRSIZE(frontSides_array));
        string expected = "1 2 3";
        RoadsAndFools theObject;
        eq(0, theObject.determineOrientation(length, frontSides), expected);
    }
    {
        int length = 5;
        int frontSides_array[] = {4, 4};
        vector<int> frontSides(frontSides_array, frontSides_array + ARRSIZE(frontSides_array));
        string expected = "1 4";
        RoadsAndFools theObject;
        eq(2, theObject.determineOrientation(length, frontSides), expected);
    }
    {
        int length = 5;
        int frontSides_array[] = {4, 4, 4};
        vector<int> frontSides(frontSides_array, frontSides_array + ARRSIZE(frontSides_array));
        string expected = "NO SOLUTION";
        RoadsAndFools theObject;
        eq(3, theObject.determineOrientation(length, frontSides), expected);
    }
    {
        int length = 5;
        int frontSides_array[] = {3};
        vector<int> frontSides(frontSides_array, frontSides_array + ARRSIZE(frontSides_array));
        string expected = "MULTIPLE SOLUTIONS";
        RoadsAndFools theObject;
        eq(4, theObject.determineOrientation(length, frontSides), expected);
    }
    {
        int length = 10;
        int frontSides_array[] = {5};
        vector<int> frontSides(frontSides_array, frontSides_array + ARRSIZE(frontSides_array));
        string expected = "5";
        RoadsAndFools theObject;
        eq(5, theObject.determineOrientation(length, frontSides), expected);
    }
    return 0;
}
// END CUT HERE
