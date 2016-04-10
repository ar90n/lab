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

class InboxCleanup {
public:
int fewestClicks(string messages, int low, int high)
{
    int min = 1 << 29;
    for( int mails = low; mails <= high; mails++ )
    {
        int clicks = 0;
        for( int i = 0; i < messages.size(); i += mails )
        {
            int d_count = 0;
            for( int j = i; j < ( i + mails ) ; j++ )
            {
                if( messages[ j ] == 'D' )
                {
                    d_count++;
                }
            }

            if( d_count == 0 )
            {
                clicks += 0;
            }
            else if( ( mails / 2 ) < d_count )
            {
                clicks += mails - d_count  + 2;
            }
            else
            {
                clicks += d_count + 1;
            }

            clicks++;
        }
        clicks--;

        if( clicks <= min )
        {
            min = clicks;
        }
    }

    return min;
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
        string messages = "..........";
        int low = 5;
        int high = 10;
        int expected = 0;
        InboxCleanup theObject;
        eq(0, theObject.fewestClicks(messages, low, high), expected);
    }
    {
        string messages = ".D.D.DD.D.";
        int low = 5;
        int high = 5;
        int expected = 8;
        InboxCleanup theObject;
        eq(1, theObject.fewestClicks(messages, low, high), expected);
    }
    {
        string messages = "...D..DDDDDD...D.DD..";
        int low = 3;
        int high = 10;
        int expected = 12;
        InboxCleanup theObject;
        eq(2, theObject.fewestClicks(messages, low, high), expected);
    }
    {
        string messages = "D.D..D..DD.DDDD.D.DDD.DDDD..";
        int low = 3;
        int high = 11;
        int expected = 17;
        InboxCleanup theObject;
        eq(3, theObject.fewestClicks(messages, low, high), expected);
    }
    {
        string messages = "DDD.........................";
        int low = 1;
        int high = 3;
        int expected = 11;
        InboxCleanup theObject;
        eq(4, theObject.fewestClicks(messages, low, high), expected);
    }

    return 0;
}
// END CUT HERE
