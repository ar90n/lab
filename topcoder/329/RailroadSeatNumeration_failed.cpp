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

class RailroadSeatNumeration {
public:

int check_domestic( int tickets )
{
    if( 36 < tickets )
    {
        return -1;
    }

    int a = tickets % 10;
    int b = tickets / 10;

    if( b == 0 )
    {
        return 1;
    }

    if( ( a == 1 ) ||
        ( a == 3 ) ||
        ( a == 4 ) ||
        ( a == 6 ) )
    {
        return 0;
    }

    return 1;
}

int check_international( int tickets )
{
    int a= tickets % 10;
    int b = tickets / 10;

    if(  b == 0 )
    {
        return -1;
    }

    if( ( a != 1 ) &&
        ( a != 3 ) &&
        ( a != 4 ) &&
        ( a != 6 ) )
    {
        return -1;
    }

    if( ( 0 < b ) && ( b <= 3 ) )
    {
        return 0;
    }

    return 1;
}

string getInternational(vector<int> tickets)
{
    int n = tickets.size();
    int type;
    int illegal1,illegal2;

    type = 0;
    illegal1 = illegal2 = 0;
    for( int i = 0; i < n; i++ )
    {
        int a = check_domestic( tickets[ i] ) ;
        int b = check_international( tickets[ i] ) ;

        if( a == 1 )
        {
            type = 1;
            break;
        }

        if( b == 1 )
        {
            type = 2;
            break;
        }

        if( a == -1 )
        {
            illegal1 = 1;
        }
        if( b == -1 )
        {
            illegal2 = 1;
        }

        if( illegal1 && illegal2 )
        {
            return "BAD DATA";
        }
    }

    if( type == 0 )
    {
        return "AMBIGUOUS";
    }

    if( type == 1 )
    {
        for( int i = 0; i < n; i++ )
        {
            int a = ( ( tickets[i] - 1) % 4 ) + 1 ;
            int b = ( ( tickets[i] - 1 ) / 4 ) + 1;

            switch(a)
            {
                case 1:
                    a = 1;
                    break;
                case 2:
                    a = 3;
                    break;
                case 3:
                    a = 4;
                    break;
                case 4:
                    a = 6;
                    break;
            }
            int c = 10 * b + a;

            tickets[i] = c;

        }
    }

    stringstream ss;
    ss << tickets[0];
    for( int i = 1; i <  n ; i++ )
    {
        ss << ' ' << tickets[i];
    }

    return ss.str();

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
        int tickets_array[] = {1};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "11";
        RailroadSeatNumeration theObject;
        eq(0, theObject.getInternational(tickets), expected);
    }
    {
        int tickets_array[] = {11};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "AMBIGUOUS";
        RailroadSeatNumeration theObject;
        eq(1, theObject.getInternational(tickets), expected);
    }
    {
        int tickets_array[] = {45};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "BAD DATA";
        RailroadSeatNumeration theObject;
        eq(2, theObject.getInternational(tickets), expected);
    }
    {
        int tickets_array[] = {5, 7, 6};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "21 24 23";
        RailroadSeatNumeration theObject;
        eq(3, theObject.getInternational(tickets), expected);
    }
    {
        int tickets_array[] = {21, 24, 23};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "AMBIGUOUS";
        RailroadSeatNumeration theObject;
        eq(4, theObject.getInternational(tickets), expected);
    }
    {
        int tickets_array[] = {8, 28};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "26 76";
        RailroadSeatNumeration theObject;
        eq(5, theObject.getInternational(tickets), expected);
    }
    {
        int tickets_array[] = {31};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "91 31";
        RailroadSeatNumeration theObject;
        eq(6, theObject.getInternational(tickets), expected);
    }

    return 0;
}
// END CUT HERE
