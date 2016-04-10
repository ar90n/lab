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

void convert_international( vector< int > &tickets )
{
    int n = tickets.size();
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

int check_domestic( int tickets )
{
    if( 36 < tickets )
    {
        return 2;
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
        return 8;
    }

    if( ( a != 1 ) &&
        ( a != 3 ) &&
        ( a != 4 ) &&
        ( a != 6 ) )
    {
        return 8;
    }

    if( ( 0 < b ) && ( b <= 3 ) )
    {
        return 0;
    }

    return 4;
}

string getInternational(vector<int> tickets)
{
    int n = tickets.size();
    int type;
    int illegal1,illegal2;

    int state = 0;
    for( int i = 0; i < n; i++ )
    {
        state |= check_domestic( tickets[ i] ) ;
        state |= check_international( tickets[ i] ) ;
    }

    switch( state )
    {
        case 0:
            return "AMBIGUOUS";
            break;
        case 1:
        case 8:
        case 9:
            convert_international( tickets );
            break;
        case 2:
        case 4:
        case 6:
            break;
        case 3:
        case 5:
        case 7:
        case 10:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
            return "BAD DATA";
            break;
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
        int tickets_array[] = {2,92};
        vector<int> tickets(tickets_array, tickets_array + ARRSIZE(tickets_array));
        string expected = "BAD DATA";
        RailroadSeatNumeration theObject;
        eq(6, theObject.getInternational(tickets), expected);
    }

    return 0;
}
// END CUT HERE
