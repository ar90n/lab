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

class BusTrip {
public:
int returnTime(int N, vector<string> buses)
{
	int n = buses.size();
	vector< vector<int> > vbuses;

	for( int i = 0; i < n; i++ )
	{
		stringstream ss;
		ss << buses[i];
		vector< int > vtmp;

		while( ss )
		{
			string stmp;
			ss >> stmp;
			vtmp.push_back( atoi( stmp.c_str() ) );
		}

		for( int i = 0; i < vtmp.size(); i ++ )
		{
			cout << vtmp[i] << endl;
		}
		vbuses.push_back( vtmp );
	}

	for( int i = 0; i < vbuses.size(); i++ )
	{
		for( int j = 0; j < vbuses[i].size();j++)
		{
			cout << vbuses[i][j] << 'a';
		}
		cout << endl;
	}

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
        int N = 3;
        string buses_array[] = {"0 1 2"};
        vector<string> buses(buses_array, buses_array + ARRSIZE(buses_array));
        int expected = 12;
        BusTrip theObject;
        eq(0, theObject.returnTime(N, buses), expected);
    }
    {
        int N = 51;
        string buses_array[] = {"0 5 10 15 20 25 30 35 40 50"};
        vector<string> buses(buses_array, buses_array + ARRSIZE(buses_array));
        int expected = 1000;
        BusTrip theObject;
        eq(1, theObject.returnTime(N, buses), expected);
    }
    {
        int N = 3;
        string buses_array[] = {"0 1 2", "2 1 0"};
        vector<string> buses(buses_array, buses_array + ARRSIZE(buses_array));
        int expected = -1;
        BusTrip theObject;
        eq(2, theObject.returnTime(N, buses), expected);
    }
    {
        int N = 5;
        string buses_array[] = {"0 1 2 3 4", "3 1 2 0", "4 1 2 3 0", "1 2 0 3 4", "4 0"};
        vector<string> buses(buses_array, buses_array + ARRSIZE(buses_array));
        int expected = 12;
        BusTrip theObject;
        eq(3, theObject.returnTime(N, buses), expected);
    }
    {
        int N = 25;
        string buses_array[] = {"24 14 9 7 2", "21 4 18 24 7 1 2 11 8 9 14 16 5 17 13 23 19 15 22", "12 22 24 9 1 5 10 8 7 18 16 19 4 13 17", "14 5 17 9 23 7 16 22 10 4 6", "19 8 1 9 24 3 5 22 16 7 6 4 10 23 17 0 13 15", "2 16 10 13 14 1 11 20 0 24 22 23 19 4 18", "19 15 18 0", "15 9 22 5 20 8 23 14 24 18 21 6 13 19", "2 6 19 3 21 10 20 22 24 13 16 15 8 18 17 14 5", "19 10 1 7 5 11 21 8 14 0 17 23 12 2 3 16"};
        vector<string> buses(buses_array, buses_array + ARRSIZE(buses_array));
        int expected = 157;
        BusTrip theObject;
        eq(4, theObject.returnTime(N, buses), expected);
    }
    {
        int N = 100;
        string buses_array[] = {"0 10 30 45 60 46 39 31 20", "9 20 0 86"};
        vector<string> buses(buses_array, buses_array + ARRSIZE(buses_array));
        int expected = -1;
        BusTrip theObject;
        eq(5, theObject.returnTime(N, buses), expected);
    }

    return 0;
}
// END CUT HERE
