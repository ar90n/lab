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

class LightsCube {
public:
vector<int> count(int N, vector<string> lights)
{
	int n = lights.size();
	vector< unsigned int > pos( n );
	vector< int > res( n );

	for( int i = 0; i < n;i++ )
	{
		int  x;
		int  y;
		int  z;

		stringstream ss;
		ss << lights[i];

		ss >> x;
		ss >> y;
		ss >> z;

		pos[i] = ( ( x << 16 ) | ( y << 8 ) | z );
		res[i] = 0;
	}

	for( int i = 0; i < N; i++ )
	{
		for( int j = 0; j < N; j ++ )
		{
			for( int k = 0; k < N; k++ )
			{
				int min_index = 0;
				int min_distance = 1 << 29;
				for( int l = 0; l < n;l++ )
				{
					int dif_x;
					int dif_y;
					int dif_z;
					int pos_x;
					int pos_y;
					int pos_z;
					int distance;

					pos_x = ( pos[l] >> 16 ) & 0x000000ff;
					pos_y = ( pos[l] >> 8 ) & 0x000000ff;
					pos_z = ( pos[l] >> 0 ) & 0x000000ff;

					dif_x = abs( pos_x - i );
					dif_y = abs( pos_y - j );
					dif_z = abs( pos_z - k );
					distance = dif_x + dif_y + dif_z;

					if( distance < min_distance )
					{
						min_distance = distance;
						min_index = l;
					}
				}
				res[ min_index ]++;
			}
		}
	}

	return res;
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
        int N = 2;
        string lights_array[] = {"0 0 0", "0 0 1", "0 1 0", "0 1 1", "1 0 0", "1 0 1", "1 1 0", "1 1 1"};
        vector<string> lights(lights_array, lights_array + ARRSIZE(lights_array));
        int expected_array[] = {1, 1, 1, 1, 1, 1, 1, 1 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        LightsCube theObject;
        eq(0, theObject.count(N, lights), expected);
    }
    {
        int N = 3;
        string lights_array[] = {"1 1 1"};
        vector<string> lights(lights_array, lights_array + ARRSIZE(lights_array));
        int expected_array[] = {27 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        LightsCube theObject;
        eq(1, theObject.count(N, lights), expected);
    }
    {
        int N = 4;
        string lights_array[] = {"0 0 0", "3 3 3"};
        vector<string> lights(lights_array, lights_array + ARRSIZE(lights_array));
        int expected_array[] = {32, 32 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        LightsCube theObject;
        eq(2, theObject.count(N, lights), expected);
    }
    {
        int N = 5;
        string lights_array[] = {"0 2 4", "2 0 0", "3 4 4", "4 1 2"};
        vector<string> lights(lights_array, lights_array + ARRSIZE(lights_array));
        int expected_array[] = {38, 28, 32, 27 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        LightsCube theObject;
        eq(3, theObject.count(N, lights), expected);
    }
    {
        int N = 12;
        string lights_array[] = {"4 7 6", "8 0 0", "3 2 3", "7 7 2", "7 5 1", "10 11 5", "4 9 7", "6 1 0", "10 1 1", "9 7 11", "11 3 11", "9 0 3", "10 2 0"};
        vector<string> lights(lights_array, lights_array + ARRSIZE(lights_array));
        int expected_array[] = {264, 18, 303, 236, 105, 124, 216, 44, 53, 146, 126, 80, 13 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        LightsCube theObject;
        eq(4, theObject.count(N, lights), expected);
    }
    {
        int N = 40;
        string lights_array[] = {"29 13 9", "4 10 34", "11 26 16", "2 33 22", "27 31 14", "36 20 8"};
        vector<string> lights(lights_array, lights_array + ARRSIZE(lights_array));
        int expected_array[] = {14657, 12834, 12364, 5902, 12678, 5565 };
        vector<int> expected(expected_array, expected_array + ARRSIZE(expected_array));
        LightsCube theObject;
        eq(5, theObject.count(N, lights), expected);
    }

    return 0;
}
// END CUT HERE
