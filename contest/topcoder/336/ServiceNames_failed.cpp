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

class ServiceNames {
public:
vector<string> makeList(vector<string> services)
{
    typedef map< string,vector< string > > Table;
    int n = services.size();
    Table  table;

    for( int i = 0; i < n; i ++ )
    {
        stringstream ss;
        ss << services[i];

        string service;
        ss >> service;

        string koi;
        while( ss  >> koi )
        {
            Table::iterator it = table.find( koi );
            if( it == table.end() )
            {
                vector< string > tmp;
                tmp.push_back( service );
                table.insert( make_pair( koi, tmp ) );
            }
            else
            {
                it->second.push_back( service );
            }
        }
    }

    vector< string > result;
    Table::iterator it = table.begin();
    while( it != table.end() )
    {
        string element = it->first;
        vector< string > kois = it->second;

        element += " ==> " + kois[0];
        for( int i = 1; i < kois.size(); i++ )
        {
            element += ", " + kois[i];
        }
        result.push_back( element );
        it++;
    }

    return result;
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
        string services_array[] = {"BLAST Genome Annotation Sensitivity", "PING", "X Annotation"};
        vector<string> services(services_array, services_array + ARRSIZE(services_array));
        string expected_array[] = {"Annotation ==> BLAST, X", "Genome ==> BLAST", "Sensitivity ==> BLAST"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ServiceNames theObject;
        eq(0, theObject.makeList(services), expected);
    }
    {
        string services_array[] = {"PING"};
        vector<string> services(services_array, services_array + ARRSIZE(services_array));
        string expected_array[] = {};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ServiceNames theObject;
        eq(1, theObject.makeList(services), expected);
    }
    {
        string services_array[] = {"BLAST Genome annotation Sensitivity", "PING", "X Annotation", "Apple X ample"};
        vector<string> services(services_array, services_array + ARRSIZE(services_array));
        string expected_array[] = {"Annotation ==> X", "Genome ==> BLAST", "Sensitivity ==> BLAST", "X ==> Apple", "ample ==> Apple", "annotation ==> BLAST"};
        vector<string> expected(expected_array, expected_array + ARRSIZE(expected_array));
        ServiceNames theObject;
        eq(2, theObject.makeList(services), expected);
    }

    return 0;
}
// END CUT HERE
