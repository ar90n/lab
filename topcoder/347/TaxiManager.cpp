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

//refer to s2k's code

class TaxiManager {
public:
int schedule(vector<string> roads, vector<string> customers)
{
    int n = roads.size();
    int m = customers.size();
    int dist[n][n];

    for( int i = 0; i < n; ++i )
    {
        for( int j = 0; j < n; ++j )
        {
           dist[i][j] =  roads[ i ][ j ] - '0';
           if( ( dist[i][j] == 0 ) && ( i != j ) )
           {
               dist[i][j] += 1 << 29;
           }

        }
    }

    pair< int ,int > from_to[ m ];
    for( int i = 0; i < m; ++ i )
    {
        stringstream ss;
        ss << customers[ i ];

        int tmp;
        ss >> tmp;
        from_to[ i ].first = tmp;
        ss >> tmp;
        from_to[ i ].second = tmp;
    }

    for( int k = 0; k < n; ++ k )
    {
        for( int i = 0; i < n; ++ i )
        {
            for( int j = 0; j < n; ++ j )
            {
                dist[i][j] = min( dist[i][j], dist[i][k] + dist[k][j] );
            }
        }
    }

    int dp[ 1 << m ][ n ];//dp[残りのお客さん][出発位置]
    memset( dp, 0x00, ( 1 << m ) * n * sizeof( int ) );

    //お客さんがいないときは帰ってくるだけ
    for( int i = 0; i < n; ++ i )
    {
        dp[ 0 ][ i ] = dist[ i ][ 0 ];
    }

    for( int remain = 1; remain < ( 1 << m ); ++ remain )//お客さんの全パターンをremainで表現
    {
        for( int x = 0; x < n ; ++ x )//出発点を走査
        {
            int c = 1 << 29;
            for( int i = 0; i < m ; ++ i )
            {
                if( ( remain & ( 1 << i ) ) != 0 )//最初にi番目のお客さんを乗せますか？
                {
                    const int from = from_to[ i ].first;
                    const int to = from_to[ i ].second;

                    c = min( c, dist[ x ][ from ] + dist[ from ][ to ] + dp[ remain & ~( 1 << i ) ][ to ] );
                }
            }
            dp[ remain ][ x ] = c;
        }
    }

    int res = 1 << 29 ;
    for( int c1 = 0, c2 = (1 << m ) - 1; c1 < ( 1 << m ); ++c1,--c2 )
    {
        res = min( res, max( dp[c1][0],dp[c2][0] ) );
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
        string roads_array[] = {"020200", "202020", "020002", "200020", "020202", "002020"};
        vector<string> roads(roads_array, roads_array + ARRSIZE(roads_array));
        string customers_array[] = {"5 3", "2 4", "1 5", "3 2"};
        vector<string> customers(customers_array, customers_array + ARRSIZE(customers_array));
        int expected = 16;
        TaxiManager theObject;
        eq(0, theObject.schedule(roads, customers), expected);
    }
    {
        string roads_array[] = {"00020251090265906661", "00763002550100090081", "06003699000080062771", "00000710460400035310", "50000039119198350060", "66060004050810046028", "02333108565000200880", "40212560000209205231", "02601150098329905062", "00210383709951005203", "10111087340780827070", "05065800003095040140", "15604020082000100090", "83430030070580600750", "10588355007006001150", "14400080790005400536", "23400990400933060004", "11053016300602000090", "90040920084059282502", "61300007077904050900"};
        vector<string> roads(roads_array, roads_array + ARRSIZE(roads_array));
        string customers_array[] = {"0 19", "4 16", "15 16", "4 18", "2 7", "9 15", "11 6", "7 13", "19 13", "12 19", "14 12", "16 1"};
        vector<string> customers(customers_array, customers_array + ARRSIZE(customers_array));
        int expected = 33;
        TaxiManager theObject;
        eq(1, theObject.schedule(roads, customers), expected);
    }
    {
        string roads_array[] = {"095222800320504", "107600288090501", "760973530769345", "963093337510830", "338404069255826", "291700050155264", "002783031709004", "404730701707712", "068870030090995", "320025180036103", "468695042801904", "233626561000105", "070014432197086", "887301000143802", "230852749990330"};
        vector<string> roads(roads_array, roads_array + ARRSIZE(roads_array));
        string customers_array[] = {"3 6", "0 4", "2 7", "9 7", "13 9", "1 6", "7 13", "14 2", "8 7", "10 1", "11 13", "7 12"};
        vector<string> customers(customers_array, customers_array + ARRSIZE(customers_array));
        int expected = 28;
        TaxiManager theObject;
        eq(2, theObject.schedule(roads, customers), expected);
    }
    {
        string roads_array[] = {"00401", "50990", "00062", "08008", "03000"};
        vector<string> roads(roads_array, roads_array + ARRSIZE(roads_array));
        string customers_array[] = {"2 4"};
        vector<string> customers(customers_array, customers_array + ARRSIZE(customers_array));
        int expected = 14;
        TaxiManager theObject;
        eq(3, theObject.schedule(roads, customers), expected);
    }

    return 0;
}
// END CUT HERE
