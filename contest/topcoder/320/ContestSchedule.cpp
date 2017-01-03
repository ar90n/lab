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

class ContestSchedule {
public:
double expectedWinnings(vector<string> contests)
{
    int n = contests.size();
    set<int> time;

    for( int i = 0; i < n; i++ )
    {
        int s, t, p;
        sscanf( contests[i].c_str(),"%d %d %d", &s, &t, &p );

        time.insert( s );
        time.insert( t );
    }

    vector< int > dp( time.size() + 1 );
    multimap< int, pair< int, int > > schedule;
    map< int , int > table;
    set<int>::iterator it = time.begin();
    int count = 0;
    while( it != time.end() )
    {
        table[ *it ] = count++;
        it++;
    }

    for( int i = 0; i < contests.size() ;i++ )
    {
        int s, t, p;
        sscanf( contests[i].c_str(),"%d %d %d", &s, &t, &p );

        s = table[ s ];
        t = table[ t ];

        schedule.insert( map< int, pair<int, int> >::value_type( s, pair<int, int>( t, p ) ) );
    }

    fill( dp.begin(), dp.end(), 0 );
    for( int i = dp.size() - 2; i >= 0; i-- )
    {
        dp[i] = dp[i + 1];

        if( schedule.find( i ) != schedule.end() )
        {
            pair< multimap< int, pair<int, int> >::iterator, multimap< int, pair<int, int> >::iterator > it_range;
            it_range = schedule.equal_range( i );
            for( multimap< int, pair<int, int> >::iterator it = it_range.first; it != it_range.second; it++ )
            {
                dp[ i ] = max( dp[i], dp[ it->second.first ] + it->second.second );
            }
        }
    }

    return (double)dp[0] / 100.0;
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
        string contests_array[] = {"1 10 100", "10 20 100", "20 30 100", "30 40 100"};
        vector<string> contests(contests_array, contests_array + ARRSIZE(contests_array));
        double expected = 4.0;
        ContestSchedule theObject;
        eq(0, theObject.expectedWinnings(contests), expected);
    }
    {
        string contests_array[] = {"10 20 20", "30 40 60", "15 35 90"};
        vector<string> contests(contests_array, contests_array + ARRSIZE(contests_array));
        double expected = 0.9;
        ContestSchedule theObject;
        eq(1, theObject.expectedWinnings(contests), expected);
    }
    {
        string contests_array[] = {"1 100 85", "99 102 100", "101 200 60"};
        vector<string> contests(contests_array, contests_array + ARRSIZE(contests_array));
        double expected = 1.45;
        ContestSchedule theObject;
        eq(2, theObject.expectedWinnings(contests), expected);
    }
    {
        string contests_array[] = {"1 1000000000 1"};
        vector<string> contests(contests_array, contests_array + ARRSIZE(contests_array));
        double expected = 0.01;
        ContestSchedule theObject;
        eq(3, theObject.expectedWinnings(contests), expected);
    }
    {
        string contests_array[] = {"1361955 8940967 10", "628145516 644285978 17", "883515280 910484865 36", "762888635 769291174 52", "98152102 136328674 1", "9580638 20450682 50", "646139319 664648341 20", "484027666 505290970 57", "841082555 879295329 99", "940563715 966953344 4", "579113528 595261527 25", "925801172 962952912 9", "285845005 307916055 45", "403573723 410697485 10", "9467290 25698952 90", "631265400 650639733 3", "520690249 559261759 96", "491747709 531061081 86", "643215695 672420161 94", "614608448 617341712 44", "456517316 491770747 24", "806956091 828414045 88", "528156706 559510719 15", "158398859 190439269 4", "743974602 761975244 49", "882264704 887725893 1", "877444309 884479317 59", "785986538 806192822 19", "732553407 747696021 81", "132099860 148305810 97", "555144412 572785730 99", "506507263 535927950 82", "489726669 505160939 90", "793692316 804153358 99", "901329638 919179990 10", "29523791 44318662 58", "570497239 595701008 73", "706091347 730328348 23", "118054686 135301010 39", "307178252 337804001 93", "896162463 900667971 39", "271356542 273095245 80", "865692962 891795950 91", "593986003 596160000 58", "807688147 831190344 59", "468916697 496462595 92"};
        vector<string> contests(contests_array, contests_array + ARRSIZE(contests_array));
        double expected = 14.12;
        ContestSchedule theObject;
        eq(4, theObject.expectedWinnings(contests), expected);
    }

    return 0;
}
// END CUT HERE
