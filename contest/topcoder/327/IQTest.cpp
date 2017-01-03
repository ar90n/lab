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

class IQTest
{
    public:
        string nextNumber ( vector<int> previous )
        {
            int n = previous.size();

            if ( n == 1 )
            {
                return "AMBIGUITY";
            }
            if ( n == 2 )
            {
                if ( previous[1] == previous[0] )
                {
                    stringstream ss;
                    string res;
                    ss << previous[0];
                    ss >> res;

                    return res;
                }
                else
                {
                    return "AMBIGUITY";
                }
            }

            vector<int> diffs ( n - 1 );
            for ( int i = 0; i < diffs.size(); i++ )
            {
                diffs[ i ] = previous[ i + 1 ] - previous[ i ];
            }

            vector<int> mods ( n - 2 );
            vector<int> as ( n - 2 );
            vector<int> bs ( n - 2 );
            for ( int i = 0; i < mods.size(); i++ )
            {
                if ( diffs[i] == 0 )
                {
                    if ( 1 < i  )
                    {
                        return "BUG";
                    }

                    for ( int k = i + 1; k < previous.size(); k++ )
                    {
                        if ( previous[k] != previous[k - 1] )
                        {
                            return "BUG";
                        }
                    }

                    stringstream ss;
                    string res;
                    ss << previous[i];
                    ss >> res;
                    return res;
                }
                else
                {
                    mods[i ] = diffs[ i + 1 ] % diffs[ i ];
                    as[i] = diffs[i + 1 ] / diffs[i];
                    if ( mods[i] != 0 )
                    {
                        return "BUG";
                    }
                }

            }

            for ( int i = 1; i < as.size(); i++ )
            {
                if ( as[i] != as[i - 1] )
                {
                    return "BUG";
                }
            }


            int b = previous[1] - as[0] * previous[0];

            string res;
            stringstream ss;
            ss << ( as[0] * previous[ n - 1 ] + b );
            ss >> res;
            return res;
        }

};



// BEGIN CUT HERE
#define ARRSIZE(x) (sizeof(x) / sizeof(x[0]))

template<typename T> void print ( T a )
{
    cerr << a;
}

static void print ( long long a )
{
    cerr << a << "L";
}

static void print ( string a )
{
    cerr << '"' << a << '"';
}

template <typename T> void print ( vector <T> a )
{
    cerr << "{";
    for ( int i = 0; i != a.size(); i++ )
    {
        if ( i != 0 )
        {
            cerr << ", ";
        }
        print ( a[i] );
    }
    cerr << "}";
}

template <typename T> void printerror ( T have, T need )
{
    cerr << "\tExpected: ";
    print ( need );
    cerr << endl;
    cerr << "\tReceived: ";
    print ( have );
    cerr << endl;
}

template <typename T> void eq ( int n, T have, T need )
{
    if ( have == need )
    {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    }
    else
    {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror ( have, need );
    }
}

template <typename T> void eq ( int n, vector <T> have, vector <T> need )
{
    if ( have.size() != need.size() )
    {
        cerr << "Test Case #" << n << "...FAILED: ";
        cerr << "returned " << have.size() << " elements; expected " << need.size() << " elements." << endl;
        printerror ( have, need );
        return;
    }

    for ( int i = 0; i < have.size(); i++ )
    {
        if ( have[i] != need[i] )
        {
            cerr << "Test Case #" << n << "...FAILED: ";
            cerr << "expected and returned array differ in position " << i << "." << endl;
            printerror ( have, need );
            return;
        }
    }

    cerr << "Test Case #" << n << "...PASSED" << endl;
}

static void eq ( int n, double have, double need )
{
    if ( fabs ( have - need ) < 1e-9 ||
         ( fabs ( need ) >= 1 && fabs ( ( have - need ) / need ) < 1e-9 ) )
    {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    }
    else
    {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror ( have, need );
    }
}

static void eq ( int n, string have, string need )
{
    if ( have == need )
    {
        cerr << "Test Case #" << n << "...PASSED" << endl;
    }
    else
    {
        cerr << "Test Case #" << n << "...FAILED" << endl;
        printerror ( have, need );
    }
}

int main ( int argc, char *argv[] )
{
    {
        int previous_array[] = {1, 2, 3, 4, 5};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "6";
        IQTest theObject;
        eq ( 0, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = {3, 6, 12, 24, 48};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "96";
        IQTest theObject;
        eq ( 1, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = {1, 4, 13, 40};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "121";
        IQTest theObject;
        eq ( 2, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = {0};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "AMBIGUITY";
        IQTest theObject;
        eq ( 3, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = { -1, 2};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "AMBIGUITY";
        IQTest theObject;
        eq ( 4, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = {57, 57};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "57";
        IQTest theObject;
        eq ( 5, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = {16, -8, 4, -2};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "BUG";
        IQTest theObject;
        eq ( 6, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = {6, 5, 4, 3, 1};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "BUG";
        IQTest theObject;
        eq ( 7, theObject.nextNumber ( previous ), expected );
    }
    {
        int previous_array[] = { -12, 12, -36, 60};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "-132";
        IQTest theObject;
        eq ( 8, theObject.nextNumber ( previous ), expected );
    }

    {
        int previous_array[] = { 57, -42, -42, -42};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "-42";
        IQTest theObject;
        eq ( 8, theObject.nextNumber ( previous ), expected );
    }

    {
        int previous_array[] = {2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} ;
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "BUG";
        IQTest theObject;
        eq ( 8, theObject.nextNumber ( previous ), expected );
    }

    {
        int previous_array[] = {57, -42, -42, -41};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "BUG";
        IQTest theObject;
        eq ( 8, theObject.nextNumber ( previous ), expected );
    }

    {
        int previous_array[] = {1, 2, 3, 5};
        vector<int> previous ( previous_array, previous_array + ARRSIZE ( previous_array ) );
        string expected = "BUG";
        IQTest theObject;
        eq ( 8, theObject.nextNumber ( previous ), expected );
    }


    return 0;
}
// END CUT HERE
