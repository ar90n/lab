#include <cassert>
#include <vector>
#include <set>
#include <iostream>
#include <string>
using namespace std;

// Prototypes (implement in your sliding_window.cpp)
size_t    longest_unique_substr(const string& s)
{
    std::set<char> acc;
    int ret = 0;
    int head = 0;
    int tail = 0;
    while(tail < s.size())
    {
        ret = std::max(ret, head - tail);
        if(head < s.size() && acc.count(s[head]) == 0)
        {
            acc.insert(s[head]);
            head++;
        }
        else
        {
            acc.erase(s[tail]);
            tail++;
        }
    }

    return ret;
}

long long max_window_sum(const vector<int>& a, int k)
{
    long long ret = 0;
    long long acc = 0;
    for(int i = 0; i < a.size(); i++)
    {
        acc += a[i];
        if(k <= i)
        {
            acc -= a[i - k];
        }
        if((k-1) <= i)
        {
            ret = std::max(ret, acc);
        }
    }
    return ret;
}

int main(){
    string s = "abcaabcdbb"; // longest unique substring = "abcd" -> 4
    assert(longest_unique_substr(s) == 4);

    vector<int> a{1,2,3,4,5};
    assert(max_window_sum(a, 2) == 9);
    assert(max_window_sum(a, 5) == 15);
    assert(max_window_sum(a, 1) == 5);
    return 0;
}

