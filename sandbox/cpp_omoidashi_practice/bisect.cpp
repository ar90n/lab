#include <cassert>
#include <vector>
#include <cstddef>
using namespace std;

#include <iostream>
size_t bisect_left (const vector<int>& a, int x)
{
    int left = -1;
    int right = a.size();
    while(1 < (right - left))
    {
        size_t const mid = left + (right - left) / 2;
        if(x <= a[mid]) {
            right = mid;
        }
        else {
            left = mid;
        }
    }

    return right;
}

size_t bisect_right(const vector<int>& a, int x)
{
    size_t left = -1;
    size_t right = a.size();
    while(1 < (right - left))
    {
        size_t const mid = left + (right - left) / 2;
        if(a[mid] <= x) {
            left = mid;
        }
        else {
            right = mid;
        }
    }

    return right;
}

bool   binary_search_exist(const vector<int>& a, int x)
{
    size_t pos = bisect_left(a, x);
    return a[pos] == x;
}

int main(){
    vector<int> a{1,2,2,2,3,5};
    assert(bisect_left(a, 2) == 1);
    assert(bisect_right(a, 2) == 4);
    assert(bisect_left(a, 4) == 5);
    assert(bisect_right(a, 5) == 6);
    assert(binary_search_exist(a, 3));
    assert(!binary_search_exist(a, 4));

    vector<int> b{};
    assert(bisect_left(b, 10) == 0);
    assert(bisect_right(b, 10) == 0);
    return 0;
}

