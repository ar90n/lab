#include <cassert>
#include <vector>
#include <numeric>
#include <iostream>
using namespace std;

// Forward declaration only (you define full class in dsu.cpp)
class DSU {
public:
    explicit DSU(int n)
        : parents(n)
        , sizes(n, 1)
    {
        std::iota(std::begin(parents), std::end(parents), 0);
    }

    bool unite(int a, int b)
    {
        if(size(b) < size(a))
        {
            std::swap(a, b);
        }

        int root_a = root(a);
        int root_b = root(b);
        if(root_a == root_b)
        {
            return false;
        }

        parents[root_b] = root_a;
        sizes[root_a] += sizes[root_b];
        return true;
    }
    bool same (int a, int b)
    {
        return root(a) == root(b);
    }
    int  size (int a)
    {
        return sizes[a];
    }

private:
    int root(int n)
    {
        int ret = n;
        std::vector<int> ns{n};
        while(ret != parents[ret])
        {
            ret = parents[ret];
            ns.push_back(ret);
        }
        for(auto const &nn: ns)
        {
            parents[nn] = ret;
        }
        return ret;
    }

    std::vector<int> parents;
    std::vector<int> sizes;
};

int main(){
    DSU dsu(6);
    dsu.unite(0,1);
    dsu.unite(2,3);
    dsu.unite(1,2); // {0,1,2,3}
    assert(dsu.same(0,3));
    assert(!dsu.same(0,4));

    dsu.unite(4,5);
    assert(dsu.size(0) == 4);
    assert(dsu.size(4) == 2);
    return 0;
}

