#include <cassert>
#include <vector>
#include <queue>
#include <limits>
using namespace std;

// Prototype (implement in your bfs.cpp)
vector<int> bfs_shortest(const vector<vector<int>>& g, int s)
{
    std::vector<int> ret(g.size(), std::numeric_limits<int>::max());
    std::queue<std::pair<int, int>> q;

    q.push(std::make_pair(s, 0));
    while(!q.empty())
    {
        auto const cur = q.front();
        int const cur_ind = cur.first;
        int const cur_acc = cur.second;
        q.pop();
        if(cur_ind < 0 || g.size() <= cur_ind || ret[cur_ind] < cur_acc)
        {
            continue;
        }
        ret[cur_ind] = cur_acc;

        for(int neigh_ind: g[cur_ind])
        {
            q.push(std::make_pair(neigh_ind, cur_acc + 1));
        }
    }

    return ret;
}

int main(){
    vector<vector<int>> g(5);
    g[0] = {1,2};
    g[1] = {0,3};
    g[2] = {0,3};
    g[3] = {1,2,4};
    g[4] = {3};

    auto d = bfs_shortest(g, 0);
    assert((d == vector<int>({0,1,1,2,3})));
    return 0;
}
