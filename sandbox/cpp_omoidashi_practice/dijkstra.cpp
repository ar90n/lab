#include <cassert>
#include <vector>
#include <utility>
#include <limits>
#include <queue>
using namespace std;

vector<long long> dijkstra(const vector<vector<pair<int,long long>>>& g, int s)
{
    std::vector<long long> ret(g.size(), std::numeric_limits<long long>::max());
    std::priority_queue<std::pair<int, long long>> queue;

    queue.push(std::make_pair(s, 0));
    while(!queue.empty())
    {
        auto const cur = queue.top();
        queue.pop();

        int const cur_index = cur.first;
        int const cur_acc_cost = cur.second;
        if (cur_index < 0 || ret.size() <= cur_index || ret[cur_index] <= cur_acc_cost)
        {
            continue;
        }
        ret[cur_index] = cur_acc_cost;

        for(auto const& neighbor: g[cur_index])
        {
            queue.push(std::make_pair(neighbor.first, cur_acc_cost + neighbor.second));
        }
    }
   
    return ret;
}

int main(){
    vector<vector<pair<int,long long>>> g(4);
    g[0] = {{1,2},{2,5}};
    g[1] = {{2,1},{3,3}};
    g[2] = {{3,1}};

    auto dist = dijkstra(g, 0);
    assert((dist == vector<long long>({0,2,3,4})));
    return 0;
}
