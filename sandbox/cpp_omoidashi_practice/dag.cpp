#include <cassert>
#include <vector>
#include <optional>
#include <iostream>
#include <algorithm>

using namespace std;

// Prototype (implement in your dag.cpp)
optional<vector<int>> topo_sort(const vector<vector<int>>& g)
{
    std::vector<int> ret;
    std::vector<int> visited(g.size(), 0);

    for(int i = 0; i < g.size(); i++)
    {
        if(visited[i] != 0)
        {
            continue;
        }

        std::vector<int> st{i};
        while(!st.empty())
        {
            int const cur = st.back(); st.pop_back();
            if(visited[cur] == 0)
            {
                st.push_back(cur);
                visited[cur]++;

                for(int const &child: g[cur])
                {
                    if(visited[child] == 1)
                    {
                        return std::nullopt;
                    }
                    st.push_back(child);
                }
            }
            else if(visited[cur] == 1)
            {
                ret.push_back(cur);
                visited[cur]++;
            }
        }
    }

    std::reverse(std::begin(ret), std::end(ret));
    return ret;
}

int main(){
    vector<vector<int>> g(5);
    g[0] = {1,2};
    g[1] = {3};
    g[2] = {3};
    g[3] = {4};

    auto ord = topo_sort(g);
    assert(ord.has_value());
    vector<int> pos(5);
    for (int i=0;i<5;++i) pos[(*ord)[i]] = i;
    for (int u=0; u<5; ++u) for (int v: g[u]) assert(pos[u] < pos[v]);

    g[4].push_back(1); // make a cycle
    auto ord2 = topo_sort(g);
    assert(!ord2.has_value());
    return 0;
}

