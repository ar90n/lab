#include <cassert>
#include <vector>
#include <stack>
using namespace std;

// Prototype (implement in your dfs.cpp)
vector<int> dfs_preorder_all(const vector<vector<int>>& g)
{
    std::vector<int> ret;
    std::vector<bool> visited(g.size(), false);

    std::stack<int> st;
    for(int i = g.size() - 1; 0 <= i; i--)
    {
        st.push(i);
    }

    while(!st.empty())
    {
        int const cur_ind = st.top();
        st.pop();

        if (cur_ind < 0 || g.size() <= cur_ind || visited[cur_ind])
        {
            continue;
        }
        ret.push_back(cur_ind);
        visited[cur_ind] = true;

        for(int const children: g[cur_ind])
        {
            st.push(children);
        }
    }

    return ret;
}

int main(){
    vector<vector<int>> g(6);
    g[0] = {1,2};
    g[1] = {3};
    g[2] = {};
    g[3] = {4};
    g[4] = {};
    g[5] = {}; // isolated node

    auto order = dfs_preorder_all(g);
    assert(order.size() == 6);
    vector<int> pos(6, -1);
    for (int i=0;i<6;++i) pos[order[i]] = i;
    assert(pos[0] < pos[1] && pos[0] < pos[2]);
    assert(pos[1] < pos[3]);
    assert(pos[3] < pos[4]);
    assert(pos[5] >= 0);
    return 0;
}

