#include <cassert>
#include <vector>
using namespace std;

#include<iostream>

vector<int> preorder_traversal (const vector<int>& val, const vector<int>& L, const vector<int>& R, int root)
{
    std::vector<int> ret;
    std::vector<int> visited(val.size(), 0);
    std::vector<int> st;

    st.push_back(root);
    while(!st.empty())
    {
        int const cur_ind = st.back(); st.pop_back();
        if (visited[cur_ind] == 0)
        {
            visited[cur_ind] = 1;

            if(R[cur_ind] != -1)
            {
                st.push_back(R[cur_ind]);
            }
            if(L[cur_ind] != -1)
            {
                st.push_back(L[cur_ind]);
            }
            st.push_back(cur_ind);
        }
        else if(visited[cur_ind] == 1)
        {
            ret.push_back(val[cur_ind]);
        }
    }

    return ret;
}

vector<int> inorder_traversal  (const vector<int>& val, const vector<int>& L, const vector<int>& R, int root)
{
    std::vector<int> ret;
    std::vector<int> visited(val.size(), 0);
    std::vector<int> st;

    st.push_back(root);
    while(!st.empty())
    {
        int const ind = st.back(); st.pop_back();
        if(visited[ind] == 0)
        {
            visited[ind] = 1;

            if(R[ind] != -1)
            {
                st.push_back(R[ind]);
            }
            st.push_back(ind);
            if(L[ind] != -1)
            {
                st.push_back(L[ind]);
            }
        }
        else if(visited[ind] == 1)
        {
            ret.push_back(val[ind]);
        }
    }


    return ret;
}

vector<int> postorder_traversal(const vector<int>& val, const vector<int>& L, const vector<int>& R, int root)
{
    std::vector<int> ret;
    std::vector<int> visited(val.size(), 0);
    std::vector<int> st;

    st.push_back(root);
    while(!st.empty())
    {
        int const ind = st.back(); st.pop_back();
        if(visited[ind] == 0)
        {
            visited[ind] = 1;

            st.push_back(ind);
            if(R[ind] != -1)
            {
                st.push_back(R[ind]);
            }
            if(L[ind] != -1)
            {
                st.push_back(L[ind]);
            }
        }
        else if(visited[ind] == 1)
        {
            ret.push_back(val[ind]);
        }
    }


    return ret;
}

int main(){
    // Perfect BST values 1..7 as an index-based tree.
    // index: 0 1 2 3 4 5 6
    // val:   4 2 6 1 3 5 7
    vector<int> val{4,2,6,1,3,5,7};
    vector<int> L  {1,3,5,-1,-1,-1,-1};
    vector<int> R  {2,4,6,-1,-1,-1,-1};
    int root = 0;

    auto pre  = preorder_traversal (val,L,R,root);
    auto in   = inorder_traversal  (val,L,R,root);
    auto post = postorder_traversal(val,L,R,root);

    assert((pre  == vector<int>({4,2,1,3,6,5,7})));
    assert((in   == vector<int>({1,2,3,4,5,6,7})));
    assert((post == vector<int>({1,3,2,5,7,6,4})));
    return 0;
}

