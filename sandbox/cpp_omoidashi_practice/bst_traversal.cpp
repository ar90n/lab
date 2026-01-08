#include <cassert>
#include <vector>
using namespace std;

#include<iostream>

vector<int> preorder_traversal (const vector<int>& val, const vector<int>& L, const vector<int>& R, int root)
{
    std::vector<int> ret;
    std::vector<bool> visited(val.size(), false);
    auto impl = [&](auto&& self, int ind) -> void {
        if(visited[ind]) {
            return;
        }
        visited[ind] = true;

        ret.push_back(val[ind]);
        if(auto const l_ind = L[ind]; l_ind != -1)
        {
            self(self, l_ind);
        }
        if(auto const r_ind = R[ind]; r_ind != -1)
        {
            self(self, r_ind);
        }
    };
    impl(impl, root);

    return ret;
}

vector<int> inorder_traversal  (const vector<int>& val, const vector<int>& L, const vector<int>& R, int root)
{
    std::vector<int> ret;
    std::vector<bool> visited(val.size(), false);
    auto impl = [&](auto&& self, size_t ind) -> void {
        if(visited.size() <= ind || visited[ind]) {
            return;
        }
        visited[ind] = true;

        if(auto const l_ind = L[ind]; l_ind != -1)
        {
            self(self, l_ind);
        }
        ret.push_back(val[ind]);
        if(auto const r_ind = R[ind]; r_ind != -1)
        {
            self(self, r_ind);
        }
    };
    impl(impl, root);

    return ret;
}

vector<int> postorder_traversal(const vector<int>& val, const vector<int>& L, const vector<int>& R, int root)
{
    std::vector<int> ret;
    std::vector<bool> visited(val.size(), false);
    auto const impl = [&](auto&& self, size_t ind) -> void {
	if(visited.size() <= ind || visited[ind]) {
		return;
	}
	visited[ind] = true;

	if(auto const l_ind = L[ind]; l_ind != -1)
	{
		self(self, l_ind);
	}
	if(auto const r_ind = R[ind]; r_ind != -1)
	{
		self(self, r_ind);
	}
	ret.push_back(val[ind]);
    };
    impl(impl, root);

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

