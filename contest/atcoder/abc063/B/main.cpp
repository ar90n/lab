#include <bits/stdc++.h>
using namespace std;

const string YES = "yes";
const string NO = "no";

auto solve(std::string S){
    return set<char>(begin(S), end(S)).size() == S.size() ? YES : NO;
}

int main(){
    std::string S;
    std::cin >> S;
    auto result = solve(S);
    cout << result << endl;
    return 0;
}
