#include <bits/stdc++.h>
using namespace std;


auto solve(std::string s){
    stringstream ss;
    ss << s[0] << (s.size() - 2) << s[s.size() - 1];
    return ss.str();
}

int main(){
    std::string s;
    std::cin >> s;
    auto result = solve(s);
    cout << result << endl;
    return 0;
}
