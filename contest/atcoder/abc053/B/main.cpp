#include <bits/stdc++.h>
using namespace std;


auto solve(std::string s){
    auto bi = find(begin(s), end(s), 'A');
    auto ei = find(rbegin(s), rend(s), 'Z');
    return s.size() - distance(begin(s), bi) - distance(rbegin(s), ei);
}

int main(){
    std::string s;
    std::cin >> s;
    auto result = solve(s);
    cout << result << endl;
    return 0;
}
