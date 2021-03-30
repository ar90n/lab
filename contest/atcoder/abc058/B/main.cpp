#include <bits/stdc++.h>
using namespace std;


auto solve(std::string O, std::string E){
    stringstream ss;
    for(int i = 0; i < E.size(); ++i) {
        ss << O[i] << E[i];
    }
    if(E.size() < O.size()) {
        ss << O[O.size()-1];
    }
    return ss.str();
}

int main(){
    std::string O;
    std::cin >> O;
    std::string E;
    std::cin >> E;
    auto result = solve(O, E);
    cout << result << endl;
    return 0;
}
