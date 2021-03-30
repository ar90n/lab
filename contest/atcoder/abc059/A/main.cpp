#include <bits/stdc++.h>
using namespace std;


auto solve(std::vector<std::string> s){
    stringstream ss;
    ss << static_cast<char>(toupper(s[0][0])) << static_cast<char>(toupper(s[1][0])) << static_cast<char>(toupper(s[2][0]));
    return ss.str();
}

int main(){
    std::vector<std::string> s(3);
    for(int i = 0 ; i < 3 ; i++){
        std::cin >> s[i];
    }
    auto result = solve(std::move(s));
    cout << result << endl;
    return 0;
}
