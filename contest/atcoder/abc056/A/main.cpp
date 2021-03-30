#include <bits/stdc++.h>
using namespace std;


auto solve(std::string a, std::string b){
    return ((a == "D") ^ (b == "H")) ? "H" : "D";
}

int main(){
    std::string a;
    std::cin >> a;
    std::string b;
    std::cin >> b;
    auto result = solve(a, b);
    cout << result << endl;
    return 0;
}
