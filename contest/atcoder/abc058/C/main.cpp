#include <bits/stdc++.h>
using namespace std;


auto solve(long long n, std::vector<std::string> S){

}

int main(){
    long long n;
    scanf("%lld",&n);
    std::vector<std::string> S(n);
    for(int i = 0 ; i < n ; i++){
        std::cin >> S[i];
    }
    auto result = solve(n, std::move(S));
    cout << result << endl;
    return 0;
}
