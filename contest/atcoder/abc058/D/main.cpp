#include <bits/stdc++.h>
using namespace std;

const long long MOD = 1000000007;

auto solve(long long n, long long m, std::vector<long long> x, std::vector<long long> y){

}

int main(){
    long long n;
    scanf("%lld",&n);
    long long m;
    scanf("%lld",&m);
    std::vector<long long> x(n);
    for(int i = 0 ; i < n ; i++){
        scanf("%lld",&x[i]);
    }
    std::vector<long long> y(m);
    for(int i = 0 ; i < m ; i++){
        scanf("%lld",&y[i]);
    }
    auto result = solve(n, m, std::move(x), std::move(y));
    cout << result << endl;
    return 0;
}
