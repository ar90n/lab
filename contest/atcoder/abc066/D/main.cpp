#include <bits/stdc++.h>
using namespace std;

const long long MOD = 1000000007;

auto solve(long long n, std::vector<long long> a){

}

int main(){
    long long n;
    scanf("%lld",&n);
    std::vector<long long> a(n+1);
    for(int i = 0 ; i < n+1 ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(n, std::move(a));
    cout << result << endl;
    return 0;
}
