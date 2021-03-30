#include <bits/stdc++.h>
using namespace std;

const long long MOD = 1000000007LL;

auto solve(long long N){
    long long ret = 1LL;
    for(long long i = 1LL;i <= N; ++i) {
        ret = (ret * i) % MOD;
    }

    return ret;
}

int main(){
    long long N;
    scanf("%lld",&N);
    auto result = solve(N);
    cout << result << endl;
    return 0;
}
