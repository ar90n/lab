#include <bits/stdc++.h>
using namespace std;

const long long MOD = 1000000007;

auto solve(long long N, long long M){
    long long minor = min(N, M);
    long long major = max(N, M);

    long long p = 1;
    for(long long i = 1; i <= minor; i++) {
        p = (p * i) % MOD;
    }

    if(N == M) {
        return (2 * p * p) % MOD;
    }
    else if(abs(N - M) == 1) {
        return (p * ((p * major) % MOD)) % MOD;
    }

    return 0LL;
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long M;
    scanf("%lld",&M);
    auto result = solve(N, M);
    cout << result << endl;
    return 0;
}
