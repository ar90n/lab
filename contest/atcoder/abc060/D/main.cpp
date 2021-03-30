#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long W, std::vector<long long> w, std::vector<long long> v){
    auto const w0 = w[0];
    auto const WW = W - w0 * N;
    cout << WW << endl;
    //std::vector< long long > dp(WW + 1, 0LL);
    std::vector< long long > dp(10, 0LL);
//    std::vector< std::vector< long long > > dp(WW + 1, std::vector< long long >(N + 1, 0));

//    for(int i = 0; i < N; i++) {
//        auto const cw = w[i] - w0;
//        auto const cv = v[i];
//        for(int j = 0; j <= WW; j++) {
//            dp[i+1][j] = max(dp[i+1][j], dp[i][j]);
//            if(0 <= j - cw){
//                dp[i+1][j] = max(dp[i+1][j], dp[i][j - cw] + cv);
//            }
//        }
//    }

    long long res = 0;
//    for(int i = 0; i <= WW; i++) {
//        res = max(res, dp[N][i]);
//    }
    return res;
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long W;
    scanf("%lld",&W);
    std::vector<long long> w(N);
    std::vector<long long> v(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&w[i]);
        scanf("%lld",&v[i]);
    }
    auto result = solve(N, W, std::move(w), std::move(v));
    cout << result;
    return 0;
}
