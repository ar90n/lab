#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long K, std::vector<long long> l){
    sort(begin(l), end(l), greater<long long>());
    auto bi = begin(l);
    auto ei = bi;
    advance(ei, K);
    return accumulate(bi , ei, 0LL);
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long K;
    scanf("%lld",&K);
    std::vector<long long> l(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&l[i]);
    }
    auto result = solve(N, K, std::move(l));
    cout << result << endl;
    return 0;
}
