#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long T, std::vector<long long> t){
    long long acc = 0;
    for(int i = 0; i < (N-1); ++i) {
        acc += min(T, t[i+1] - t[i]);
    }
    acc += T;

    return acc;
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long T;
    scanf("%lld",&T);
    std::vector<long long> t(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&t[i]);
    }
    auto result = solve(N, T, std::move(t));
    cout << result << endl;;
    return 0;
}
