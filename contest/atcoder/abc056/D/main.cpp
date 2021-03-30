#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long K, std::vector<long long> a){

}

int main(){
    long long N;
    scanf("%lld",&N);
    long long K;
    scanf("%lld",&K);
    std::vector<long long> a(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(N, K, std::move(a));
    cout << result << endl;
    return 0;
}
