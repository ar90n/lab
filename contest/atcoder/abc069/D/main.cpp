#include <bits/stdc++.h>
using namespace std;


auto solve(long long H, long long W, long long N, std::vector<long long> a){

}

int main(){
    long long H;
    scanf("%lld",&H);
    long long W;
    scanf("%lld",&W);
    long long N;
    scanf("%lld",&N);
    std::vector<long long> a(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(H, W, N, std::move(a));
    cout << result << endl;
    return 0;
}
