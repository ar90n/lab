#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long A, long long B, std::vector<long long> h){

}

int main(){
    long long N;
    scanf("%lld",&N);
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    std::vector<long long> h(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&h[i]);
    }
    auto result = solve(N, A, B, std::move(h));
    cout << result << endl;
    return 0;
}
