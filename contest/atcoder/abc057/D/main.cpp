#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long A, long long B, std::vector<long long> v){

}

int main(){
    long long N;
    scanf("%lld",&N);
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    std::vector<long long> v(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&v[i]);
    }
    auto result = solve(N, A, B, std::move(v));
    cout << result << endl;
    return 0;
}
