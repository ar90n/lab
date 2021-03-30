#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> x, std::vector<long long> y){

}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> x(N);
    std::vector<long long> y(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&x[i]);
        scanf("%lld",&y[i]);
    }
    auto result = solve(N, std::move(x), std::move(y));
    cout << result << endl;
    return 0;
}
