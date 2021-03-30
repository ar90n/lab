#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> a, std::vector<long long> b){

}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> a(N-1);
    std::vector<long long> b(N-1);
    for(int i = 0 ; i < N-1 ; i++){
        scanf("%lld",&a[i]);
        scanf("%lld",&b[i]);
    }
    auto result = solve(N, std::move(a), std::move(b));
    cout << result << endl;
    return 0;
}
