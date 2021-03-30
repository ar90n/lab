#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long M_a, long long M_b, std::vector<long long> a, std::vector<long long> b, std::vector<long long> c){

}

int main(){
    long long N;
    scanf("%lld",&N);
    long long M_a;
    scanf("%lld",&M_a);
    long long M_b;
    scanf("%lld",&M_b);
    std::vector<long long> a(N);
    std::vector<long long> b(N);
    std::vector<long long> c(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&a[i]);
        scanf("%lld",&b[i]);
        scanf("%lld",&c[i]);
    }
    auto result = solve(N, M_a, M_b, std::move(a), std::move(b), std::move(c));
    cout << result << endl;
    return 0;
}
