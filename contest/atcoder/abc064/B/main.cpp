#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> a){
    auto r = minmax_element(begin(a), end(a));
    return *r.second - *r.first;
}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> a(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(N, std::move(a));
    cout << result << endl;
    return 0;
}
