#include <bits/stdc++.h>
using namespace std;


template<typename T>
auto gcd(T a, T b) -> T {
    T minv = min(a, b);
    T maxv = max(a, b);
    return (minv == 0) ? maxv : gcd(minv, (maxv % minv));
}

template<typename T>
auto lcm(T a, T b) -> T {
    return a * (b / gcd(a, b));
}


auto solve(long long N, std::vector<long long> T){
    return accumulate(T.begin(), T.end(), 1LL, [](long long a, long long b){return lcm(a, b);});
}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> T(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&T[i]);
    }
    auto result = solve(N, std::move(T));
    cout << result << endl;
    return 0;
}
