#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> a){
    vector< long long > acc{};
    for(const auto& v : a) {
        auto last = (0 < acc.size()) ? acc.back() : 0LL;
        acc.push_back(last + v);
    }

    long long ret = numeric_limits<long long >::max();
    for(int i = 0; i < (acc.size() - 1); ++i) {
        const auto v = acc[i];
        ret = min(ret, abs(acc.back() - 2 * v));
    }

    return ret;
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
