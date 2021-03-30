#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> s){
    sort(begin(s), end(s));
    auto acc =accumulate(begin(s), end(s), 0LL);
    if((acc % 10) != 0) {
        return acc;
    }

    for(const auto v : s) {
        if(((acc - v) % 10) != 0) {
            return acc - v;
        }
    }

    return 0LL;
}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> s(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&s[i]);
    }
    auto result = solve(N, std::move(s));
    cout << result << endl;
    return 0;
}
