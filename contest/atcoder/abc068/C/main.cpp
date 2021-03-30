#include <bits/stdc++.h>
using namespace std;

const string YES = "POSSIBLE";
const string NO = "IMPOSSIBLE";

auto solve(long long N, long long M, std::vector<long long> a, std::vector<long long> b){
    auto d = vector< vector<int> >(N + 1, vector<int>());
    for(int i = 0; i < M; ++i) {
        d[a[i]].push_back(b[i]);
    }

    queue<tuple<long long, int>> q;
    q.push(make_tuple(1, 0));
    while(!q.empty()) {
        const auto s = get<0>(q.front());
        const auto n = get<1>(q.front());
        q.pop();

        if(s == N) {
            return YES;
        }
        if(n == 2) {
            continue;
        }

        for(const auto dd : d[s]) {
            q.push(make_tuple(dd, n+1));
        }
    }

    return NO;
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long M;
    scanf("%lld",&M);
    std::vector<long long> a(M);
    std::vector<long long> b(M);
    for(int i = 0 ; i < M ; i++){
        scanf("%lld",&a[i]);
        scanf("%lld",&b[i]);
    }
    auto result = solve(N, M, std::move(a), std::move(b));
    cout << result << endl;
    return 0;
}
