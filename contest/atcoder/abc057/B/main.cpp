#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, long long M, std::vector<long long> a, std::vector<long long> b, std::vector<long long> c, std::vector<long long> d){
    std::vector<int> result(N);

    for(int i = 0; i < N; ++i){
        long long minv = numeric_limits<long long>::max();
        for(int j = 0; j < M; ++j) {
            long long dist = abs(a[i] - c[j]) + abs(b[i] - d[j]);
            if(dist < minv){
                minv = dist;
                result[i] = j;
            }
        }
    }


    stringstream ss;
    for(const auto& i : result) {
        ss << (i + 1) << endl;
    }
    return ss.str();
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long M;
    scanf("%lld",&M);
    std::vector<long long> a(N);
    std::vector<long long> b(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&a[i]);
        scanf("%lld",&b[i]);
    }
    std::vector<long long> c(M);
    std::vector<long long> d(M);
    for(int i = 0 ; i < M ; i++){
        scanf("%lld",&c[i]);
        scanf("%lld",&d[i]);
    }
    auto result = solve(N, M, std::move(a), std::move(b), std::move(c), std::move(d));
    cout << result;
    return 0;
}
