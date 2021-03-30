#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> a){
    array<long long, 9> c{0};

    for(const auto v :a) {
        int i = 0;
        if(3200LL <= v) i++; 
        if(2800LL <= v) i++; 
        if(2400LL <= v) i++;
        if(2000LL <= v) i++;
        if(1600LL <= v) i++;
        if(1200LL <= v) i++;
        if(800LL  <= v) i++; 
        if(400LL  <= v) i++;
        c[i]++;
    }

    long long acc = 0LL;
    for(int i = 0; i < 8; ++i) {
        if(0 < c[i]) {
            acc++;
        }
    }
    long long minv = max(acc, static_cast<long long >((0LL < c.back())));
    long long maxv = acc + c.back();

    stringstream ss;
    ss << minv << " " << maxv;
    return ss.str();
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
