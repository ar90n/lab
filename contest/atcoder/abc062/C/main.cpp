#include <bits/stdc++.h>
using namespace std;


auto solve(long long H, long long W){
    auto ret = numeric_limits<long long>::max();
    for(long long i = 1LL; i < W; ++i) {
        auto j = static_cast<long long >(round((W - i) / 2));
        auto k = W - i - j;
        if(0 < k) {
            ret = min(ret, H * (max(i, max(j, k)) - min(i , min(j, k))));
        }

        j = i;
        k = W - i - j;
        if(0 < k) {
            ret = min(ret, H * (max(i, max(j, k)) - min(i , min(j, k))));
        }
    
        j = static_cast<long long>(round(H / 2));
        k = H - j;
        if(0 < k){
            ret = min(ret, max(H * i, (W - i) * max(j, k)) - min(H * i, (W - i) * min(j, k)));
        }

        j = static_cast<long long >(round((H * i) / (W - i)));
        k = H - j;
        if(0 < k) {
            ret = min(ret, max(H * i, (W - i) * max(j, k)) - min(H * i, (W - i) * min(j, k)));
        }
     }

    for(long long i = 1LL; i < H; ++i) {
        auto j = static_cast<long long >(round((H - i) / 2));
        auto k = H - i - j;
        if(0 < k){
            ret = min(ret, W * (max(i, max(j, k)) - min(i , min(j, k))));
        }

        j = i;
        k = H - i - j;
        if(0 < k){
            ret = min(ret, W * (max(i, max(j, k)) - min(i , min(j, k))));
        }

        j = static_cast<long long>(round(W / 2));
        k = W - j;
        if(0 < k){
            ret = min(ret, max(W * i, (H - i) * max(j, k)) - min(W * i, (H - i) * min(j, k)));
        }

        j = static_cast<long long >(round((W * i) / (H - i)));
        k = W - j;
        if(0 < k){
            ret = min(ret, max(W * i, (H - i) * max(j, k)) - min(W * i, (H - i) * min(j, k)));
        }
     }

    return ret;
}

int main(){
    long long H;
    scanf("%lld",&H);
    long long W;
    scanf("%lld",&W);
    auto result = solve(H, W);
    cout << result << endl;
    return 0;
}
