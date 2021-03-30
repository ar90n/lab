#include <bits/stdc++.h>
using namespace std;


auto solve(long long W, long long a, long long b){
    if((b <= (a + W)) && (a <= (b + W))) {
        return 0LL;
    }
    return min(abs((a + W) - b), abs(a - (b + W)));
}

int main(){
    long long W;
    scanf("%lld",&W);
    long long a;
    scanf("%lld",&a);
    long long b;
    scanf("%lld",&b);
    auto result = solve(W, a, b);
    cout << result << endl;
    return 0;
}
