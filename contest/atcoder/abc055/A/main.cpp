#include <bits/stdc++.h>
using namespace std;


auto solve(long long N){
    return 800 * N - 200 * (N / 15);
}

int main(){
    long long N;
    scanf("%lld",&N);
    auto result = solve(N);
    cout << result << endl;
    return 0;
}
