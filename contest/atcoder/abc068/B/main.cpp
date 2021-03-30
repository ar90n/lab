#include <bits/stdc++.h>
using namespace std;


auto solve(long long N){
    int i = 0;
    while(1 < N){
        i++;
        N >>= 1;
    }

    return 1 << i;
}

int main(){
    long long N;
    scanf("%lld",&N);
    auto result = solve(N);
    cout << result << endl;
    return 0;
}
