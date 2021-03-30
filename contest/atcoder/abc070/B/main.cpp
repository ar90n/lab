#include <bits/stdc++.h>
using namespace std;


auto solve(long long A, long long B, long long C, long long D){
    return max(min(B, D) - max(A, C), 0LL);
}

int main(){
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    long long C;
    scanf("%lld",&C);
    long long D;
    scanf("%lld",&D);
    auto result = solve(A, B, C, D);
    cout << result << endl;
    return 0;
}
