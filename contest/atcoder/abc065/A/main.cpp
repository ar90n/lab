#include <bits/stdc++.h>
using namespace std;


auto solve(long long X, long long A, long long B){
    return ((A + X) < B) ? "dangerous" :
           (A < B) ? "safe" :
           "delicious";
}

int main(){
    long long X;
    scanf("%lld",&X);
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    auto result = solve(X, A, B);
    cout << result << endl;
    return 0;
}
