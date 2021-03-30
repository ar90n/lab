#include <bits/stdc++.h>
using namespace std;


auto solve(long long A, long long B){
    return (A + B) % 24;
}

int main(){
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    auto result = solve(A, B);
    cout << result << endl;
    return 0;
}
