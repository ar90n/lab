#include <bits/stdc++.h>
using namespace std;

const long long MOD = 5;
const string YES = "YES";
const string NO = "NO";

auto solve(long long A, long long B, long long C){
    for(long long i = 1; i <= B; i++) {
        if((A * i) % B == C){
            return YES;
        }
    }

    return NO;
}

int main(){
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    long long C;
    scanf("%lld",&C);
    auto result = solve(A, B, C);
    cout << result << endl;
    return 0;
}
