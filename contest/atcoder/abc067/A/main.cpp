#include <bits/stdc++.h>
using namespace std;

const string YES = "Possible";
const string NO = "Impossible";

auto solve(long long A, long long B){ 
    return (((A % 3) == 0) || ((B % 3) == 0) || (((A + B) % 3) == 0)) ? YES : NO;
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
