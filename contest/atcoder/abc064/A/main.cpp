#include <bits/stdc++.h>
using namespace std;

const string YES = "YES";
const string NO = "NO";

auto solve(long long r, long long g, long long b){
    auto s = 100 * r + 10 * g + b;
    return (s % 4 == 0) ? YES : NO;
}

int main(){
    long long r;
    scanf("%lld",&r);
    long long g;
    scanf("%lld",&g);
    long long b;
    scanf("%lld",&b);
    auto result = solve(r, g, b);
    cout << result << endl;
    return 0;
}
