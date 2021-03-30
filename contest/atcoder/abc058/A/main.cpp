#include <bits/stdc++.h>
using namespace std;

const string YES = "YES";
const string NO = "NO";

auto solve(long long a, long long b, long long c){
    return ((b - a) == (c - b)) ? YES : NO;
}

int main(){
    long long a;
    scanf("%lld",&a);
    long long b;
    scanf("%lld",&b);
    long long c;
    scanf("%lld",&c);
    auto result = solve(a, b, c);
    cout << result << endl;
    return 0;
}
