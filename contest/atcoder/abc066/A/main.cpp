#include <bits/stdc++.h>
using namespace std;


auto solve(long long a, long long b, long long c){
    return min(min(a + b, a + c), b + c);
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
