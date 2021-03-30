#include <bits/stdc++.h>
using namespace std;


auto solve(long long n, long long m){
    return (n-1) * (m-1);
}

int main(){
    long long n;
    scanf("%lld",&n);
    long long m;
    scanf("%lld",&m);
    auto result = solve(n, m);
    cout << result << endl;
    return 0;
}
