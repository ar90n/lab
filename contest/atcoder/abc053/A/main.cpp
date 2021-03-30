#include <bits/stdc++.h>
using namespace std;


auto solve(long long x){
    return (x < 1200) ? "ABC" : "ARC";
}

int main(){
    long long x;
    scanf("%lld",&x);
    auto result = solve(x);
    cout << result << endl;
    return 0;
}
