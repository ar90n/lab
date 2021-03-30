#include <bits/stdc++.h>
using namespace std;


auto solve(long long n, std::vector<long long> a){

}

int main(){
    long long n;
    scanf("%lld",&n);
    std::vector<long long> a(n);
    for(int i = 0 ; i < n ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(n, std::move(a));
    cout << result << endl;
    return 0;
}
