#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> a){

}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> a(3*N);
    for(int i = 0 ; i < 3*N ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(N, std::move(a));
    cout << result << endl;
    return 0;
}
