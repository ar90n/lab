#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> A){

}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> A(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&A[i]);
    }
    auto result = solve(N, std::move(A));
    cout << result << endl;
    return 0;
}
