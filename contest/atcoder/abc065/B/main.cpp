#include <bits/stdc++.h>
using namespace std;


auto solve(long long N, std::vector<long long> a){
    int i = 0;
    int c = 0;
    while((0 < a[i]) && (i != 1)) {
        int n = a[i] - 1;
        a[i] = -1;
        i = n;
        c++;
    }

    return (i == 1) ? c : -1;

}

int main(){
    long long N;
    scanf("%lld",&N);
    std::vector<long long> a(N);
    for(int i = 0 ; i < N ; i++){
        scanf("%lld",&a[i]);
    }
    auto result = solve(N, std::move(a));
    cout << result << endl;
    return 0;
}
