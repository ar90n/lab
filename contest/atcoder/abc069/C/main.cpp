#include <bits/stdc++.h>
using namespace std;

const string YES = "Yes";
const string NO = "No";

auto solve(long long N, std::vector<long long> a){
    int fs = 0;
    int os = 0;
    for(const auto& v : a) {
        fs += static_cast<int>((v % 4) == 0);
        os += static_cast<int>((v % 2) == 1);
    }

    return (os <= fs) ? YES :
           ((os == (fs + 1)) && ((fs + os) == a.size())) ? YES:
            NO;
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
