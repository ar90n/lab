#include <bits/stdc++.h>
using namespace std;


auto solve(long long n, std::vector<long long> a){
    vector<long long > res;

    auto bi0 = rbegin(a);
    auto ei0 = rend(a);
    while(true) {
        res.push_back(*bi0);
        bi0++;
        if(bi0 == ei0){
            break;
        }
        bi0++;
        if(bi0 == ei0){
            break;
        }
    }

    auto bi = begin(a);
    auto ei = end(a);
    if(n % 2 == 1) {
        bi++;
    }
    while(bi != ei) {
        res.push_back(*bi);
        bi++;
        if(bi == ei){
            break;
        }
        bi++;
        if(bi == ei){
            break;
        }
    }

    stringstream ss;
    ss << res[0];
    for(int i = 1; i< res.size(); i++) {
        ss << " " << res[i];
    }
    return ss.str();
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
