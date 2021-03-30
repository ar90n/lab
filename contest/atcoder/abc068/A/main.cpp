#include <bits/stdc++.h>
using namespace std;


auto solve(long long N){
    stringstream ss;
    ss << "000" << N;
    auto s = ss.str();
    string res(end(s) - 3, end(s));
    stringstream ss2;
    ss2 << "ABC" << res;
    return ss2.str();
}

int main(){
    long long N;
    scanf("%lld",&N);
    auto result = solve(N);
    cout << result << endl;
    return 0;
}
