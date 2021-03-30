#include <bits/stdc++.h>
using namespace std;

const string YES = "Yes";
const string NO = "No";

auto solve(long long N){
    stringstream ss;
    ss << N;
    auto s = ss.str();
    return equal(begin(s), end(s), rbegin(s)) ? YES : NO;
}

int main(){
    long long N;
    scanf("%lld",&N);
    auto result = solve(N);
    cout << result << endl;
    return 0;
}
