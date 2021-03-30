#include <bits/stdc++.h>
using namespace std;

const string YES = "Yes";
const string NO = "No";

auto solve(long long x, long long y){
    int gs[] = {0, 1, 3, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1};
    return gs[x] == gs[y] ?  YES : NO;
}

int main(){
    long long x;
    scanf("%lld",&x);
    long long y;
    scanf("%lld",&y);
    auto result = solve(x, y);
    cout << result << endl;
    return 0;
}
