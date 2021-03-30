#include <bits/stdc++.h>
using namespace std;


auto solve(long long A, long long B){
    auto sum = A + B;
    stringstream ss;
    if(10 <= sum){
        ss << "error";
    }
    else{
        ss << sum;
    }

    return ss.str();
}

int main(){
    long long A;
    scanf("%lld",&A);
    long long B;
    scanf("%lld",&B);
    auto result = solve(A, B);
    cout << result << endl;
    return 0;
}
