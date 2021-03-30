#include <bits/stdc++.h>
using namespace std;


auto solve(long long A, long long B){
    int p[] = {0, 13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13};
    return (p[A] < p[B]) ? "Bob":
        (p[A] > p[B]) ? "Alice":
        "Draw";
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
