#include <bits/stdc++.h>
using namespace std;

const string YES = "YES";
const string NO = "NO";

auto solve(std::string A, std::string B, std::string C){
    if((A[A.size()-1] == B[0]) && (B[B.size()-1] == C[0])) {
        return YES;
    }
    else {
        return NO;
    }
}

int main(){
    std::string A;
    std::cin >> A;
    std::string B;
    std::cin >> B;
    std::string C;
    std::cin >> C;
    auto result = solve(A, B, C);
    cout << result << endl;
    return 0;
}
