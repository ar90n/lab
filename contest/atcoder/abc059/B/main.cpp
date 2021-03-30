#include <bits/stdc++.h>
using namespace std;


auto solve(string A, string B){
    if(A.size() < B.size()) {
        return "LESS";
    }
    else if(B.size() < A.size()) {
        return "GREATER";
    }
    else {
        int i = 0;
        for(;i < A.size(); ++i) {
            int a = A[i] - '0';
            int b = B[i] - '0';
            if(b < a) {
                return "GREATER";
            }
            else if(a < b) {
                return "LESS";
            }
        }
    }

    return "EQUAL";
}

int main(){
    string A;
    cin >> A;
    string B;
    cin >> B;
    auto result = solve(A, B);
    cout << result << endl;
    return 0;
}
