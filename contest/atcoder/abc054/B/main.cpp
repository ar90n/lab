#include <bits/stdc++.h>
using namespace std;

const string YES = "Yes";
const string NO = "No";

auto solve(long long N, long long M, std::vector<std::string> A, std::vector<std::string> B){
    for(int i = 0; i < (N - M + 1); ++i) {
        for(int j = 0; j < (N - M + 1); ++j) {
            bool match = true;
            for(int k = 0; k < M; ++k ){
                for(int l = 0; l < M; ++l ){
                    match &= (A[i+k][j+l] == B[k][l]);
                }
            }

            if(match) {
                return YES;
            }
        }
    }
    return NO;
}

int main(){
    long long N;
    scanf("%lld",&N);
    long long M;
    scanf("%lld",&M);
    std::vector<std::string> A(N);
    for(int i = 0 ; i < N ; i++){
        std::cin >> A[i];
    }
    std::vector<std::string> B(M);
    for(int i = 0 ; i < M ; i++){
        std::cin >> B[i];
    }
    auto result = solve(N, M, std::move(A), std::move(B));
    cout << result << endl;
    return 0;
}
