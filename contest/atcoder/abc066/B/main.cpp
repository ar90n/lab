#include <bits/stdc++.h>
using namespace std;


auto solve(std::string S){
    auto bi = begin(S);
    auto ei = end(S);
    auto mi = bi + (S.size() / 2);

    while(bi != mi) {
        mi -= 1;
        ei -= 2;
        if(equal(bi, mi, mi)){
            break;
        }
    }
    return (bi == mi) ? -1 : distance(bi, ei);
}

int main(){
    std::string S;
    std::cin >> S;
    auto result = solve(S);
    cout << result << endl;
    return 0;
}
