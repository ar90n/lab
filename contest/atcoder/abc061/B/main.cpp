#include <bits/stdc++.h>
using namespace std;

auto solve(long long N, long long M, std::vector<long long> a, std::vector<long long> b) {
    std::vector<int> ns(N, 0);
    for(int i = 0;i < M; ++i){
        ns[a[i]-1]++;
        ns[b[i]-1]++;
    }

    stringstream ss;
    for(const auto& n : ns){
        ss << n << endl;
    }
    return ss.str();
}

int main() {
  long long N;
  scanf("%lld", &N);
  long long M;
  scanf("%lld", &M);
  std::vector<long long> a(M);
  std::vector<long long> b(M);
  for (int i = 0; i < M; i++) {
    scanf("%lld", &a[i]);
    scanf("%lld", &b[i]);
  }
  auto result = solve(N, M, std::move(a), std::move(b));
  cout << result;
  return 0;
}
