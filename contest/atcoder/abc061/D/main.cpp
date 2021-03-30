#include <bits/stdc++.h>
using namespace std;

auto solve(long long N, long long M, std::vector<long long> a,
           std::vector<long long> b, std::vector<long long> c) {}

int main() {
  long long N;
  scanf("%lld", &N);
  long long M;
  scanf("%lld", &M);
  std::vector<long long> a(M);
  std::vector<long long> b(M);
  std::vector<long long> c(M);
  for (int i = 0; i < M; i++) {
    scanf("%lld", &a[i]);
    scanf("%lld", &b[i]);
    scanf("%lld", &c[i]);
  }
  auto result = solve(N, M, std::move(a), std::move(b), std::move(c));
  cout << result << endl;
  return 0;
}
