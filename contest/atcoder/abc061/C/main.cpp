#include <bits/stdc++.h>
using namespace std;

auto solve(long long N, long long K, std::vector<long long> a, std::vector<long long> b) {
    vector<tuple<long long, long long>> vs;
    for(int i = 0; i < N; ++i) {
        vs.push_back(make_tuple(a[i], b[i]));
    }
    sort(begin(vs), end(vs));

    long long acc = 0;
    for(const auto& v : vs) {
        acc += get<1>(v);
        if(K <= acc) {
            return get<0>(v);
        }
    }
    return b[N-1];
}

int main() {
  long long N;
  scanf("%lld", &N);
  long long K;
  scanf("%lld", &K);
  std::vector<long long> a(N);
  std::vector<long long> b(N);
  for (int i = 0; i < N; i++) {
    scanf("%lld", &a[i]);
    scanf("%lld", &b[i]);
  }
  auto result = solve(N, K, std::move(a), std::move(b));
  cout << result << endl;
  return 0;
}
