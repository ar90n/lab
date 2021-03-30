#include <bits/stdc++.h>
using namespace std;

const string YES = "Yes";
const string NO = "No";

auto solve(long long A, long long B, long long C) {
    return ((A <= C) && (C <= B)) ? YES : NO;
}

int main() {
  long long A;
  scanf("%lld", &A);
  long long B;
  scanf("%lld", &B);
  long long C;
  scanf("%lld", &C);
  auto result = solve(A, B, C);
  cout << result << endl;
  return 0;
}
