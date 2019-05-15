template<typename T>
auto gcd(T a, T b) -> T {
    T minv = min(a, b);
    T maxv = max(a, b);
    return (minv == 0) ? maxv : gcd(minv, (maxv % minv));
}

template<typename T>
auto lcm(T a, T b) -> T {
    return a * (b / gcd(a, b));
}
