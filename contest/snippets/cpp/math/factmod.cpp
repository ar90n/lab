template<typename T>
auto factmod(T n, T m) -> T {
    T ret = 1;
    for(T i = 1; i <= n; ++i) {
        ret = (ret * i) % m;
    }

    return ret;
}
