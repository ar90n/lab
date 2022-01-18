# go_static_dispatch_benchmark

## Result

```
$ go test -bench . -benchmem
goos: linux
goarch: amd64
pkg: github.com/ar90n/lab/go_static_dispatch_benchmark
cpu: AMD Ryzen 9 3950X 16-Core Processor            
BenchmarkCall_DynamicFunction-32           82634             13550 ns/op               0 B/op          0 allocs/op
BenchmarkCall_DynamicFunctor-32            61860             19771 ns/op               0 B/op          0 allocs/op
BenchmarkCall_StaticFunctor-32             62876             19559 ns/op               0 B/op          0 allocs/op
PASS
ok      github.com/ar90n/lab/go_static_dispatch_benchmark       4.121s
```
