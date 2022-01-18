package main

import (
	"testing"
)

func DynamicFunction(f func(a, b int) int, n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			f(i, j)
		}
	}
}

func DynamicFunctor(f Functor, n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			f.Call(i, j)
		}
	}
}

func StaticFunctor[F Functor](f F, n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			f.Call(i, j)
		}
	}
}

type Functor interface {
	Call(a, b int) int
}

type Add struct {}

func (a Add) Call(x, y int) int {
	return add(x, y)
}


func add(x, y int) int {
	return x + y
}

func BenchmarkCall_DynamicFunction(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		DynamicFunction(add, 100)
	}
}

func BenchmarkCall_DynamicFunctor(b *testing.B) {
	b.ResetTimer()
	add := Add{}
	for i := 0; i < b.N; i++ {
		DynamicFunctor(add, 100)
	}
}

func BenchmarkCall_StaticFunctor(b *testing.B) {
	b.ResetTimer()
	add := Add{}
	for i := 0; i < b.N; i++ {
		StaticFunctor[Add](add, 100)
	}
}
