package main

import (
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"testing"

	"golang.org/x/sync/errgroup"
)

func TestServer_Run(t *testing.T) {
	l, err := net.Listen("tcp", "localhost:0")
	if err != nil {
		t.Errorf("failed to listen: %+v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	eg, ctx := errgroup.WithContext(ctx)
	mux := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello %s!", r.URL.Path[1:])
	})

	eg.Go(func() error {
		s := NewServer(l, mux)
		return s.Run(ctx)
	})

	in := "message"
	url := fmt.Sprintf("http://%s/%s", l.Addr(), in)
	t.Logf("url: %q", url)
	rsp, err := http.Get(url)
	if err != nil {
		t.Errorf("failed to get: %+v", err)
	}
	defer func() {
		if rsp != nil {
			rsp.Body.Close()
		}
	}()

	got, err := io.ReadAll(rsp.Body)
	if err != nil {
		t.Errorf("failed to read: %+v", err)
	}

	want := fmt.Sprintf("Hello %s!", in)
	if string(got) != want {
		t.Errorf("got %q, want %q", got, want)
	}

	cancel()
	if err := eg.Wait(); err != nil {
		t.Errorf("failed to wait: %+v", err)
	}
}
