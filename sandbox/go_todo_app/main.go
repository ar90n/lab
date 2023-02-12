package main

import (
	"context"
	"fmt"
	"log"
	"net"
	"os"

	"github.com/ar90n/lab/sandbox/go_todo_app/config"
)

func run(ctx context.Context) error {
	cfg, err := config.New()
	if err != nil {
		return err
	}

	l, err := net.Listen("tcp", fmt.Sprintf(":%d", cfg.Port))
	if err != nil {
		log.Fatalf("failed to listen: %+v", err)
	}
	url := fmt.Sprintf("http://%s", l.Addr().String())
	log.Printf("url: %q", url)

	mux := NewMux()
	s := NewServer(l, mux)
	return s.Run(ctx)
}

func main() {
	ctx := context.Background()
	if err := run(ctx); err != nil {
		log.Printf("failed to run: %+v", err)
		os.Exit(1)
	}
}
