.PHONY: all

all:
	dune build
	./_build/default/examples/native/main.exe

release:
	dune build --profile release
	./_build/default/examples/native/main.exe

server:
	python3 -m http.server 8080

perf:
	dune build --profile release
	perf record --call-graph dwarf ./_build/default/bin/main.exe
	perf report
