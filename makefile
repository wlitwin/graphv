.PHONY: all release server test perf

all:
	dune build
	./_build/default/examples/native/main.exe

release:
	dune build --profile release
	./_build/default/examples/native/main.exe

server:
	python3 -m http.server 8080

test:
	dune build test/gles2/graphv_gles2_tests.exe --profile release
	./_build/default/test/gles2/graphv_gles2_tests.exe

perf:
	dune build --profile release
	perf record --call-graph dwarf ./_build/default/bin/main.exe
	perf report
