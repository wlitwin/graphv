.PHONY: all release server test perf

gles3:
	dune build --profile release
	./_build/default/examples/native_gles3/main.exe

trace:
	dune build --profile release
	MEMTRACE=trace.ctf ./_build/default/examples/native_gles3/main.exe 1
	MEMTRACE=trace2.ctf ./_build/default/examples/native/main.exe 1

gles2:
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
