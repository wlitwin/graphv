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

unlink:
	@# gles2
	@rm -rf gles2/core
	@rm -rf gles2/native
	@rm -rf gles2/native2
	@rm -rf gles2/gl
	@rm -rf gles2/font
	@rm -rf gles2/graphv
	@# gles3
	@rm -rf gles3/core
	@rm -rf gles3/native
	@rm -rf gles3/native3
	@rm -rf gles3/gl
	@rm -rf gles3/font
	@rm -rf gles3/graphv
	@# webgl1
	@rm -rf webgl1/core
	@rm -rf webgl1/js
	@rm -rf webgl1/js2
	@rm -rf webgl1/gl
	@rm -rf webgl1/font
	@rm -rf webgl1/graphv
	@# webgl2
	@rm -rf webgl2/core
	@rm -rf webgl2/js
	@rm -rf webgl2/js3
	@rm -rf webgl2/gl
	@rm -rf webgl2/font
	@rm -rf webgl2/graphv

link: unlink
	@# gles 2
	@ln -s $(CURDIR)/src/core gles2/core
	@ln -s $(CURDIR)/src/font gles2/font
	@ln -s $(CURDIR)/src/graphv gles2/graphv
	@ln -s $(CURDIR)/src/gl2 gles2/gl
	@ln -s $(CURDIR)/src/native gles2/native
	@ln -s $(CURDIR)/src/native2 gles2/native2
	@# gles3
	@ln -s $(CURDIR)/src/core gles3/core
	@ln -s $(CURDIR)/src/font gles3/font
	@ln -s $(CURDIR)/src/graphv gles3/graphv
	@ln -s $(CURDIR)/src/gl3 gles3/gl
	@ln -s $(CURDIR)/src/native gles3/native
	@ln -s $(CURDIR)/src/native3 gles3/native3
	@# webgl1
	@ln -s $(CURDIR)/src/core webgl1/core
	@ln -s $(CURDIR)/src/font webgl1/font
	@ln -s $(CURDIR)/src/graphv webgl1/graphv
	@ln -s $(CURDIR)/src/gl2 webgl1/gl
	@ln -s $(CURDIR)/src/js webgl1/js
	@ln -s $(CURDIR)/src/js2 webgl1/js2
	@# webgl2
	@ln -s $(CURDIR)/src/core webgl2/core
	@ln -s $(CURDIR)/src/font webgl2/font
	@ln -s $(CURDIR)/src/graphv webgl2/graphv
	@ln -s $(CURDIR)/src/gl3 webgl2/gl
	@ln -s $(CURDIR)/src/js webgl2/js
	@ln -s $(CURDIR)/src/js3 webgl2/js3

copy: unlink
	@# gles 2
	@cp -r $(CURDIR)/src/core gles2/core
	@cp -r $(CURDIR)/src/font gles2/font
	@cp -r $(CURDIR)/src/graphv gles2/graphv
	@cp -r $(CURDIR)/src/gl2 gles2/gl
	@cp -r $(CURDIR)/src/native gles2/native
	@cp -r $(CURDIR)/src/native2 gles2/native2
	@# gles3
	@cp -r $(CURDIR)/src/core gles3/core
	@cp -r $(CURDIR)/src/font gles3/font
	@cp -r $(CURDIR)/src/graphv gles3/graphv
	@cp -r $(CURDIR)/src/gl3 gles3/gl
	@cp -r $(CURDIR)/src/native gles3/native
	@cp -r $(CURDIR)/src/native3 gles3/native3
	@# webgl1
	@cp -r $(CURDIR)/src/core webgl1/core
	@cp -r $(CURDIR)/src/font webgl1/font
	@cp -r $(CURDIR)/src/graphv webgl1/graphv
	@cp -r $(CURDIR)/src/gl2 webgl1/gl
	@cp -r $(CURDIR)/src/js webgl1/js
	@cp -r $(CURDIR)/src/js2 webgl1/js2
	@# webgl2
	@cp -r $(CURDIR)/src/core webgl2/core
	@cp -r $(CURDIR)/src/font webgl2/font
	@cp -r $(CURDIR)/src/graphv webgl2/graphv
	@cp -r $(CURDIR)/src/gl3 webgl2/gl
	@cp -r $(CURDIR)/src/js webgl2/js
	@cp -r $(CURDIR)/src/js3 webgl2/js3
	

test:
	dune build test/gles2/graphv_gles2_tests.exe --profile release
	./_build/default/test/gles2/graphv_gles2_tests.exe

perf:
	dune build --profile release
	perf record --call-graph dwarf ./_build/default/bin/main.exe
	perf report
