all: hello.wasm sxml.wasm counter.wasm todo.wasm

hello.wasm: hello.scm
	guile hello.scm

sxml.wasm: sxml.scm
	guile sxml.scm

counter.wasm: counter.scm
	guile counter.scm

todo.wasm: todo.scm
	guile todo.scm

serve:
	guile web-server.scm

clean:
	rm *.wasm
