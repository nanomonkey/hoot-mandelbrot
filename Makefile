all: hello.wasm sxml.wasm counter.wasm todo.wasm canvas.wasm

hello.wasm: hello.scm
	guile hello.scm

sxml.wasm: sxml.scm
	guile sxml.scm

counter.wasm: counter.scm
	guile counter.scm

todo.wasm: todo.scm
	guile todo.scm

canvas.wasm: canvas.scm
	guile canvas.scm

serve: *.wasm
	guile web-server.scm

clean:
	rm *.wasm
