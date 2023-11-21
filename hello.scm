(use-modules (hoot compile)
             (ice-9 binary-ports)
             (wasm assemble))

(define src
  '(let ()
     (define-foreign document-body
       "document" "body"
       -> (ref null extern))
     (define-foreign make-text-node
       "document" "createTextNode"
       (ref string) -> (ref null extern))
     (define-foreign append-child!
       "element" "appendChild"
       (ref null extern) (ref null extern) -> (ref null extern))
     (append-child! (document-body) (make-text-node "Hello, world!"))))

(call-with-output-file "hello.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))
