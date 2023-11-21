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
     (define-foreign make-element
       "document" "createElement"
       (ref string) -> (ref null extern))
     (define-foreign append-child!
       "element" "appendChild"
       (ref null extern) (ref null extern) -> (ref null extern))
     (define-foreign set-attribute!
       "element" "setAttribute"
       (ref null extern) (ref string) (ref string) -> none)
     (define (render-sxml exp)
       (match exp
         ((? string? str)
          (make-text-node str))
         (((? symbol? tag) . body)
          (let ((elem (make-element (symbol->string tag))))
            (define (add-children children)
              (for-each (lambda (child)
                          (append-child! elem (render-sxml child)))
                        children))
            (match body
              ((('@ . attrs) . children)
               (for-each (lambda (attr)
                           (match attr
                             (((? symbol? name) (? string? val))
                              (set-attribute! elem
                                              (symbol->string name)
                                              val))))
                         attrs)
               (add-children children))
              (children (add-children children)))
            elem))))
     (define sxml
       '(section
         (h1 "Scheme rocks!")
         (p "With Scheme, data is code and code is data.")
         (small "Made with "
                (a (@ (href "https://spritely.institute/hoot"))
                   "Guile Hoot"))))
     (append-child! (document-body) (render-sxml sxml))))

(call-with-output-file "sxml.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))
