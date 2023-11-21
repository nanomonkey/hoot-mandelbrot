(use-modules (hoot compile)
             (ice-9 binary-ports)
             (wasm assemble))

(define src
  '(let ()
     (define-foreign document-body
       "document" "body"
       -> (ref null extern))
     (define-foreign get-element-by-id
       "document" "getElementById"
       (ref string) -> (ref null extern))
     (define-foreign make-text-node
       "document" "createTextNode"
       (ref string) -> (ref null extern))
     (define-foreign make-element
       "document" "createElement"
       (ref string) -> (ref null extern))
     (define-foreign add-event-listener!
       "element" "addEventListener"
       (ref null extern) (ref string) (ref null extern) -> none)
     (define-foreign append-child!
       "element" "appendChild"
       (ref null extern) (ref null extern) -> (ref null extern))
     (define-foreign remove!
       "element" "remove"
       (ref null extern) -> none)
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
                                              val))
                             ;; This is the new bit.
                             (((? symbol? name) (? procedure? proc))
                              (add-event-listener! elem
                                                   (symbol->string name)
                                                   (procedure->external proc)))))
                         attrs)
               (add-children children))
              (children (add-children children)))
            elem))))
     (define *clicks* 0)
     (define (template)
       `(div (@ (id "container"))
         (p ,(number->string *clicks*) " clicks")
         (button (@ (click ,(lambda (event)
                              (set! *clicks* (+ *clicks* 1))
                              (render))))
                 "Click me")))
     (define (render)
       (let ((old (get-element-by-id "container")))
         (unless (external-null? old) (remove! old)))
       (append-child! (document-body) (render-sxml (template))))
     (render)))

(call-with-output-file "counter.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))
