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
     (define (sxml->dom exp)
       (match exp
         ;; The simple case: a string representing a text node.
         ((? string? str)
          (make-text-node str))
         ;; An element tree.  The first item is the HTML tag.
         (((? symbol? tag) . body)
          ;; Create a new element with the given tag.
          (let ((elem (make-element (symbol->string tag))))
            (define (add-children children)
              ;; Recursively call sxml->dom for each child node and
              ;; append it to elem.
              (for-each (lambda (child)
                          (append-child! elem (sxml->dom child)))
                        children))
            (match body
              ;; '@' denotes an attribute list.  Child nodes follow.
              ((('@ . attrs) . children)
               ;; Set attributes.
               (for-each (lambda (attr)
                           (match attr
                             ;; Attributes are (symbol string) tuples.
                             (((? symbol? name) (? string? val))
                              (set-attribute! elem
                                              (symbol->string name)
                                              val))))
                         attrs)
               (add-children children))
              ;; No attributes, just a list of child nodes.
              (children (add-children children)))
            elem))))
     (define sxml
       '(section
         (h1 "Scheme rocks!")
         (p "With Scheme, data is code and code is data.")
         (small "Made with "
                (a (@ (href "https://spritely.institute/hoot"))
                   "Guile Hoot"))))
     (append-child! (document-body) (sxml->dom sxml))))

(call-with-output-file "sxml.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))
