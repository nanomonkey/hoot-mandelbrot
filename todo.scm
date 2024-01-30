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
     (define-foreign make-tree-walker
       "document" "createTreeWalker"
       (ref null extern) -> (ref null extern))

     (define-foreign element-value
       "element" "value"
       (ref null extern) -> (ref string))
     (define-foreign set-element-value!
       "element" "setValue"
       (ref null extern) (ref string) -> none)
     (define-foreign %element-checked?
       "element" "checked"
       (ref null extern) -> i32)
     (define (element-checked? elem)
       (= (%element-checked? elem) 1))
     (define-foreign set-element-checked!
       "element" "setChecked"
       (ref null extern) i32 -> none)
     (define-foreign add-event-listener!
       "element" "addEventListener"
       (ref null extern) (ref string) (ref null extern) -> none)
     (define-foreign remove-event-listener!
       "element" "removeEventListener"
       (ref null extern) (ref string) (ref null extern) -> none)
     (define-foreign append-child!
       "element" "appendChild"
       (ref null extern) (ref null extern) -> (ref null extern))
     (define-foreign remove!
       "element" "remove"
       (ref null extern) -> none)
     (define-foreign replace-with!
       "element" "replaceWith"
       (ref null extern) (ref null extern) -> none)
     (define-foreign set-attribute!
       "element" "setAttribute"
       (ref null extern) (ref string) (ref string) -> none)
     (define-foreign remove-attribute!
       "element" "removeAttribute"
       (ref null extern) (ref string) -> none)

     (define-foreign event-target
       "event" "target"
       (ref null extern) -> (ref null extern))

     (define-foreign current-node
       "treeWalker" "currentNode"
       (ref null extern) -> (ref null extern))
     (define-foreign set-current-node!
       "treeWalker" "setCurrentNode"
       (ref null extern) (ref null extern) -> (ref null extern))
     (define-foreign next-node!
       "treeWalker" "nextNode"
       (ref null extern) -> (ref null extern))
     (define-foreign first-child!
       "treeWalker" "firstChild"
       (ref null extern) -> (ref null extern))
     (define-foreign next-sibling!
       "treeWalker" "nextSibling"
       (ref null extern) -> (ref null extern))

     (define procedure->external/cached
       (let ((cache (make-weak-key-hashtable)))
         (lambda (proc)
           (or (weak-key-hashtable-ref cache proc)
               (let ((f (procedure->external proc)))
                 (weak-key-hashtable-set! cache proc f)
                 f)))))

     (define (add-event-listener!/wrap elem name proc)
       (add-event-listener! elem name (procedure->external/cached proc)))
     (define (remove-event-listener!/wrap elem name proc)
       (remove-event-listener! elem name (procedure->external/cached proc)))

     (define (set-attribute!* elem name val)
       (if (string=? name "checked")
           ;; Special case for input 'checked' attribute.  Instead of
           ;; setting an attribute, we set the property.  It's a hack,
           ;; but fine for this little demo.
           (set-element-checked! elem (if val 1 0))
           (set-attribute! elem name val)))

     (define (attr-value? x)
       (or (string? x) (boolean? x)))

     (define (sxml->dom exp)
       (match exp
         ((? string? str)
          (make-text-node str))
         (((? symbol? tag) . body)
          (let ((elem (make-element (symbol->string tag))))
            (define (add-children children)
              (for-each (lambda (child)
                          (append-child! elem (sxml->dom child)))
                        children))
            (match body
              ((('@ . attrs) . children)
               (for-each (lambda (attr)
                           (match attr
                             (((? symbol? name) (? attr-value? val))
                              (set-attribute!* elem
                                               (symbol->string name)
                                               val))
                             (((? symbol? name) (? procedure? proc))
                              (let ((name* (symbol->string name)))
                                (add-event-listener!/wrap elem name* proc)))))
                         attrs)
               (add-children children))
              (children (add-children children)))
            elem))))

     (define (virtual-dom-render root old new)
       (define (attrs+children exp)
         (match exp
           ((('@ . attrs) . children)
            (values attrs children))
           (children
            (values '() children))))
       (define (find-attr attrs name)
         (match attrs
           (() #f)
           ((attr . rest)
            (match attr
              ((name* val)
               (if (eq? name name*)
                   val
                   (find-attr rest name)))))))
       (define (update-attrs node old-attrs new-attrs)
         (for-each
          (lambda (attr)
            (match attr
              ((name val)
               (let ((name-str (symbol->string name)))
                 (match (find-attr old-attrs name)
                   ;; No existing attr/listener, add new one.
                   (#f
                    (match val
                      ((? attr-value?)
                       (set-attribute!* node name-str val))
                      ((? procedure?)
                       (add-event-listener!/wrap node name-str val))))
                   ;; Replace old attr or listener with new.
                   (old-val
                    (match val
                      ((? attr-value?)
                       (unless (equal? old-val val)
                         (set-attribute!* node name-str val)))
                      ((? procedure?)
                       (unless (eq? old-val val)
                         (remove-event-listener!/wrap node name-str old-val)
                         (add-event-listener!/wrap node name-str val))))))))))
          new-attrs)
         ;; Delete old attrs that aren't in new.
         (for-each
          (lambda (attr)
            (match attr
              ((name val)
               (let ((name-str (symbol->string name)))
                 (match (find-attr new-attrs name)
                   (#f
                    (match val
                      ((? attr-value?)
                       (remove-attribute! node name-str))
                      ((? procedure?)
                       (remove-event-listener! node name-str val))))
                   (_ #t))))))
          old-attrs))
       (let ((walker (make-tree-walker root)))
         (first-child! walker)
         (let loop ((parent root)
                    (old old)
                    (new new))
           (match old
             (#f
              ;; It's the first render, so clear out whatever might be
              ;; in the actual DOM and render the entire tree.  No
              ;; diffing necessary.
              (let loop ((node (current-node walker)))
                (unless (external-null? node)
                  (let ((next (next-sibling! walker)))
                    (remove! node)
                    (loop next))))
              (append-child! parent (sxml->dom new)))
             ((? string?)
              ;; Replace text node with either a new text node if the
              ;; text has changed, or an element subtree if the text
              ;; has been replaced by an element.
              (unless (and (string? new) (string=? old new))
                (let ((new-node (sxml->dom new)))
                  (replace-with! (current-node walker) new-node)
                  (set-current-node! walker new-node))))
             (((? symbol? old-tag) . old-rest)
              (let-values (((old-attrs old-children)
                            (attrs+children old-rest)))
                (match new
                  ((? string?)
                   ;; Old node was an element, but the new node is a
                   ;; string, so replace the element subtree with a
                   ;; text node.
                   (let ((new-text (make-text-node new)))
                     (replace-with! (current-node walker) new-text)
                     (set-current-node! walker new-text)))
                  (((? symbol? new-tag) . new-rest)
                   (let-values (((new-attrs new-children)
                                 (attrs+children new-rest)))
                     (cond
                      ;; The element tag is the same, so modify the
                      ;; inner contents of the element if necessary.
                      ((eq? old-tag new-tag)
                       (let ((parent (current-node walker)))
                         (update-attrs parent old-attrs new-attrs)
                         (first-child! walker)
                         (let child-loop ((old old-children)
                                          (new new-children))
                           (match old
                             (()
                              ;; The old child list is empty, so
                              ;; diffing stops here.  All remaining
                              ;; children in the new list are fresh
                              ;; elements that need to be added.
                              (for-each
                               (lambda (new)
                                 (append-child! parent (sxml->dom new)))
                               new))
                             ((old-child . old-rest)
                              (match new
                                ;; The new child list is empty, so any
                                ;; remaining children in the old child
                                ;; list need to be removed, including
                                ;; the current one.
                                (()
                                 (let rem-loop ((node (current-node walker)))
                                   (unless (external-null? node)
                                     (let ((next (next-sibling! walker)))
                                       (remove! node)
                                       (rem-loop next)))))
                                ;; Recursively diff old and new child
                                ;; elements.
                                ((new-child . new-rest)
                                 (loop parent old-child new-child)
                                 (next-sibling! walker)
                                 (child-loop old-rest new-rest))))))
                         (set-current-node! walker parent)))
                      ;; New element tag is different than the old
                      ;; one, so replace the entire element subtree.
                      (else
                       (replace-with! (current-node walker)
                                      (sxml->dom new)))))))))))))

     (define (delq item lst)
       (match lst
         (() '())
         ((x . rest)
          (if (eq? x item)
              rest
              (cons x (delq item rest))))))

     ;; Tasks:
     (define-record-type <task>
       (make-task name done?)
       task?
       (name task-name)
       (done? task-done? set-task-done!))

     (define *tasks* '())

     (define (add-task! task)
       (set! *tasks* (cons task *tasks*)))

     (define (remove-task! task)
       (set! *tasks* (delq task *tasks*)))

     (define (template)
       (define (task-template task)
         `(li (input (@ (type "checkbox")
                        ;; Toggle done? flag on click.
                        (change ,(lambda (event)
                                   (let* ((checkbox (event-target event))
                                          (checked? (element-checked? checkbox)))
                                     (set-task-done! task checked?)
                                     (render))))
                        ;; Check the box if task is done.
                        (checked ,(task-done? task))))
              (span (@ (style "padding: 0 1em 0 1em;"))
                    ;; Strikethrough if task is done.
                    ,(if (task-done? task)
                         `(s ,(task-name task))
                         (task-name task)))
              (a (@ (href "#")
                    ;; Remove task on click.
                    (click ,(lambda (event)
                              (remove-task! task)
                              (render))))
                 "remove")))
       `(div
         (h2 "Tasks")
         ;; Tasks are stored in reverse order.
         (ul ,@(map task-template (reverse *tasks*)))
         (input (@ (id "new-task")
                   (placeholder "Write more Scheme")))
         ;; Add new task on click
         (button (@ (click ,(lambda (event)
                              (let* ((input (get-element-by-id "new-task"))
                                     (name (element-value input)))
                                (unless (string=? name "")
                                  (add-task! (make-task name #f))
                                  (set-element-value! input "")
                                  (render))))))
                 "Add task")))

     (define *current-vdom* #f)

     (define (render)
       (let ((new-vdom (template)))
         (virtual-dom-render (document-body) *current-vdom* new-vdom)
         (set! *current-vdom* new-vdom)))

     (render)))

(call-with-output-file "todo.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))
