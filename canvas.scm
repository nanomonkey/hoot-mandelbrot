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
     (define-foreign make-element
       "document" "createElement"
       (ref string) -> (ref null extern))
     (define-foreign make-text-node
       "document" "createTextNode"
       (ref string) -> (ref null extern))
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

     ;; Canvas API:
     (define-foreign get-context
       "canvas" "getContext"
        (ref null extern) -> (ref null extern))
     (define-foreign fill-rect
       "canvas" "fillRect"
       (ref null extern) i32 i32 i32 i32 -> none)
     (define-foreign fill-style 
       "canvas" "fillStyle"
       (ref null extern) (ref string) -> (ref null extern)) ;;should the return value be none?
     (define-foreign cursor-position
       "canvas" "getCursorPosition"
       (ref null extern) (ref null extern) -> (ref null extern)) ;; returns array [x,y]

     (define range
       (lambda (n . m)
         (let
             ((n (if (null? m) 0 n)) (m (if (null? m) n (car m))))
           (cond
            ((= n m) (list n))
            (else (cons n (range ((if (< n m) + -) n 1) m)))))))

     (define (draw-pixel ctx x y h)
       (let ((style (if (= 0 h) "hsl(0, 100%, 0%)"
                        (string-append "hsl(" (number->string h)  ", 100%, 50%)"))))
             (fill-style ctx style)
             (fill-rect ctx x y 1 1)))
     
     (define (hue level)
       (if (positive? level) (floor (* 359 (/ level 80))) 0))

     (define (iteration a b)
       (let loop ((x 0) (y 0) (iter 80))
         (if (and (< (+ (* x x) (* y y)) 4) (> iter 0))
             (let ((xtemp (+ (- (* x x) (* y y)) a)))
               (loop xtemp 
                     (+ (* 2 x y) b)
                     (1- iter)))
             iter)))

     (define (mandelbrot ctx Px Py scale)
       (for-each 
        (lambda (y)
          (let ((y0 (+ (/ (- y 300.0) 300 scale) Py)))
            (for-each 
             (lambda (x)
               (let ((x0 (+ (/ (- x 400.0) 400 scale) Px)))
                 (draw-pixel ctx x y (hue (iteration x0 y0))))) 
             (range 800))))
        (range 600)))
     
     
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
                             (((? symbol? name) (? string? val))
                              (set-attribute! elem
                                              (symbol->string name)
                                              val))
                             (((? symbol? name) (? procedure? proc))
                              (add-event-listener! elem
                                                   (symbol->string name)
                                                   (procedure->external proc)))))
                         attrs)
               (add-children children))
              (children (add-children children)))
            elem))))
     
     (define *zoom* 1)
     (define *x* -0.5)
     (define *y* 0)

     (define (controls)
       `(div (@ (id "container"))
             (label (@ (for "x")) "X: ")
             (input (@ (id "x")
                       (type "number") (value ,(number->string *x*)) (step "0.001")
                       (min "-2.00") (max "0.47")
                       (change ,(lambda (event) 
                                  (set! *x* 
                                        (string->number 
                                         (element-value (get-element-by-id "x"))))))))
             (label (@ (for "y")) "Y: ")
             (input (@ (id "y")
                       (type "number") (value ,(number->string *y*)) (step "0.001")
                       (min "-1.12") (max "1.12")
                       (change ,(lambda (event) 
                                  (set! *y* 
                                        (string->number 
                                         (element-value (get-element-by-id "y"))))))))
             (label (@ (for "zoom")) "Zoom: ")
             (input (@ (id "zoom")
                       (type "number") (value ,(number->string *zoom*)) 
                       (change ,(lambda (event) 
                                  (set! *zoom* 
                                        (string->number 
                                         (element-value (get-element-by-id "zoom"))))))))
             (button (@ (click ,(lambda (event) (render))))
                     "Render")
             (button (@ (click ,(lambda (event)
                                  (set! *zoom* (1+ *zoom*))
                                  (render))))
                     "Zoom In")))

     (define (render)
       (let* ((canvas (get-element-by-id "myCanvas"))
              (ctx (get-context canvas)))
         (mandelbrot ctx *x* *y* *zoom*)
         (let ((old (get-element-by-id "container")))
           (unless (external-null? old) (remove! old)))
         (append-child! (document-body) (sxml->dom (controls)))))     
     
     (render)))

(call-with-output-file "canvas.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))


