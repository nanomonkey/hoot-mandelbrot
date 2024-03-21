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
     
     (define range
       (lambda (n . m)
         (let
             ((n (if (null? m) 0 n)) (m (if (null? m) n (car m))))
           (cond
            ((= n m) (list n))
            (else (cons n (range ((if (< n m) + -) n 1) m)))))))

     (define (draw-pixel ctx x y h)
       (let ((style (string-append "hsl(" (number->string h)  ", 100%, 50%)")))
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
          (let ((y0 (+ (/ (- y 120.0) 120 scale) Py)))
            (for-each 
             (lambda (x)
               (let ((x0 (+ (/ (- x 160.0) 160 scale) Px)))
                 (draw-pixel ctx x y (hue (iteration x0 y0))))) 
             (range 320))))
        (range 240)))
    
     (define (gradient ctx)
       (for-each 
        (lambda (y)
          (for-each  
           (lambda (x)
             (draw-pixel ctx x y x)) 
           (range 320)))
        (range 240)))

     (define (render)
       (let* ((canvas (get-element-by-id "myCanvas"))
              (ctx (get-context canvas)))
         ;(mandelbrot ctx -0.5 0 1)
         (mandelbrot ctx -0.53 -0.61 11)
         ;;(gradient ctx)
         ))     

       (render)))

(call-with-output-file "canvas.wasm"
  (lambda (port)
    (put-bytevector port (assemble-wasm (compile src)))))

