;; view the resulting ppm file with imagemagick's display command
(define (inclusively-bound-by? n lower upper)
  (and (>= n lower)
       (<= n upper)))

(define (exclusively-bound-by? n lower upper)
  (and (> n lower)
       (< n upper)))

(define (valid-rgb? val)
  (inclusively-bound-by? val 0 255))

(define (make-color r g b)
  (if (or (not (valid-rgb? r))
          (not (valid-rgb? g))
          (not (valid-rgb? b)))
      (error "make-color" "invalid r, g, or b value")
      (case-lambda
        (() (list 'color r g b))
        ((component)
         (cond ((eq? component 'r) r)
               ((eq? component 'g) g)
               ((eq? component 'b) b)
               (else #f))))))

(define (color? thing)
  (and (procedure? thing)
       (list? (thing))
       (not (null? (thing)))
       (eq? (car (thing)) 'color)))

(define file-name "foo.ppm")
(define max-x 100)
(define max-y 100)
(define max-color-component-value 255)

(define color-matrix (make-vector max-y (make-vector max-x (make-color 30 65 9))))

;; (file-options) returns and enum-set. learn the data structure.
(define out-port (open-file-output-port file-name (file-options no-fail) (buffer-mode block) (native-transcoder)))

(put-string out-port "P3\n")
(put-string out-port "# Its the comment\n")
(put-string out-port (number->string max-x))
(put-string out-port " ")
(put-string out-port (number->string max-y))
(put-string out-port "\n")
(put-string out-port (number->string max-color-component-value))

;; vector, row, col
(define (ref-2d-vect v i j)
  (vector-ref (vector-ref v i) j))

(define (set-2d-vect! v i j value)
  (let ((row (vector-ref v i)))
   (vector-set! v j value)))

(do ((i 0 (add1 i)))
    ((= i max-y) '())
  (put-string out-port "\n")
  (do ((j 0 (add1 j)))
      ((= j max-x) '())
      (let* ((color (ref-2d-vect color-matrix i j))
             (r (number->string (color 'r)))
             (g (number->string (color 'g)))
             (b (number->string (color 'b))))
       (put-string out-port (string-append r " " g " " b " ")))))

(put-string out-port "\n")
(close-output-port out-port)
