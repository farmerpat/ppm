;; view the resulting ppm file with imagemagick's display command
(define (ref-2d-vect v i j)
  (vector-ref (vector-ref v i) j))

(define (set-2d-vect! v i j value)
  (let ((row (vector-ref v i)))
   (vector-set! v j value)))

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

(define (write-header-to-ppm-port out-port max-x max-y)
  (put-string out-port "P3\n")
  (put-string out-port "# Its the comment\n")
  (put-string out-port (number->string max-x))
  (put-string out-port " ")
  (put-string out-port (number->string max-y))
  (put-string out-port "\n")
  (put-string out-port (number->string (*max-color-component-value*))))

(define (write-color-matrix-to-ppm-port cm out-port max-x max-y)
  (do ((i 0 (add1 i)))
      ((= i max-y) '())
      (put-string out-port "\n")
      (do ((j 0 (add1 j)))
          ((= j max-x) '())
          (let* ((color (ref-2d-vect cm i j))
                 (r (number->string (color 'r)))
                 (g (number->string (color 'g)))
                 (b (number->string (color 'b))))
            (put-string out-port (string-append r " " g " " b " "))))))

;; (file-options) returns and enum-set. investigate that data structure.
;; this assumes cm is the correct format
;; fix that.
(define (color-matrix->ppm cm file-name)
  (let ((max-x (vector-length (vector-ref cm 0)))
        (max-y (vector-length cm))
        (out-port (open-file-output-port file-name (file-options no-fail)
                                         (buffer-mode block) (native-transcoder))))

    (write-header-to-ppm-port out-port max-x max-y)
    (write-color-matrix-to-ppm-port cm out-port max-x max-y)
    (put-string out-port "\n")
    (close-output-port out-port)))

(define file-name "foo.ppm")
(define max-x 100)
(define max-y 100)
(define *max-color-component-value* (make-parameter 255))
(define color-matrix (make-vector max-y (make-vector max-x (make-color 30 65 9))))

(color-matrix->ppm color-matrix file-name)
