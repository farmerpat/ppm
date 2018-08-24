(import (chezscheme))

;; (darken cm i j)
;; (darken-row cm i)
;; (darken-col cm j)
;; (draw-line cm x1 y1 x2 y2 color)
;; (draw-rect...
;; (fill-rect...
;; ...etc
;; can have (library (ppm color-matrix))
;; in another file...

;; view the resulting ppm file with imagemagick's display command
(define (ref-2d-vect v i j)
  (vector-ref (vector-ref v i) j))

(define (set-2d-vect! v i j value)
  (let ((row (vector-ref v i)))
   (vector-set! row j value)))

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

(define (set-pt-color! cm row col color)
  (if (color? color)
    (set-2d-vect! cm row col color)
    (error "set-pt-color!" "invalid color" color)))

;; row = i == #(rows #(columns))
(define (cm-num-rows cm)
  (vector-length cm))

(define (cm-num-cols cm)
  (vector-length (vector-ref cm 1)))

(define (set-row-color! cm row color)
  (vector-set! cm row (make-vector (cm-num-cols cm) color)))

(define (set-col-color! cm col color)
  (do ((i 0 (add1 i)))
    ((= i (cm-num-rows cm)))
    (set-pt-color! cm i col color)))

(define (set-row-range-color! cm r0 rn color)
  (if (and (number? r0) (number? rn) (>= r0 0) (>= rn 0) (>= rn r0))
    (do ((i r0 (add1 i)))
      ((= i rn) '())
      (set-row-color! cm i color))
    (error "invalid start and/or end rown numbers" r0 rn)))

(define (set-col-range-color! cm c0 cn color)
  (if (> c0 cn)
    '()
    (begin
      (set-col-color! cm c0 color)
      (set-col-range-color! cm (add1 c0) cn color))))

;; masks:
;; can generate a mask (a 2d-vecotr of 1s and 0s
;; the same size as some target cm
;; and the masks can be paired with transformation functions
;; that take a color as an argument and returns a new color.
;; really, the transformer could also use the row/col data
;; as input if it wanted to...

;; layers:
;; if we pair masks and color matrices together
;; (or just combine the idea and have masked-matrices
;; that have #f or a color as values),
;; we can build images in layers by applying
;; layers to a blank color matrix (a canvas?)
;; in order from the lowest-ranked layer to
;; the highest. when a layer with a higher rank is
;; being written to the canvas, if it is to write
;; a #f it can do nothing or it can clobber any
;; existing value with #f. That could be an option.

;; palettes:
;; can be as simple as a list of colors.
;; a palette could be a procedure
;; that takes tags as arguments
;; can ask it for a random color.

;; might want to save palettes between
;; sessions. could look for a hidden
;; file with serialized data. this
;; is starting to feel more like M$-PAINT meets GIMP
;; e.g. a scriptable m$-paint.

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

(set-row-color! color-matrix 5 (make-color 100 220 60))
(set-col-range-color! color-matrix 10 70 (make-color 80 220 150))

(color-matrix->ppm color-matrix file-name)
