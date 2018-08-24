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
  (vector-set! (vector-ref v i) j value))

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

(define get-color-at ref-2d-vect)

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

(define file-name "foo.ppm")
(define max-x 400)
(define max-y 400)
(define *max-color-component-value* (make-parameter 255))

;; obviously make the following a proc or macro
;; the following fails...
;(define color-matrix (make-vector max-y (make-vector max-x (make-color 255 255 255))))
;; so...
(define color-matrix (make-vector max-y))

(do ((i 0  (add1 i)))
  ((= i max-y))
  (vector-set! color-matrix i (make-vector max-x (make-color 255 255 255))))

;; painter:
;; a painter is a procedure whose input is:
;; cm, row, col.
;; and it computes the "next" row,col.
;; it sounds like more of a path or a walk
;; through the cm. a walk would have seed
;; coordinates, a procuedure for computing
;; the 'next' coordinates, and way to handle
;; cases where the next coords are out of bounds.
;; using walks might be easier if we generated
;; "instances" of them by passing the the cm
;; then the instance could be used like
;; (set! current-pair (my-walk current-pair))
;; and the walk would return #f when there
;; are no more points.
;; these walks then are strictly building
;; blocks in the sense that they
;; must be combined with procedures
;; that perform some operation on the
;; points encountered along the path being
;; traversed.
;; we could differentiate between procedures
;; that use walks that are destrcutive and
;; those that are not.
;; in other words, a walk can be thought
;; of as a way to grab a set of points
;; or as a way to set the values of a set
;; of points, or whatever.  In a sense,
;; the walk represents an iterator-esque
;; interface to a set of points.
;; this can't backtrace once it goes out of bounds
;; e.g. it just returns #f when it steps OOB
;; could add back-tracing (recoverable?) transformers later

;; sort of extending that idea one step further,
;; we could use a similar abstraction for functions
;; that transform points with respect to other points.
;; for instance, every third point, we actually want
;; to do something to the point +1,+2 away from the
;; current point. or maybe that doesn't make sense,
;; and the transformer should have just picked
;; the right point in the first place.

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
(define (make-point-transformer cm row-transformer col-transformer)
  (lambda (pt)
    (let* ((row (car pt))
           (col (cdr pt))
           (new-row (row-transformer row))
           (new-col (col-transformer col))
           (max-row (cm-num-rows cm))
           (max-col (cm-num-cols cm)))
      (if (or (>= new-row max-row) (>= new-col max-col))
        #f
        (cons new-row new-col)))))

(define (make-walk point-transformer row-0 col-0)
  (let ((current-row row-0)
        (current-col col-0))
    (lambda ()
      (let ((new-point (point-transformer (cons current-row current-col))))
        (when new-point
          (set! current-row (car new-point))
          (set! current-col (cdr new-point)))
        new-point))))

(define (take-walk walk cm proc)
  (let ((next-point (walk)))
    (when next-point
      (proc cm next-point)
      (take-walk walk cm proc))))

(define (do-work)
  ;; x/y transformers (here, add1) could be a single function that accepts a pair as an arg.
  (let ((transformer (make-point-transformer color-matrix add1 add1)))
    (take-walk
      (make-walk transformer 0 0)
      color-matrix
      (lambda (cm pt)
        (set-pt-color! cm (car pt) (cdr pt) (make-color 155 0 155))))

    (take-walk
      (make-walk transformer 0 4)
      color-matrix
      (lambda (cm pt)
        (set-pt-color! cm (car pt) (cdr pt) (make-color 0 155 155))))

    (take-walk
      (make-walk transformer 4 0)
      color-matrix
      (lambda (cm pt)
        (set-pt-color! cm (car pt) (cdr pt) (make-color 0 155 155))))))

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

(set-row-color! color-matrix 5 (make-color 100 220 60))
(set-col-range-color! color-matrix 10 70 (make-color 80 220 150))
(do-work)

(color-matrix->ppm color-matrix file-name)
