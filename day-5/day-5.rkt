#lang racket

(require threading)

;; https://www.reddit.com/r/Racket/comments/rbuu5d/multidimensional_vectors/
;; either of these works
#;(define (make-2d-vector columns rows fill)
  (build-vector rows
                (lambda (row)
                  (make-vector columns fill))))
(define (make-2d-vector columns rows fill)
  (define result! (make-vector rows))
  (for ([x (range rows)])
    (vector-set! result! x (make-vector columns fill)))
  result!)

(define test-input '("0,9 -> 5,9"
                     "8,0 -> 0,8"
                     "9,4 -> 3,4"
                     "2,2 -> 2,1"
                     "7,0 -> 7,4"
                     "6,4 -> 2,0"
                     "0,9 -> 2,9"
                     "3,4 -> 1,4"
                     "0,0 -> 8,8"
                     "5,5 -> 8,2"))
(struct point (x y) #:transparent)
(struct line  (a b) #:transparent)

#;(define (comma-separated-pair->point csp)
  (let* ([split-pair (string-split (string-trim csp) ",")]
         [number-pair (map string->number split-pair)])
    (point (first number-pair) (second number-pair))))

(define file-lines->point-lines
  (letrec ([comma-separated-pair->point
            (lambda (csp)
              (let* ([split-pair (string-split (string-trim csp) ",")]
                     [number-pair (map string->number split-pair)])
                (point (first number-pair) (second number-pair))))]
           [point-pair->line (lambda (point-pair)
                               (line (first point-pair) (second point-pair)))])
    (lambda~>>
     (map (lambda~>>
         (string-split _ "->")
         (map comma-separated-pair->point)))
     (map point-pair->line))))
;(file-lines->point-lines test-input)
(define test-lines (file-lines->point-lines test-input))

(define (get-greatest-x-and-y lines)
  (let ([all-points (flatten (map (lambda (line)
                                  (list (line-a line) (line-b line))) lines))])
  (letrec ([spin (lambda (remaining-points max-x max-y)
                   (cond [(empty? remaining-points) (values max-x max-y)]
                         [else (let* ([current-point (first remaining-points)]
                                      [x             (point-x current-point)]
                                      [y             (point-y current-point)])
                                 (spin (rest remaining-points)
                                       (max max-x x)
                                       (max max-y y)))]))])
    (spin all-points 0 0))))
;(get-greatest-x-and-y (file-lines->point-lines test-input))
(define (add-line-to-grid line grid)
  (let* ([mid-points (get-mid-points line)])
    (for ([pt mid-points])
      (let* ([x           (point-x pt)]
             [y           (point-y pt)]
             [prior-value (vector-ref (vector-ref grid y) x)]
             [next-value  (if (equal? "." prior-value) 1 (add1 prior-value))])
        ;(displayln (string-append (number->string x) ", " (number->string y) ": " (number->string next-value)))
        (vector-set! (vector-ref grid y) x next-value)))))
(define (repeat what times)
  (letrec ([spin (lambda (remaining-times repeating-list)
                   (cond [(> 0 remaining-times) repeating-list]
                         [else (spin (sub1 remaining-times)
                                     (cons what repeating-list))]))])
    (spin times '())))
(define (get-mid-points line)
  (let* ([start-x (point-x (line-a line))]
         [start-y (point-y (line-a line))]
         [end-x   (point-x (line-b line))]
         [end-y   (point-y (line-b line))]
         [x-difference (- end-x start-x)]
         [y-difference (- end-y start-y)]
         [x-direction (if (zero? x-difference) 0 (/ x-difference (abs x-difference)))]
         [y-direction (if (zero? y-difference) 0 (/ y-difference (abs y-difference)))])
    (map (lambda (x y)
         (point x y))
       (cond [(zero? x-difference) (repeat start-x (abs y-difference))]
             [else (range start-x (+ end-x x-direction) x-direction)])
       (cond [(zero? y-difference) (repeat start-y (abs x-difference))]
             [else (range start-y (+ end-y y-direction) y-direction)]))))
;(get-mid-points (second test-lines))
;(vector-ref (vector 0 1 2 3) 1)
;(define test-vector-10s (vector->immutable-vector (make-vector 10 (make-vector 10 "."))))
;(vector-ref (vector-ref test-vector-10s 5) 2)
(define (point-lines->grid lines sifter)
  (let-values ([(max-x max-y) (get-greatest-x-and-y lines)])
    (let ([grid (make-2d-vector (add1 max-x) (add1 max-y) ".")]
          [filtered-lines (sifter lines)])
      ;(displayln (number->string (length filtered-lines)))
      (for ([line filtered-lines])
        (add-line-to-grid line grid))
      grid)))

(define (filter-lines-by-points-fun fun)
  (lambda (lines)
    (filter (lambda (line)
            (let ([start (line-a line)]
                  [end   (line-b line)])
              (fun start end)))
          lines)))
(define (horizontal? start end)
  (= (point-y start)
     (point-y end)))
(define (vertical? start end)
  (= (point-x start)
     (point-x end)))
(define (not-diagonal? start end)
  (or (horizontal? start end)
      (vertical?   start end)))
;; grid picture:
;(point-lines->grid test-lines (filter-lines-by-points-fun not-diagonal?))
(define vertical-or-horizontal? (filter-lines-by-points-fun not-diagonal?))
(define (count-overlapping-lines compare grid)
  (let ([rows    (vector-length grid)]
        [columns (vector-length (vector-ref grid 0))])
    ;(display (string-append (number->string columns) "," (number->string rows)))
    (foldl (lambda (row soFar)
             (+ soFar
                (foldl (lambda (column soFar)
                         (let* ([box   (vector-ref (vector-ref grid row) column)]
                                [value (if (equal? "." box) 0 box)])
                           (+ soFar (compare value))))
                       0
                       (range 0 columns))))
           0
           (range 0 rows))))
(define (at-least n)
  (lambda (x)
    (if (>= x n) 1 0)))
(define count-overlapping-vertical-or-horizontal-lines-2-or-more
  (lambda~>> file-lines->point-lines
             (point-lines->grid _ vertical-or-horizontal?)
             (count-overlapping-lines (at-least 2) _)))
;(count-overlapping-vertical-or-horizontal-lines-2-or-more test-input)
;; 5
;(count-overlapping-vertical-or-horizontal-lines-2-or-more (file->lines "input.txt"))
;; 5294
;
;; Part 2
(define (horizontal-vertical-diagonal? start end)
  (or (horizontal? start end)
      (vertical?   start end)
      (= (abs (- (point-x start) (point-x end)))
         (abs (- (point-y start) (point-y end))))))
(define valid-horizontal-vertical-diagonal-line?
  (filter-lines-by-points-fun horizontal-vertical-diagonal?))
(define (count-overlapping-lines-2-or-more file-lines)
  (~>> file-lines
       file-lines->point-lines
       (point-lines->grid _ valid-horizontal-vertical-diagonal-line?)
       (count-overlapping-lines (at-least 2) _)))
(define t1 (make-2d-vector 5 4 "."))
;; grid picture:
;(point-lines->grid test-lines (filter-lines-by-points-fun horizontal-vertical-diagonal?))
;
;(count-overlapping-lines-2-or-more test-input)
;; 12
;(count-overlapping-lines-2-or-more (file->lines "input.txt"))
;; 21698
