#lang racket

(require threading)

(define directions '("forward 5"
                     "down 5"
                     "forward 8"
                     "up 3"
                     "down 8"
                     "forward 2"))
;; Part 1
;; horizontal, depth
(define initial-h-d '(0 0))

(define (get-multiplied-horizontal-depth directions)
  (~>> directions
       (map string-split)
       (map (lambda (pair) (list (first pair) (string->number (second pair)))))
       (foldl (lambda (pair result)
                (let ([direction  (first   pair)]
                      [num        (second  pair)]
                      [horizontal (first  result)]
                      [depth      (second result)])
                  (cond [(equal? direction "forward") (list (+ num horizontal) depth)]
                        [(equal? direction "down")    (list horizontal (+ depth num))]
                        [(equal? direction "up")      (list horizontal (- depth num))])))
              initial-h-d)
       (apply *)))
;(get-multiplied-horizontal-depth directions)
;; 150
;(get-multiplied-horizontal-depth (file->lines "input.txt"))
;
;
;
;; Part 2
(define initial-h-d-a '(0 0 0))
(define (multiply-first-two ls)
  (* (first ls) (second ls)))
(define (get-multiplied-horizontal-depth-corrected directions)
  (~>> directions
       (map string-split)
       (map (lambda (pair) (list (first pair) (string->number (second pair)))))
       (foldl (lambda (pair result)
                (let ([direction  (first   pair)]
                      [num        (second  pair)]
                      [horizontal (first  result)]
                      [depth      (second result)]
                      [aim        (third  result)])
                  (cond [(equal? direction "forward") (list (+ horizontal num) (+ depth (* aim num)) aim)]
                        [(equal? direction "down")    (list horizontal depth (+ aim num))]
                        [(equal? direction "up")      (list horizontal depth (- aim num))])))
              initial-h-d-a)
       multiply-first-two))
;(get-multiplied-horizontal-depth-corrected directions)
;; 900
;(get-multiplied-horizontal-depth-corrected (file->lines "input.txt"))
;; 1765720035
