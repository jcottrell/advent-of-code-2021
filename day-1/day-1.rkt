#lang racket

(require threading)

(define (get-number-of-increases depths)
  (letrec ([count-increases (lambda (remaining-depths count)
                              (cond [(null? remaining-depths) count]
                                    [else (let* ([current (car remaining-depths)]
                                                 [rest    (cdr remaining-depths)]
                                                 [next    (if (> (length rest) 0)
                                                              (cadr remaining-depths)
                                                              null)]
                                                 [increment (if (and (not (empty? next)) (< current next)) 1 0)])
                                            (count-increases rest (+ count increment)))]))])
    (count-increases depths 0)))
(define test-input '(199 200 208 210 200 207 240 269 260 263))
;(get-number-of-increases test-input)
;; 7
;(get-number-of-increases (map string->number (file->lines "input.txt")))
;; 1502
;
;
; Part 2
;
(define (get-depth-increases-by-window depths)
  (letrec ([take-at-most (lambda (some-list most-elements-to-take)
                           (cond [(>= (length some-list) most-elements-to-take) (take some-list most-elements-to-take)]
                                 [else some-list]))]
           [spin (lambda (remaining-depths result-list)
                   (cond [(empty? remaining-depths) result-list]
                         [else (spin (rest remaining-depths)
                                     (append result-list
                                             (list (apply + (take-at-most remaining-depths 3)))))]))])
    (get-number-of-increases (spin depths '()))))
;(get-depth-increases-by-window test-input)
;; 5
;(get-depth-increases-by-window (map string->number (file->lines "input.txt")))
;;
;; ARRRRHHHHGGGGGGGGHHHHH !!!!!!!!!!!!!!!!
;; The mess I thought I had to make before looking at the input a second time (the same input!)
(define test-input-part2 '("199  A      "
                           "200  A B    "
                           "208  A B C  "
                           "210    B C D"
                           "200  E   C D"
                           "207  E F   D"
                           "240  E F G  "
                           "269    F G H"
                           "260      G H"
                           "263        H"))
#;(define (depths->windows depths)
  (letrec ([to-window (lambda (current-letter current-number)
                        (list depth letters))])
    (to-windows )))
;goal test-input to:
;(lines-list->number-with-keys by-lines)
(define by-lines (map string-split test-input-part2))
(define (lines-list->number-with-keys lines-list)
  (map (lambda (line)
       (cons (string->number (first line)) (rest line)))
     lines-list))
(define (numbers-with-keys->formatted-hash number-with-keys)
  (foldl (lambda (line groups)
           (let* ([depth   (first line)]
                  [windows (rest line)])
             (foldl (lambda (window groups)
                      (cond [(empty? window) groups]
                            [else (hash-set groups
                                            window
                                            (cons depth (hash-ref groups window '())))]))
                    groups
                    windows)))
         (make-immutable-hash)
         number-with-keys))
(define (formatted-hash->increase-count formatted-hash)
  (get-number-of-increases (hash-map formatted-hash
                                     (lambda (window depths)
                                       (apply + depths))
                                     #t)))
(define (get-number-of-increases-by-windows lines-list)
  (~>> lines-list
       (map string-split)
       lines-list->number-with-keys
       numbers-with-keys->formatted-hash
       formatted-hash->increase-count))
;(get-number-of-increases-by-windows test-input-part2)
;; 5
;(get-number-of-increases-by-windows (file->lines "input-part2.txt"))
