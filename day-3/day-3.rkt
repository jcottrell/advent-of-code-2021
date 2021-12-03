#lang racket

(require awesome-list
         threading)

(define test-input '("00100"
                     "11110"
                     "10110"
                     "10111"
                     "10101"
                     "01111"
                     "00111"
                     "11100"
                     "10000"
                     "11001"
                     "00010"
                     "01010"))

;; gamma epsilon
(define initial '("" ""))

(define (string-split-character some-string)
  (filter (lambda (character-string)
            (> (string-length character-string) 0))
          (string-split some-string "")))

(define (count-bits bits)
  (foldl (lambda (bit bit-count)
           (list (+ (first  bit-count) (if (equal? "0" bit) 1 0))
                 (+ (second bit-count) (if (equal? "1" bit) 1 0))))
         '(0 0)
         bits))

;(count-bits '("0" "1" "1" "1" "1" "0" "0" "1" "1" "1" "0" "0"))
;; '(5 7)

(define (binary-string->decimal binary-string)
  (string->number binary-string 2))

;(string->number "10110" 2)

(define (parse-and-multiply-gamma-epsilon binary-strings)
  (~>> binary-strings
       (map string-split-character)
       transpose
       (map count-bits)
       (foldl (lambda (pair gamma-epsilon)
                (let ([gamma           (first  gamma-epsilon)]
                      [epsilon         (second gamma-epsilon)]
                      [number-of-zeros (first pair)]
                      [number-of-ones  (second pair)])
                  (list (string-append gamma   (if (> number-of-zeros number-of-ones) "0" "1"))
                        (string-append epsilon (if (< number-of-zeros number-of-ones) "0" "1")))))
              initial)
       (map binary-string->decimal)
       (apply *)))
;(parse-and-multiply-gamma-epsilon test-input)
;; 198
;(parse-and-multiply-gamma-epsilon (file->lines "input.txt"))
;
;
;; Part 2
;;
(define (debug x) (displayln x) x)
(define (filter-to-popularity pop-fun tie-breaker list-of-binaries)
  (let* ([binary-vectors (map list->vector list-of-binaries)])
    (letrec ([narrow-down (lambda (current-index remaining-vectors)
                            (cond [(= 1 (length remaining-vectors)) (~>> (first remaining-vectors)
                                                                         vector->list
                                                                         (apply string-append)
                                                                         binary-string->decimal)]
                                  [else (let* ([countable-list    (map vector->list remaining-vectors)]
                                               [popularity-count  (~>> remaining-vectors
                                                                       (map vector->list)
                                                                       transpose
                                                                       (map count-bits)
                                                                       (list-ref _ current-index))]
                                               [zeros-count       (first  popularity-count)]
                                               [ones-count        (second popularity-count)]
                                               [filter-keeper     (cond [(= zeros-count ones-count) tie-breaker]
                                                                        [(pop-fun zeros-count ones-count) "1"]
                                                                        [else "0"])])
                                          (letrec ([vector-index-matches-keeper
                                                    (lambda (binary-vector)
                                                      (equal? filter-keeper
                                                              (vector-ref binary-vector
                                                                          current-index)))])
                                            (narrow-down (add1 current-index)
                                                         (filter vector-index-matches-keeper remaining-vectors))))]))])
      (narrow-down 0 binary-vectors))))
;(filter-to-popularity < "1" (map string-split-character test-input))
(define (parse-and-multiply-oxygen-co2 lines)
  (let ([list-of-binary-list (map string-split-character lines)])
    (* (filter-to-popularity < "1" list-of-binary-list)
       (filter-to-popularity > "0" list-of-binary-list))))
;(parse-and-multiply-oxygen-co2 test-input)
;(parse-and-multiply-oxygen-co2 (file->lines "input.txt"))
;
;(define t-pop-fun <)
;(define t-tie-breaker "1")
;(define t-list-of-binaries (map string-split-character test-input))
;(define t-binary-vectors (map list->vector t-list-of-binaries))
;(define t-current-index 0)
;(define t-remaining-vectors t-binary-vectors)
;;; "first"
;(define l-popularity-count (~>> t-remaining-vectors
;                                (map vector->list)
;                                transpose
;                                (map count-bits)
;                                (list-ref _ t-current-index)))
;(define l-zeros-count (first l-popularity-count))
;(define l-ones-count (second l-popularity-count))
;(define l-filter-keeper (cond [(= l-zeros-count l-ones-count) t-tie-breaker]
;                                [(t-pop-fun l-zeros-count l-ones-count) "1"]
;                                [else "0"]))
;(define vector-index-matches-keeper
;    (lambda (binary-vector)
;      (equal? l-filter-keeper
;              (vector-ref binary-vector
;                          t-current-index))))
;;(define t-remaining-vectors (filter vector-index-matches-keeper t-remaining-vectors))
;;(define t-current-index (add1 t-current-index))
