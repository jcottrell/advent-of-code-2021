#lang racket

(require awesome-list
         threading)

(struct square (val marked)
  #:transparent)

;(file->lines "input.txt")

(define test-input '("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
                     ""
                     "22 13 17 11  0"
                     " 8  2 23  4 24"
                     "21  9 14 16  7"
                     " 6 10  3 18  5"
                     " 1 12 20 15 19"
                     ""
                     " 3 15  0  2 22"
                     " 9 18 13 17  5"
                     "19  8  7 25 23"
                     "20 11 10 24  4"
                     "14 21 16 12  6"
                     ""
                     "14 21 17 24  4"
                     "10 16 15  9 19"
                     "18  8 23 26 20"
                     "22 11 13  6  5"
                     " 2  0 12  3  7"))

(define lines->draws
  (lambda~>> first
             (string-split _ ",")
             (map string->number)))
;(lines->draws test-input)
(define test-draws
  '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))

(define (number->square n)
  (square n #f))

;(number->square 4)

(define (card-lines->card card-lines)
  (map (lambda (string-numbers)
       (map (compose number->square string->number) string-numbers))
     (map string-split card-lines)))

(define (lines->cards lines)
  (let ([all-card-lines (filter non-empty-string? (list-tail lines 2))])
    (letrec ([add-next-card (lambda (remaining-card-lines cards)
                              (cond [(empty? remaining-card-lines) cards]
                                    [else (add-next-card (drop remaining-card-lines 5)
                                                         (append cards
                                                                 (list (card-lines->card
                                                                        (take remaining-card-lines 5)))))]))])
      (add-next-card all-card-lines '()))))
;(lines->cards test-input)

(define test-card (card-lines->card '("22 13 17 11  0"
                                      " 8  2 23  4 24"
                                      "21  9 14 16  7"
                                      " 6 10  3 18  5"
                                      " 1 12 20 15 19")))

(define (mark-square card number-to-mark)
  (map (lambda (row)
       (map (lambda (sq)
            (let ([square-value (square-val sq)])
              (cond [(= square-value number-to-mark)
                     (square square-value (= square-value number-to-mark))]
                    [else sq])))
          row))
     card))
;(mark-square test-card 9)
(define horizontal-winner (~>> test-card
                               (mark-square _ 21)
                               (mark-square _ 9)
                               (mark-square _ 14)
                               (mark-square _ 16)
                               (mark-square _ 7)))
(define vertical-winner (~>> test-card
                             (mark-square _ 11)
                             (mark-square _ 4)
                             (mark-square _ 16)
                             (mark-square _ 18)
                             (mark-square _ 15)))
(define (winner? card)
  (let ([columnized-card (transpose card)])
    (letrec ([all-squares-marked? (lambda (squares)
                                    (andmap (lambda (sq)
                                              (square-marked sq))
                                            squares))])
      (or (ormap all-squares-marked? card)
          (ormap all-squares-marked? columnized-card)))))
;(winner? horizontal-winner)
;(winner? vertical-winner)
;(winner? test-card)
(define (get-unmarked-total card)
  (apply + (map (lambda (row)
                (apply + (map (lambda (sq)
                              (cond [(not (square-marked sq)) (square-val sq)]
                                    [else 0]))
                            row)))
              card)))
;(get-unmarked-total horizontal-winner)
;(get-unmarked-total vertical-winner)

(define (get-draw-winner which-winner-fun remaining-draws cards)
  (letrec ([spin (lambda (remaining-draws cards)
    (cond [(empty? remaining-draws) cards]
          [else (let* ([current-draw (first remaining-draws)]
                       [current-cards (map (lambda (card)
                                           (mark-square card current-draw))
                                         cards)]
                       [winning-card  (which-winner-fun current-cards cards current-draw)])
                  (if winning-card
                      (values winning-card current-draw)
                      (spin (rest remaining-draws)
                            current-cards)))]))])
  (spin remaining-draws cards)))

(define (find-first-winner current-cards _ __)
  (findf winner? current-cards))

(define (get-first-winner draws cards)
    (get-draw-winner find-first-winner draws cards))
#;(get-first-winner (lines->draws test-input)
                    (lines->cards test-input))
(define (parse-and-get-final-score-by-first-winner lines)
  (let-values ([(winner last-draw) (get-first-winner (lines->draws lines)
                                                     (lines->cards lines))])
    (* (get-unmarked-total winner)
       last-draw)))
;(parse-and-get-final-score-by-first-winner test-input)
;; 4512
;(parse-and-get-final-score-by-first-winner (file->lines "input.txt"))
;; 39902
;
;; Part 2
(define (find-last-winner current-cards previous-cards current-draw)
  (if (and (andmap winner? current-cards)
           (ormap  winner? previous-cards))
      (mark-square (findf (lambda (card)
               (not (winner? card)))
             previous-cards) current-draw)
      #f))
(define (get-last-winner draws cards)
  (get-draw-winner find-last-winner draws cards))
#;(get-last-winner (lines->draws test-input)
                   (lines->cards test-input))
(define (parse-and-get-final-score-by-last-winner lines)
  (let-values ([(winner last-draw) (get-last-winner (lines->draws lines)
                                                    (lines->cards lines))])
    (* (get-unmarked-total winner)
       last-draw)))
;(parse-and-get-final-score-by-last-winner test-input)
;; 1924
;(parse-and-get-final-score-by-last-winner (file->lines "input.txt"))
;; 26936
