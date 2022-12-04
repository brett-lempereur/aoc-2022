#lang racket/base

(require racket/function)
(require racket/list)
(require racket/sequence)

;; Data structures
(struct assignment (a b) #:transparent)

;; Parsing
(define (input->assignments filename)
  (define (line->assignments line)
    (define match (regexp-match #px"(\\d+)-(\\d+),(\\d+)-(\\d+)" line))
    (define match-numbers (map string->number (drop match 1)))
    (list (apply assignment (take match-numbers 2)) (apply assignment (drop match-numbers 2))))
  (define input-lines (in-lines (open-input-file filename)))
  (map line->assignments (sequence->list input-lines)))

;; Return the region covered by two assignments, or #f if two
;; assignments do not overlap.
(define (overlap p q)
  (define ma (max (assignment-a p) (assignment-a q)))
  (define mb (min (assignment-b p) (assignment-b q)))
  (if (<= ma mb) (assignment ma mb) #f))

;; Holds if one of two assignments completely contains the other.
(define (contained p q)
  (define pq (overlap p q))
  (and pq (or (equal? pq p) (equal? pq q))))

;; Solutions
(define input-assignments (input->assignments "data/input.txt"))
(printf "Number of assignment pairs where one fully contains another: ~a\n"
        (length (filter identity (map (curry apply contained) input-assignments))))
(printf "Number of assignment pairs with any overlap: ~a\n"
        (length (filter identity (map (curry apply overlap) input-assignments))))
