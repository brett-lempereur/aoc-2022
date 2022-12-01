#lang racket/base

(require racket/list)
(require racket/sequence)

(define (file->calories filename)
  (define input-lines (in-lines (open-input-file filename)))
  (define input-numbers (sequence-map string->number input-lines))
  (for/fold ([acc '(0)])
            ([maybe-number input-numbers])
    (if maybe-number
        (cons (+ (car acc) maybe-number) (cdr acc))
        (cons 0 acc))))

(define (top-n-calories calories n) (take (sort calories >) n))
(define (sum calories) (foldl + 0 calories))

;; Compute the total calories held by the top and top three elves.
(define calories-by-elf (file->calories "data/input.txt"))
(printf "Solution 1: ~a\n" (sum (top-n-calories calories-by-elf 1)))
(printf "Solution 2: ~a\n" (sum (top-n-calories calories-by-elf 3)))
