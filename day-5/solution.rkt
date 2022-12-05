#lang racket/base

(require racket/function)
(require racket/list)
(require racket/sequence)
(require racket/string)
(require threading)

;; Data structures
(struct move (n from to) #:transparent)

;; Read the stacks section into a list of stacks.
(define (input->stacks filename)
  (define input-lines (take (sequence->list (in-lines (open-input-file filename))) 8))
  (define input-letters
    (map (λ (x) (~> x (string-replace "[" "") (string-replace "]" "") (string-replace "  " " ")))
         input-lines))
  (for*/fold ([acc (build-list 9 (λ (_) '()))]) ([letters (reverse input-letters)] [i (range 9)])
    (let ([c (string-ref letters (* 2 i))])
      (if (not (eq? c #\space)) (list-set acc i (cons c (list-ref acc i))) acc))))

;; Read the instructions section into a list of moves.
(define (input->moves filename)
  (define (line->move line)
    (define match (regexp-match #px"move (\\d+) from (\\d) to (\\d)" line))
    (define match-numbers (map string->number (drop match 1)))
    (apply move match-numbers))
  (define input-lines (drop (sequence->list (in-lines (open-input-file filename))) 10))
  (map line->move input-lines))

;; Apply a move to a list of stacks.
(define (with-move move stacks [reversed #t])
  (define to-move (take (list-ref stacks (sub1 (move-from move))) (move-n move)))
  (~> stacks
      (list-update (sub1 (move-to move)) (λ (v) (append (if reversed (reverse to-move) to-move) v)))
      (list-update (sub1 (move-from move)) (λ (v) (drop v (move-n move))))))

;; Read the input.
(define stacks (input->stacks "data/input.txt"))
(define moves (input->moves "data/input.txt"))
(printf "Solution one: ~a\n" (list->string (map car (foldl with-move stacks moves))))
(printf "Solution two: ~a\n" (list->string (map car (foldl (curryr with-move #f) stacks moves))))
