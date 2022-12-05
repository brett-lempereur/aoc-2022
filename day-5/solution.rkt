#lang racket/base

(require racket/function)
(require racket/list)
(require racket/match)
(require racket/sequence)
(require threading)

;; Data structures
(struct move (n from to) #:transparent)

;; Read the stacks section into a list of stacks.
(define (input->stacks filename)
  (define input-lines
    (take (sequence->list (in-lines (open-input-file filename))) 8))
  (define input-stacks (/ (+ (string-length (car input-lines)) 1) 4))
  (for*/fold ([acc (make-list input-stacks '())])
             ([letters (reverse input-lines)] [i (range input-stacks)])
    (let ([c (string-ref letters (+ 1 (* 4 i)))])
      (if (not (eq? c #\space))
          (list-set acc i (cons c (list-ref acc i)))
          acc))))

;; Read the instructions section into a list of moves.
(define (input->moves filename)
  (define (line->move line)
    (define match (regexp-match #px"move (\\d+) from (\\d+) to (\\d+)" line))
    (match-define (list n from to) (map string->number (drop match 1)))
    (move n (sub1 from) (sub1 to)))
  (define input-lines
    (drop (sequence->list (in-lines (open-input-file filename))) 10))
  (map line->move input-lines))

;; Apply a move to a list of stacks.
(define (with-move m stacks [reversed #t])
  (match-define (move n from to) m)
  (define to-move (take (list-ref stacks from) n))
  (~>
   stacks
   (list-update to (λ (v) (append (if reversed (reverse to-move) to-move) v)))
   (list-update from (λ (v) (drop v n)))))

;; Read the input.
(define stacks (input->stacks "data/input.txt"))
(define moves (input->moves "data/input.txt"))
(printf "Solution one: ~a\n"
        (list->string (map car (foldl with-move stacks moves))))
(printf "Solution two: ~a\n"
        (list->string (map car (foldl (curryr with-move #f) stacks moves))))
