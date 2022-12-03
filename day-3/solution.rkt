#lang racket/base

(require racket/list)
(require racket/set)
(require racket/sequence)

(define (char->priority c)
  (if (char-lower-case? c)
      (- (char->integer c) 96)
      (- (char->integer c) 38)))

(define (input->backpacks filename)
  (define (line->backpack line)
    (map char->priority (string->list line)))
  (sequence-map line->backpack (in-lines (open-input-file filename))))

(define (duplicate-item-priority backpack)
  (define-values (l r) (split-at backpack (/ (length backpack) 2)))
  (car (set->list (set-intersect (list->set l) (list->set r)))))

(define (badge-priority a b c)
  (car (set->list (set-intersect (list->set a) (list->set b) (list->set c)))))

(define (badge-priorities backpacks)
  (define-values (head tail) (split-at backpacks 3))
  (define priority (apply badge-priority head))
  (+ priority (if (empty? tail) 0 (badge-priorities tail))))

(define (sum input)
  (sequence-fold + 0 input))

(printf "Solution to part one: ~a\n"
        (sum (sequence-map duplicate-item-priority (input->backpacks "data/input.txt"))))
(printf "Solution to part two: ~a\n"
        (badge-priorities (sequence->list (input->backpacks "data/input.txt"))))
