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
  (sequence->list
   (sequence-map line->backpack (in-lines (open-input-file filename)))))

(define (common-element sets)
  (car (set->list (foldl set-intersect (car sets) (cdr sets)))))

(define (split-backpack backpack)
  (define-values (a b) (split-at backpack (/ (length backpack) 2)))
  (list (list->set a) (list->set b)))

(define (chunk backpacks n)
  (define-values (a b) (split-at backpacks n))
  (cons a (if (empty? b) '() (chunk b n))))

(define (sum input)
  (sequence-fold + 0 input))

(define backpacks (input->backpacks "data/input.txt"))
(printf "Solution one: ~a\n" (sum (map (compose common-element split-backpack) backpacks)))
(printf "Solution two: ~a\n" (sum (map common-element (chunk backpacks 3))))
