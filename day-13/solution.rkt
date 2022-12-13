#lang racket/base

(require racket/function)
(require racket/list)
(require racket/sequence)
(require racket/string)

;; Read the input into a list of pairs.
(define (read-input filename)
  (define (read-line line)
    (eval (read (open-input-string
                 (foldl (λ (transform a) (string-replace a (car transform) (cdr transform)))
                        line
                        '(("[" . "(list ") ("]" . ")") ("," . " ")))))))
  (reverse (let loop ([acc '()] [lines (sequence->list (in-lines (open-input-file filename)))])
             (if (empty? lines)
                 acc
                 (loop (cons (map read-line (take lines 2)) acc)
                       (drop lines (min (length lines) 3)))))))

;; Holds if two lists are sorted according to the rule of the puzzle.
(define (lists-sorted? a b)
  (define (number-or-list->list q)
    (if (list? q) q (list q)))
  (let loop ([a a] [b b])
    (cond
      [(or (and (empty? a) (empty? b)) (empty? a)) #t]
      [(empty? b) #f]
      [else
       (let ([ha (car a)] [hb (car b)])
         (cond
           [(or (list? ha) (list? hb))
            (lists-sorted? (number-or-list->list ha) (number-or-list->list hb))]
           [(= ha hb) (loop (cdr a) (cdr b))]
           [else (< ha hb)]))])))

;; Return the sum of the indices of pairs that are sorted.
(define (sorted-pair-index-sum p)
  (define status (map (curry apply lists-sorted?) p))
  (foldl (λ (i s a) (if s (+ a i 1) a)) 0 (range (length status)) status))

;; Return the product of the indices of the divider packets.
(define (divider-packet-index-product p)
  (define packets (append (foldl append '() p) '(((2)) ((6)))))
  (define sorted (sort packets lists-sorted?))
  (* (+ 1 (index-of sorted '((2)))) (+ 1 (index-of sorted '((6))))))

;; Output the solutions.
(define input-lists (read-input "data/input.txt"))
(printf "Solution one: ~a\n" (sorted-pair-index-sum input-lists))
(printf "Solution two: ~a\n" (divider-packet-index-product input-lists))
