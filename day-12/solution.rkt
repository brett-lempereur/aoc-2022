#lang racket/base

(require racket/function)
(require racket/list)
(require racket/port)
(require racket/string)

(require graph)

;; Data structures
(struct cell (x y height is-start? is-target?) #:transparent)

;; Read the input file to a directed unweighted graph.
(define (read-graph filename w h)
  (define (char->cell c i)
    (define is-start? (eq? c #\S))
    (define is-target? (eq? c #\E))
    (cell (modulo i w)
          (floor (/ i w))
          (cond
            [is-start? 0]
            [is-target? 25]
            [else (- (char->integer c) 97)])
          is-start?
          is-target?))
  (define (cell-at x y)
    (vector-ref input-cells (+ (* y w) x)))
  (define (cell-edges x y)
    (map (λ (n) (list (cell-at x y) n))
         (for/fold ([acc '()]) ([d '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))])
           (let ([nx (+ x (car d))] [ny (+ y (cdr d))])
             (if (or (< nx 0)
                     (< ny 0)
                     (>= nx w)
                     (>= ny h)
                     (> (cell-height (cell-at nx ny)) (+ 1 (cell-height (cell-at x y)))))
                 acc
                 (cons (cell-at nx ny) acc))))))
  (define input-string (string-replace (port->string (open-input-file filename)) "\n" ""))
  (define input-cells
    (list->vector (map char->cell (string->list input-string) (range (string-length input-string)))))
  (unweighted-graph/directed (foldl append '() (for*/list ([x (range w)] [y (range h)]) (cell-edges x y)))))

;; Find the shortest possible path from nodes for which some predicate
;; holds.
(define (shortest-path g p)
  (define target-node (car (filter cell-is-target? (get-vertices g))))
  (define source-nodes (filter p (get-vertices g)))
  (define paths (filter identity (map (λ (v) (fewest-vertices-path g v target-node)) source-nodes)))
  (sub1 (length (car (sort paths (λ (a b) (< (length a) (length b))))))))

;; Read the graph and output the solutions.
(define input-graph (read-graph "data/input.txt" 143 41))
(printf "Solution one: ~a\n" (shortest-path input-graph cell-is-start?))
(printf "Solution two: ~a\n" (shortest-path input-graph (λ (v) (= (cell-height v) 0))))
