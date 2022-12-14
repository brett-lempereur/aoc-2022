#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/match)
(require racket/vector)

;; Data structures.
(struct grid (cells width height))

;; Convert a list of lists to a grid, the lists must all be of equal
;; length.
(define (list->grid l)
  (when (empty? l)
    (error "cannot create grid from empty list"))
  (when (ormap (λ (m) (not (= (length m) (length (car l))))) (cdr l))
    (error "all rows must have the same length"))
  (define width (length (car l)))
  (define height (length l))
  (grid (list->vector (foldl (λ (a b) (append b a)) '() l)) width height))

;; Make a grid of the given width and height, with all cells containing
;; value.
(define (make-grid width height value)
  (grid (make-vector (* width height) value) width height))

;; Return the value of the cell at the given index.
(define (grid-ref g x y)
  (vector-ref (grid-cells g) (+ (* y (grid-width g)) x)))

;; Set the value of the cell at the given index.
(define (grid-set! g x y c)
  (vector-set! (grid-cells g) (+ (* y (grid-width g)) x) c))

;; Copy the grid and set the value of the cell at the given index.
(define (grid-set g x y c)
  (define gc (grid (vector-copy (grid-cells g)) (grid-width g) (grid-height g)))
  (grid-set! gc x y c)
  gc)

;; Return a list of the indices of the cells adjacent to the cell at x
;; and y in the cardinal directions.
(define (grid-cardinal-adjacent-indices g x y)
  (match-define (grid _ width height) g)
  (for/list ([dx '(0 1 0 -1)]
             [dy '(1 0 -1 0)]
             #:when (and (>= (+ x dx) 0) (< (+ x dx) width) (>= (+ y dy) 0) (< (+ y dy) height)))
    (list (+ x dx) (+ y dy))))

;; Return a list of the indices of the cells adjacent to the cell at x
;; and y in the cardinal and ordinal directions.
(define (grid-ordinal-adjacent-indices g x y)
  (match-define (grid _ width height) g)
  (for/list ([dx '(0 1 1 1 0 -1 -1 -1)]
             [dy '(1 1 0 -1 -1 -1 0 1)]
             #:when (and (>= (+ x dx) 0) (< (+ x dx) width) (>= (+ y dy) 0) (< (+ y dy) height)))
    (list (+ x dx) (+ y dy))))

;; Return a list of the cells adjacent to the cell at x and y in the
;; cardinal directions.
(define (grid-cardinal-adjacent g x y)
  (map (λ (p) (apply grid-ref g p)) (grid-cardinal-adjacent-indices g x y)))

;; Return a list of the cells adjacent to the cell at x and y in the
;; cardinal and ordinal directions.
(define (grid-ordinal-adjacent g x y)
  (map (λ (p) (apply grid-ref g p)) (grid-ordinal-adjacent-indices g x y)))

;; Return a list of the cells in a grid that some predicate holds for.
(define (grid-filter g p)
  (filter p (vector->list (grid-cells g))))

;; Map a function that accepts the coordinates and value of a cell over
;; a grid.
(define (grid-map g f)
  (for*/list ([x (range (grid-width g))] [y (range (grid-height g))])
    (f x y (grid-ref g x y))))

;; Fold a function that accepts the coordinates and value of a cell, and
;; an accumulator, over a grid.
(define (grid-fold g f i)
  (for*/fold ([a i]) ([x (range (grid-width g))] [y (range (grid-height g))])
    (f x y (grid-ref g x y) a)))

;; Exports.
(provide/contract
 [list->grid (-> (list/c (list/c any/c)) grid?)]
 [make-grid (-> integer? integer? any/c grid?)]
 [grid-ref (-> grid? integer? integer? any/c)]
 [grid-set! (-> grid? integer? integer? any/c void?)]
 [grid-set (-> grid? integer? integer? any/c grid?)]
 [grid-cells (-> grid? any/c)]
 [grid-width (-> grid? integer?)]
 [grid-height (-> grid? integer?)]
 [grid-cardinal-adjacent-indices (-> grid? integer? integer? (list/c (cons/c integer? integer?)))]
 [grid-ordinal-adjacent-indices (-> grid? integer? integer? (list/c (cons/c integer? integer?)))]
 [grid-cardinal-adjacent (-> grid? integer? integer? (list/c any/c))]
 [grid-ordinal-adjacent (-> grid? integer? integer? (list/c any/c))]
 [grid-filter (-> grid? (-> any/c boolean?) (list/c any/c))]
 [grid-map (-> grid? (-> integer? integer? any/c) (list/c any/c))]
 [grid-fold (-> grid? (-> integer? integer? any/c any/c) any/c any/c)])
