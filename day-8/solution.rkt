#lang racket/base

(require racket/list)
(require racket/sequence)
(require racket/vector)

;; Data structures.
(struct grid (data w h))
(struct tree (height [visible #:mutable] scores) #:transparent)

;; Compute visibility for the entire grid.
(define (update-visibility! grid)
  (define (mark-visible! line dir)
    (let loop ([trees line] [prev null] [tallest -1])
      (when (not (empty? trees))
        (let* ([head (car trees)] [head-height (tree-height head)] [tail (cdr trees)])
          (when (> head-height tallest)
            (set-tree-visible! head #t))
          (loop tail head (max head-height tallest))))))
  (for ([x (range 0 (grid-w grid))])
    (mark-visible! (grid-column grid x) 'up)
    (mark-visible! (reverse (grid-column grid x)) 'down))
  (for ([y (range 0 (grid-h grid))])
    (mark-visible! (grid-row grid y) 'left)
    (mark-visible! (reverse (grid-row grid y)) 'right)))

;; Compute directional scenic scores for each tree.
(define (update-scores! grid)
  (define (compute-score! trees dir)
    (define last-blocked
      (make-hash (for/list ([i (range 10)])
                   (cons i 0))))
    (define (update-last-blocked! height i)
      (for ([h (range (add1 height))])
        (hash-set! last-blocked h i)))
    (for ([tree trees] [n (in-naturals)])
      (hash-set! (tree-scores tree) dir (- n (hash-ref last-blocked (tree-height tree))))
      (update-last-blocked! (tree-height tree) n)))
  (for ([x (range 0 (grid-w grid))])
    (compute-score! (grid-column grid x) 'up)
    (compute-score! (reverse (grid-column grid x)) 'down))
  (for ([y (range 0 (grid-h grid))])
    (compute-score! (grid-row grid y) 'left)
    (compute-score! (reverse (grid-row grid y)) 'right)))

;; Read a grid from an input file.
(define (read-grid filename)
  (define input-lines (sequence->list (in-lines (open-input-file filename))))
  (define input-grid
    (grid (list->vector (map (λ (c)
                               (tree (- (char->integer c) 48)
                                     #f
                                     (make-hash '((up . 0) (down . 0) (left . 0) (right . 0)))))
                             (flatten (map string->list input-lines))))
          (string-length (car input-lines))
          (length input-lines)))
  (update-visibility! input-grid)
  (update-scores! input-grid)
  input-grid)

;; Get the value of a grid cell.
(define (grid-ref grid x y)
  (vector-ref (grid-data grid) (+ (* y (grid-w grid)) x)))

;; Return a list of the trees in a grid row.
(define (grid-row grid y)
  (for/fold ([acc '()]) ([x (range (- (grid-w grid) 1) -1 -1)])
    (cons (grid-ref grid x y) acc)))

;; Return a list of the trees in a grid column.
(define (grid-column grid x)
  (for/fold ([acc '()]) ([y (range (- (grid-h grid) 1) -1 -1)])
    (cons (grid-ref grid x y) acc)))

;; Return the total scenic score of a tree.
(define (scenic-score t)
  (apply * (hash-values (tree-scores t))))

;; Load the input grid and compute its visibility.
(define input-grid (read-grid "data/input.txt"))

;; Output the solutions.
(printf "Number of visible trees: ~a\n"
        (for*/sum ([x (range (grid-w input-grid))] [y (range (grid-h input-grid))])
                  (if (tree-visible (grid-ref input-grid x y)) 1 0)))
(printf "Maximum possible scenic score: ~a\n"
        (foldl max 0 (map scenic-score (vector->list (grid-data input-grid)))))
