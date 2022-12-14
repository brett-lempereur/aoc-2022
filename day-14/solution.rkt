#lang racket/base

(require racket/class)
(require racket/draw)
(require racket/list)
(require racket/match)
(require racket/set)

(require mrlib/gif)

(require "grid.rkt")

;;;
;;; Input
;;;

;; Expand a list of line segment points into a set of points.
(define (line->points segments)
  (define (build-line from to)
    (let ([fx (min (car from) (car to))]
          [tx (max (car from) (car to))]
          [fy (min (cdr from) (cdr to))]
          [ty (max (cdr from) (cdr to))])
      (list->set (for*/list ([x (inclusive-range fx tx)] [y (inclusive-range fy ty)])
                   (cons x y)))))
  (let loop ([points (set)] [s segments])
    (if (< (length s) 2)
        points
        (loop (set-union points (build-line (first s) (second s))) (drop s 1)))))

;; Read an input file and return the cave, as a grid, and the origin point.
(define (read-input filename)
  (define input-lines
    (for/list ([line (in-lines (open-input-file filename))])
      (for/list ([coordinate (regexp-match* #px"\\d+,\\d+" line)])
        (match-let ([(list _ x y) (regexp-match #px"(\\d+),(\\d+)" coordinate)])
          (cons (string->number x) (string->number y))))))
  (define input-points (foldl set-union (set) (map line->points input-lines)))
  (define min-x (inexact->exact (foldl min +inf.0 (set-map input-points car))))
  (define points (set-map input-points (λ (p) (cons (- (car p) min-x) (cdr p)))))
  (define cave
    (make-grid
     (+ 1 (inexact->exact (foldl max -inf.0 (set-map points car))))
     (+ 1 (inexact->exact (foldl max -inf.0 (set-map points cdr))))
     'air))
  (for ([point points])
    (grid-set! cave (car point) (cdr point) 'rock))
  (values cave (cons (- 500 min-x) 0)))

;;;
;;; Drawing
;;;

;; Pens and brushes to use for each type of material.
(define material-pens
  (hash 'air (new pen% [color "white"])
        'rock (new pen% [color "dark brown"])
        'sand (new pen% [color "dark yellow"] [style 'dot])))
(define material-brushes
  (hash 'air (new brush% [color "white"])
        'rock (new brush% [color "brown"])
        'sand (new brush% [color "yellow"])))

;; Draw a cave as a bitmap with the given scale for each tile.
(define (draw-cave c [scale 8])
  (define width (grid-width c))
  (define height (grid-height c))
  (define target (make-bitmap (* scale width) (* scale height)))
  (define dc (new bitmap-dc% [bitmap target]))
  (for* ([x (range width)] [y (range height)])
    (let ([material (grid-ref c x y)])
      (send dc set-pen (hash-ref material-pens material))
      (send dc set-brush (hash-ref material-brushes material))
      (send dc draw-rectangle (* x scale) (* y scale) (+ (* x scale) scale) (+ (* y scale) scale))))
  target)

;; Animate the states of filling a cave.
(define (animate-fill filename cs [scale 8])
  (define bitmaps (map (λ (c) (draw-cave c scale)) cs))
  (write-animated-gif bitmaps 25 filename #:loop? #t #:last-frame-delay 300))

;;;
;;; Filling simulation
;;;

;; Simulate adding a new block of sand at the origin of a cave, if there
;; is no place for the sand returns #f.
(define (add-sand cave x y)
  (cond
    [(>= y (grid-height cave)) (<= x 0) (>= (grid-width cave) x) #f]
    [(eq? (grid-ref cave x (+ y 1)) 'air) (add-sand cave x (+ y 1))]
    [(eq? (grid-ref cave (- x 1) (+ y 1)) 'air) (add-sand cave (- x 1) (+ y 1))]
    [(eq? (grid-ref cave (+ x 1) (+ y 1)) 'air) (add-sand cave (+ x 1) (+ y 1))]
    [else (grid-set cave x y 'sand)]))

;; Count the number of sand cells in the cave.
(define (count-sand cave)
  (for/sum ([cell (grid-cells cave)]) (if (eq? cell 'sand) 1 0)))

;; Add sand to a cave at the origin until no more sand can be placed.
(define (add-sand-until-filled cave x y)
  (let loop ([states '()] [cave cave])
    (let ([new-cave (add-sand cave x y)])
      (if new-cave
          (loop (append states (list new-cave)) new-cave)
          states))))

;;;
;;; Solution
;;;

(define-values (input-grid input-origin) (read-input "data/input.txt"))
(define-values (example-grid example-origin) (read-input "data/example.txt"))

;; Solution one.
(let ([solution-one-states (add-sand-until-filled input-grid (car input-origin) (cdr input-origin))])
  (printf "Solution one: ~a\n" (length solution-one-states))
  (animate-fill "solution-one.gif" solution-one-states))
