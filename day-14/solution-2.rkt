#lang racket/base

(require racket/hash)
(require racket/list)
(require racket/match)
(require racket/set)

;;;
;;; Cave
;;;

;; Data structures.
(struct cave (cells floor-y) #:transparent)
(struct point (x y) #:transparent)

;; Return the material at the given coordinate of the cave.
(define (cave-ref c pt)
  (hash-ref (cave-cells c) pt (if (= (point-y pt) (cave-floor-y c)) 'rock 'air)))

;; Set the material at the given coordinate of the cave.
(define (cave-set c pt m)
  (struct-copy cave c [cells (hash-set (cave-cells c) pt m)]))

;; Add two points.
(define (point-add a b)
  (point (+ (point-x a) (point-x b)) (+ (point-y a) (point-y b))))

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
                   (point x y)))))
  (let loop ([points (set)] [s segments])
    (if (< (length s) 2)
        points
        (loop (set-union points (build-line (first s) (second s))) (drop s 1)))))

;; Read an input file and return the cave.
(define (read-input filename)
  (define input-lines
    (for/list ([line (in-lines (open-input-file filename))])
      (for/list ([coordinate (regexp-match* #px"\\d+,\\d+" line)])
        (match-let ([(list _ x y) (regexp-match #px"(\\d+),(\\d+)" coordinate)])
          (cons (string->number x) (string->number y))))))
  (define points (foldl set-union (set) (map line->points input-lines)))
  (define max-y (inexact->exact (foldl max -inf.0 (set-map points point-y))))
  (for/fold ([c (cave (hash) (+ max-y 2))]) ([pt points])
    (cave-set c pt 'rock)))

;;;
;;; Simulation
;;;

;; Simulate adding a new block of sand at the origin of a cave, returns
;; #f if no new sand can be added.
(define (add-sand cave pt [floor-exists? #f] [origin pt])
  (define down-left (point-add pt (point -1 1)))
  (define below (point-add pt (point 0 1)))
  (define down-right (point-add pt (point 1 1)))
  (cond
    [(eq? (cave-ref cave below) 'air) (add-sand cave below floor-exists? origin)]
    [(eq? (cave-ref cave down-left) 'air) (add-sand cave down-left floor-exists? origin)]
    [(eq? (cave-ref cave down-right) 'air) (add-sand cave down-right floor-exists? origin)]
    [(equal? pt origin) #f]
    [else
     (if (and (= (+ 1 (point-y pt)) (cave-floor-y cave)) (not floor-exists?))
         #f
         (cave-set cave pt 'sand))]))

;; Add sand to a cave at the origin until no more sand can be placed.
(define (add-sand-until-filled cave origin [floor-exists? #f])
  (let ([new-cave (add-sand cave origin floor-exists?)])
    (if new-cave
        (add-sand-until-filled new-cave origin floor-exists?)
        (if floor-exists? (cave-set cave origin 'sand) cave))))

;; Count the number of sand tiles in a cave.
(define (count-sand cave)
  (for/sum ([c (hash-values (cave-cells cave))]) (if (eq? c 'sand) 1 0)))

;;;
;;; Solution
;;;

(define input-cave (read-input "data/input.txt"))
(define example-cave (read-input "data/example.txt"))

(printf "Solution one: ~a\n" (count-sand (add-sand-until-filled input-cave (point 500 0))))
(printf "Solution two: ~a\n" (count-sand (add-sand-until-filled input-cave (point 500 0) #t)))
