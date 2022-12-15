#lang racket/base

(require racket/function)
(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/sequence)

;; We model a sensor as a point in space with a radius determined from
;; the position of its beacon.
(struct sensor (p b r) #:transparent)

;; A point in two-dimensional space.
(struct point (x y) #:transparent)

;; Read an input file as a list of sensors.
(define (read-input filename)
  (define (make-sensor p b)
    (sensor p b (distance p b)))
  (define (line->sensor line)
    (match-define (list _ px py bx by)
      (regexp-match #px"x=(-?\\d+), y=(-?\\d+):.*x=(-?\\d+), y=(-?\\d+)" line))
    (make-sensor (point (string->number px) (string->number py))
                 (point (string->number bx) (string->number by))))
  (map line->sensor (sequence->list (in-lines (open-input-file filename)))))

;; Compute the manhattan distance between two points.
(define (distance q r)
  (+ (abs (- (point-x q) (point-x r))) (abs (- (point-y q) (point-y r)))))

;; Holds if some row is covered by a sensor.
(define (covers-row? y s)
  (<= (abs (- (point-y (sensor-p s)) y)) (sensor-r s)))

;; Holds if some cell is covered by a sensor and definitely not its
;; beacon.
(define (covered-but-not-sensor? p s)
  (and (<= (distance p (sensor-p s)) (sensor-r s)) (not (equal? p (sensor-b s)))))

;; Holds if some cell is covered by a sensor.
(define (covered? p s)
  (<= (distance p (sensor-p s)) (sensor-r s)))

;; Return the list of points in a given row that definitely do not
;; contain a beacon.
(define (points-on-row-without-beacon sensors y)
  (define candidates (filter (curry covers-row? y) sensors))
  (define min-x
    (inexact->exact (foldl min +inf.0 (map (λ (s) (- (point-x (sensor-p s)) (sensor-r s))) sensors))))
  (define max-x
    (inexact->exact (foldl max -inf.0 (map (λ (s) (+ (point-x (sensor-p s)) (sensor-r s))) sensors))))
  (for/list ([p (map (λ (x) (point x y)) (inclusive-range min-x max-x))]
             #:when (ormap (curry covered-but-not-sensor? p) candidates))
    p))

;; Return a generator of points that define the perimeter for a sensor's
;; detection area.
(define (in-perimeter s)
  (match-define (point px py) (sensor-p s))
  (define r (+ 1 (sensor-r s)))
  (in-generator (for ([x (inclusive-range px (+ px r))] [y (inclusive-range (- py r) py)])
                  (yield (point x y)))
                (for ([x (inclusive-range (+ px r) px -1)] [y (inclusive-range py (+ py r))])
                  (yield (point x y)))
                (for ([x (inclusive-range px (- px r) -1)] [y (inclusive-range (+ py r) py -1)])
                  (yield (point x y)))
                (for ([x (inclusive-range (- px r) px)] [y (inclusive-range py (- py r) -1)])
                  (yield (point x y)))))

;; Return the list of points in a given block that definitely do not
;; contain a beacon.
(define (points-in-square-without-beacon sensors min-x min-y max-x max-y)
  (let outer-loop ([sensors sensors])
    (if (empty? sensors)
        #f
        (let ([result (for/first ([p (in-perimeter (car sensors))]
                                  #:when (and (>= (point-x p) min-x)
                                              (<= (point-x p) max-x)
                                              (>= (point-y p) min-y)
                                              (<= (point-y p) max-y)
                                              (andmap (λ (s) (not (covered? p s))) sensors)))
                        p)])
          (if result (+ (* max-x (point-x result)) (point-y result)) (outer-loop (cdr sensors)))))))

;;;
;;; Solutions
;;;

(define example-sensors (read-input "data/example.txt"))
(define input-sensors (read-input "data/input.txt"))

;; Examples.
(printf "Example one: ~a\n" (length (points-on-row-without-beacon example-sensors 10)))
(printf "Example two: ~a\n" (points-in-square-without-beacon example-sensors 0 0 20 20))

;; Solution one: the number of points that definitely don't contain a
;; beacon where y=2000000.
(printf "Solution one: ~a\n" (length (points-on-row-without-beacon input-sensors 2000000)))

;; Solution two: the tuning frequency of the square definitely not
;; covered by any sensors in the box (0, 0) (4000000, 4000000).
(printf "Solution two: ~a\n" (points-in-square-without-beacon input-sensors 0 0 4000000 4000000))
