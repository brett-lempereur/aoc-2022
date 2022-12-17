#lang racket/base

(require racket/function)
(require racket/generator)
(require racket/hash)
(require racket/list)
(require racket/port)
(require racket/set)
(require racket/stream)

;;;
;;; Data structures
;;;

;; A cavern, where rows is a hash-map from row number where row zero is
;; the floor, and height is the highest row on which a shape partially
;; or wholly exists.
(struct cavern (rows height))

;;
(struct state (cavern si di) #:transparent)

;; The list of input shapes, pre-aligned with their left edges two units
;; away from the left wall and padded on the right to fit in seven bits.
(define input-shapes
  (list (list #b0011110)
        (list #b0001000 #b0011100 #b0001000)
        (list #b0000100 #b0000100 #b0011100)
        (list #b0010000 #b0010000 #b0010000 #b0010000)
        (list #b0011000 #b0011000)))

;;;
;;; Input
;;;

;; Read an input file as a list of directions.
(define (read-input filename)
  (define char-to-direction (hash #\< 'left #\> 'right))
  (define input-string (port->string (open-input-file filename)))
  (append (map (curry hash-ref char-to-direction)
               (filter (curry hash-has-key? char-to-direction) (string->list input-string)))))

;;;
;;; Simulation
;;;

;; Get the value of a cavern row, implicitly zero if we have not placed
;; a shape there yet.
(define (get-row cavern i)
  (hash-ref (cavern-rows cavern) i #b0000000))

;; If possible, move a shape in the given horizontal direction and
;; return the updated shape, otherwise return the given shape.
(define (try-move state direction shape baseline)
  (define bitmask (if (eq? direction 'left) #b1000000 #b0000001))
  (define shift (if (eq? direction 'left) 1 -1))
  (define updated-shape (map (λ (r) (arithmetic-shift r shift)) shape))
  (define height (length shape))
  (if (and (not (ormap (λ (r) (> (bitwise-and bitmask r) 0)) shape))
           (not (ormap (λ (r i) (> (bitwise-and (get-row state (+ baseline i)) r) 0))
                       updated-shape
                       (reverse (range height)))))
      updated-shape
      shape))

;; Holds if it is possible for a shape to fall by one row.
(define (can-fall? state shape baseline)
  (define height (length shape))
  (and (> baseline 0)
       (not (ormap (λ (r i) (> (bitwise-and (get-row state (- (+ baseline i) 1)) r) 0))
                   shape
                   (reverse (range height))))))

;; Simulate dropping n rocks from the top of a cavern, returning the
;; resulting cavern structure.
(define (simulate shapes directions [n 2022])
  (define shape-count (length shapes))
  (define direction-count (length directions))
  (define (do-simulation c n)
    (in-generator
     (let outer-loop ([c c] [si 0] [di 0] [n n])
       (define shape (list-ref shapes si))
       (define height (length shape))
       (define baseline (+ 3 (cavern-height c)))
       (yield (state c si di))
       (unless (<= n 0)
         (let loop ([c c] [shape shape] [di di] [baseline baseline])
           (define direction (list-ref directions di))
           (define after-move (try-move c direction shape baseline))
           (if (can-fall? c after-move baseline)
               (loop c after-move (modulo (+ di 1) direction-count) (- baseline 1))
               (outer-loop
                (cavern
                 (foldl (λ (r i a)
                          (hash-set a (+ baseline i) (bitwise-ior r (get-row c (+ baseline i)))))
                        (cavern-rows c)
                        after-move
                        (reverse (range height)))
                 (inexact->exact (max (cavern-height c) (+ baseline height))))
                (modulo (+ si 1) shape-count)
                (modulo (+ di 1) direction-count)
                (- n 1))))))))
  (do-simulation (cavern (hash) 0) n))

;; Return the final state of a simulation.
(define (final-state shapes directions [n 2022])
  (for/last ([state (simulate shapes directions n)])
    (state-cavern state)))

;; Attempt to find a cycle.
(define (find-cycle shapes directions [n 5000] [t 1000000000000])
  (define states (sequence->stream (simulate shapes directions n)))
  (let loop ([seen (hash)] [states states] [n 1])
    (if (stream-empty? states)
        #f
        (let* ([state (stream-first states)]
               [cache-key (list (state-si state) (state-di state))]
               [existing (hash-ref seen cache-key #f)])
          (if existing
              (let* ([eh (cavern-height (car existing))]
                     [en (cdr existing)]
                     [ch (cavern-height (state-cavern state))]
                     [hd (- ch eh)])
                (list eh ch en n hd)
                (+ eh ch (* hd (sub1 (ceiling (/ (- t en) (- n en)))))))
              (loop (hash-set seen cache-key (cons (state-cavern state) n))
                    (stream-rest states)
                    (+ n 1)))))))

;;;
;;; Display
;;;

;; Print the rows of a cavern.
(define (print-cavern state)
  (define height (cavern-height state))
  (for ([i (inclusive-range height 0 -1)])
    (define row (get-row state i))
    (printf "|")
    (for ([j (inclusive-range 6 0 -1)])
      (if (> (bitwise-and row (arithmetic-shift 1 j)) 0) (printf "#") (printf ".")))
    (printf "|\n"))
  (printf "+-------+\n"))

;;;
;;; Solutions
;;;

(define example-directions (read-input "data/example.txt"))
(define input-directions (read-input "data/input.txt"))

(printf "Solution one (example): ~a\n"
        (cavern-height (final-state input-shapes example-directions 2022)))
(printf "Solution one: ~a\n" (cavern-height (final-state input-shapes input-directions 2022)))

#;(printf "Solution two (example): ~a\n"
          (cavern-height (simulate input-shapes example-directions 1000000000000)))
