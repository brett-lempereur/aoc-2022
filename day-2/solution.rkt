#lang racket/base

(require racket/function)
(require racket/sequence)
(require racket/string)

;;;
;;; Input
;;;

(define shape-lookup-table
  '#hash((#\A . rock) (#\B . paper) (#\C . scissors) (#\X . rock) (#\Y . paper) (#\Z . scissors)))

(define goal-lookup-table
  '#hash((#\X . lose) (#\Y . draw) (#\Z . win)))

(define (input->rounds filename)
  (define (line->shapes line)
    (list (hash-ref shape-lookup-table (string-ref line 0))
          (hash-ref shape-lookup-table (string-ref line 2))))
  (sequence-map line->shapes (in-lines (open-input-file filename))))

(define (input->goals filename)
  (define (line->goal line)
    (list (hash-ref shape-lookup-table (string-ref line 0))
          (hash-ref goal-lookup-table (string-ref line 2))))
  (sequence-map line->goal (in-lines (open-input-file filename))))

;;;
;;; Scoring assuming input describes rounds
;;;

(define shape-defeated-by-table '#hash((rock . paper) (paper . scissors) (scissors . rock)))
(define shape-score-table '#hash((rock . 1) (paper . 2) (scissors . 3)))

(define (score-round opponent player)
  (+ (hash-ref shape-score-table player)
     (cond
       [(equal? (hash-ref shape-defeated-by-table opponent) player) 6]
       [(equal? opponent player) 3]
       [else 0])))

(define (score-tournament rounds)
  (sequence-fold + 0 (sequence-map (curry apply score-round) rounds)))

;;;
;;; Scoring assuming input describes goals
;;;

(define shape-lose-by-table '#hash((rock . scissors) (paper . rock) (scissors . paper)))

(define (shape-for-goal opponent goal)
  (cond
    [(equal? goal 'win) (hash-ref shape-defeated-by-table opponent)]
    [(equal? goal 'draw) opponent]
    [(equal? goal 'lose) (hash-ref shape-lose-by-table opponent)]))

(define (score-round-with-goal opponent goal)
  (define player (shape-for-goal opponent goal))
  (score-round opponent player))

(define (score-tournament-with-goal rounds)
  (sequence-fold + 0 (sequence-map (curry apply score-round-with-goal) rounds)))

;;;
;;; Solutions
;;;

(define input-rounds (input->rounds "data/input.txt"))
(define input-goals (input->goals "data/input.txt"))

;; The solution for part one, the sum of the rounds as given.
(printf "Solution for part one: ~a\n" (score-tournament input-rounds))
(printf "Solution for part two: ~a\n" (score-tournament-with-goal input-goals))
