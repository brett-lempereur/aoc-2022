#lang racket/base

(require racket/function)
(require racket/sequence)
(require racket/string)

(define (input->rounds filename)
  (define (char->shape c)
    (modulo (- (char->integer c) 65) 23))
  (define (line->round line)
    (list (char->shape (string-ref line 0)) (char->shape (string-ref line 2))))
  (sequence-map line->round (in-lines (open-input-file filename))))

(define (score-round opponent player)
  (+ (+ 1 player)
     (cond
       [(= player (modulo (+ 1 opponent) 3)) 6]
       [(= player opponent) 3]
       [else 0])))

(define (fulfill-goal opponent player)
  (cond
    [(= player 0) (list opponent (modulo (- opponent 1) 3))]
    [(= player 1) (list opponent opponent)]
    [(= player 2) (list opponent (modulo (+ opponent 1) 3))]))

(define (score-tournament rounds)
  (sequence-fold + 0 (sequence-map (curry apply score-round) rounds)))

(printf "Solution for part one: ~a\n"
        (score-tournament (input->rounds "data/input.txt")))
(printf "Solution for part two: ~a\n"
        (score-tournament (sequence-map (curry apply fulfill-goal)
                                        (input->rounds "data/input.txt"))))
