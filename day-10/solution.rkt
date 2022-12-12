#lang racket/base

(require racket/generator)
(require racket/match)
(require racket/set)
(require racket/stream)

;; Decode and execute a program, producing a list of all register file
;; states.
(define (execute-program filename)
  (define input-lines (in-lines (open-input-file filename)))
  (in-generator (let loop ([lines (sequence->stream input-lines)] [x 1])
                  (unless (stream-empty? lines)
                    (match (stream-first lines)
                      [(regexp #px"^noop$")
                       (yield x)
                       (loop (stream-rest lines) x)]
                      [(regexp #px"^addx (-?\\d+)" (list _ a))
                       (let ([nx (+ x (string->number a))])
                         (yield x)
                         (yield x)
                         (loop (stream-rest lines) nx))])))))

;; Computes the signal strength for a program.
(define (signal-strength filename points)
  (for/fold ([acc 0]) ([cycle (in-naturals 1)] [x (execute-program filename)])
    (if (set-member? points cycle) (+ (* cycle x) acc) acc)))

;; Renders the screen for a program.
(define (signal-screen filename screen-w)
  (define (pixel-set? cycle x)
    (define i (modulo cycle screen-w))
    (<= (abs (- i x)) 1))
  (for/fold ([acc ""]) ([cycle (in-naturals 1)] [x (execute-program filename)])
    (let ([c (if (pixel-set? cycle (add1 x)) "#" ".")] [n (if (= (modulo cycle screen-w) 0) "\n" "")])
      (string-append acc c n))))

;; Print the solutions.
(define input-filename "data/input.txt")
(printf "Solution one: ~a\n" (signal-strength input-filename (set 20 60 100 140 180 220)))
(printf "Solution two:\n~a\n" (signal-screen input-filename 40))
