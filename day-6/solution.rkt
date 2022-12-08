#lang racket/base

(require racket/list)
(require racket/port)
(require racket/set)

(define (input->packet filename)
  (string->list (port->string (open-input-file filename))))

(define (packet-offset packet n)
  (let scan ([data packet] [i 0])
    (if (= (set-count (list->set (take data n))) n)
        (+ n i)
        (scan (cdr packet) (add1 i)))))

(printf "Solution one: ~a" (packet-offset (input->packet "data/input.txt") 4))
(printf "Solution two: ~a" (packet-offset (input->packet "data/input.txt") 14))
