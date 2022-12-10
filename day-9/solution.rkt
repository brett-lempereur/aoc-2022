#lang racket/base

(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/sequence)
(require racket/stream)

;; Data structures
(struct point (x y) #:transparent)

;; Convert a direction string into a symbolic direction.
(define (string->direction c)
  (match c
    ["D" (point 0 -1)]
    ["U" (point 0 1)]
    ["L" (point -1 0)]
    ["R" (point 1 0)]))

;; Return a generator that yields directions for an input file.
(define (in-moves filename)
  (define (line->pair line)
    (match line
      [(regexp #px"([DULR]) (\\d+)" (list _ direction amount))
       (cons (string->direction direction) (string->number amount))]))
  (define input-pairs (sequence-map line->pair (in-lines (open-input-file filename))))
  (in-generator (for ([pair input-pairs])
                  (for ([_ (range (cdr pair))])
                    (yield (car pair))))))

;; Return an updated position for the tail given the head.
(define (follow head tail)
  (match-define (point hx hy) head)
  (match-define (point tx ty) tail)
  (if (or (equal? head tail) (and (>= 1 (abs (- hx tx))) (>= 1 (abs (- hy ty)))))
      tail
      (let ([nx (cond
                  [(> hx tx) (+ tx 1)]
                  [(< hx tx) (- tx 1)]
                  [else tx])]
            [ny (cond
                  [(> hy ty) (+ ty 1)]
                  [(< hy ty) (- ty 1)]
                  [else ty])])
        (point nx ny))))

;; Move a point in a direction.
(define (update p v)
  (match-define (point x y) p)
  (match-define (point dx dy) v)
  (point (+ x dx) (+ y dy)))

;; Return the set of unique positions for the tail when the head follows
;; a program of moves.
(define (follow-program moves n)
  (define (apply-move knots move)
    (define uh (update (car knots) move))
    (reverse (let loop ([knots (cdr knots)] [next uh] [acc (list uh)])
               (if (empty? knots)
                   acc
                   (let ([uk (follow next (car knots))]) (loop (cdr knots) uk (cons uk acc)))))))
  (define (apply-moves moves knots [path (list (point 0 0))])
    (if (stream-empty? moves)
        path
        (let* ([uk (apply-move knots (stream-first moves))])
          (apply-moves (stream-rest moves) uk (cons (last uk) path)))))
  (apply-moves moves (make-list n (point 0 0))))

;; Count the number of moves the tail takes for a rope of n knots.
(define (count-tail-positions moves n)
  (set-count (list->set (follow-program moves n))))

;; Display the solutions.
(define input-moves (sequence->stream (in-moves "data/input.txt")))
(printf "Solution one: ~a\n" (count-tail-positions input-moves 2))
(printf "Solution one: ~a\n" (count-tail-positions input-moves 10))
