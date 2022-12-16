#lang racket/base

(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/sequence)
(require racket/set)
(require racket/string)

(require graph)

;;;
;;; Data structures
;;;

;; A valve represented as its label, flow rate, and a list of tunnels.
(struct valve (label flow-rate tunnels pattern) #:transparent)

;; A possible terminal state of the system.
(struct state (path flow-rate) #:transparent)

;;;
;;; Parsing
;;;

;; Read the input into a list of valves.
(define (read-input filename)
  (define (line->valve line i)
    (match-define (list _ label flow-rate tunnels)
      (regexp-match #px"^Valve ([A-Z]+).*=(\\d+);.* valves? (.+)$" line))
    (valve (string->symbol (string-downcase label))
           (string->number flow-rate)
           (map (compose string->symbol string-downcase) (string-split tunnels ", "))
           (arithmetic-shift 1 i)))
  (for/list ([line (in-lines (open-input-file filename))] [i (in-naturals)])
    (line->valve line i)))

;; Build a graph of the routes between the valves.
(define (build-graph valves)
  (define (valve->edges v)
    (for/list ([t (valve-tunnels v)])
      (list (valve-label v) t)))
  (define edges (foldl append '() (map valve->edges valves)))
  (unweighted-graph/undirected edges))

;;;
;;; States
;;;

;; Return the total flow for a valve v over m minutes.
(define (flow-over-minutes v m)
  (* m (valve-flow-rate v)))

;; Filter the set of valves to only those that have a non-zero flow
;; rate.
(define (with-flow-rate valves)
  (filter (λ (v) (> (valve-flow-rate v) 0)) valves))

;; Return the pairwise distances for each vertex in the graph as a hash
;; of vertex pairs.
(define (build-distances graph)
  (floyd-warshall graph))

;; Return a generator that yields every possible order of valve openings
;; and their total flow rate.
(define (possible-total-flows start valves distances [minutes 31] [partial #t])
  (define candidates (list->set (with-flow-rate valves)))
  (in-generator
   (let loop ([v start] [enabled 0] [remaining candidates] [minutes minutes] [flow-rate 0])
     (if (or (set-empty? remaining) (<= minutes 1))
         (yield (state enabled flow-rate))
         (let ([enabled (bitwise-ior enabled (valve-pattern v))]
               [remaining (set-remove remaining v)]
               [minutes (- minutes 1)]
               [flow-rate (+ flow-rate (flow-over-minutes v (- minutes 1)))])
           (when partial
             (yield (state enabled flow-rate)))
           (for ([cv remaining])
             (loop cv
                   enabled
                   remaining
                   (- minutes (hash-ref distances (list (valve-label v) (valve-label cv))))
                   flow-rate)))))))

;; Assuming two actors returns the total possible flow rate.
(define (possible-total-flows-with-elephant start valves distances consider [partial #t])
  (define states (sequence->list (possible-total-flows start valves distances 27 partial)))
  (define sorted-states (sort states (λ (a b) (> (state-flow-rate a) (state-flow-rate b)))))
  (for/fold ([acc -inf.0])
            ([pair (in-combinations (take sorted-states (min (length sorted-states) consider)) 2)])
    (let ([a (first pair)] [b (second pair)])
      (if (= (bitwise-and (state-path a) (state-path b)) (valve-pattern start))
          (max acc (+ (state-flow-rate a) (state-flow-rate b)))
          acc))))

;; Find the valve with the given label, #f if no such valve exists.
(define (get-valve valves label)
  (define matches (filter (λ (v) (eq? (valve-label v) label)) valves))
  (if (empty? matches) #f (car matches)))

;;;
;;; Solutions
;;;

;; Example
(define example-valves (read-input "data/example.txt"))
(define example-start (get-valve example-valves 'aa))
(define example-graph (build-graph example-valves))
(define example-distances (build-distances example-graph))
(define example-states
  (sequence->list (possible-total-flows example-start example-valves example-distances)))

;; Real data
(define input-valves (read-input "data/input.txt"))
(define input-start (get-valve input-valves 'aa))
(define input-graph (build-graph input-valves))
(define input-distances (build-distances input-graph))
(define input-states
  (sequence->list (possible-total-flows input-start input-valves input-distances 31 #f)))

;; Solution one.
(printf "Solution one (example): ~a\n" (foldl max -inf.0 (map state-flow-rate example-states)))
(printf "Solution one: ~a\n" (foldl max -inf.0 (map state-flow-rate input-states)))

;; Solution two.
(printf "Solution two (example): ~a\n"
        (possible-total-flows-with-elephant example-start example-valves example-distances 2000))
(printf "Solution two: ~a\n"
        (possible-total-flows-with-elephant input-start input-valves input-distances 5000 #f))
