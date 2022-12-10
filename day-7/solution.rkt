#lang racket/base

(require racket/generator)
(require racket/list)
(require racket/match)
(require racket/sequence)

;; Data structures
(struct node (name size))
(struct directory (name nodes children size))

;; Read the input into a tree and compute its size.
(define (input->directory filename)
  (define (lines->directory lines name)
    (let loop ([lines lines] [nodes '()] [children '()] [size 0])
      (if (empty? lines)
          (values '() (directory name nodes children size))
          (match (car lines)
            [(regexp #px"^\\$ cd \\.\\.$") (values (cdr lines) (directory name nodes children size))]
            [(regexp #px"^\\$ cd (.+)$" (list _ name))
             (let-values ([(lines dir) (lines->directory (cdr lines) name)])
               (loop lines nodes (cons dir children) (+ size (directory-size dir))))]
            [(regexp #px"^(\\d+) (.+)$" (list _ node-size name))
             (loop (cdr lines)
                   (cons (node name (string->number node-size)) nodes)
                   children
                   (+ size (string->number node-size)))]
            [_ (loop (cdr lines) nodes children size)]))))
  (let-values ([(_ dir)
                (lines->directory (cdr (sequence->list (in-lines (open-input-file filename)))) "/")])
    dir))

;; Return a generator that recursively yields the subdirectories of the
;; given directory.
(define (in-tree root)
  (in-generator (let loop ([unvisited (list root)])
                  (unless (empty? unvisited)
                    (let ([v (car unvisited)] [u (cdr unvisited)])
                      (yield v)
                      (loop (append (directory-children v) u)))))))

;; Return a list of directories less than size n.
(define (dirs-less-than root n)
  (define flattened (sequence->list (in-tree root)))
  (filter (λ (d) (<= (directory-size d) n)) flattened))

;; Return a list of directories greater than size n.
(define (dirs-greater-than root n)
  (define flattened (sequence->list (in-tree root)))
  (filter (λ (d) (>= (directory-size d) n)) flattened))

;; Find the optimal directory to delete to free the target space.
(define (optimal-deletion root capacity target)
  (define unused-size (- capacity (directory-size root)))
  (define needed-size (- target unused-size))
  (define candidates (dirs-greater-than root needed-size))
  (car (sort candidates (λ (a b) (<= (directory-size a) (directory-size b))))))

;; Show the solutions.
(define input-root (input->directory "data/input.txt"))
(printf "Solution one: ~a\n" (foldl + 0 (map directory-size (dirs-less-than input-root 100000))))
(printf "Solution two: ~a\n" (directory-size (optimal-deletion input-root 70000000 30000000)))
