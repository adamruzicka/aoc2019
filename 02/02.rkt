#lang racket
(require threading)

(define (reverse-map coll f) (map f coll))

(define input
  (~> "input"
      file->string
      string-trim))

(define (input-list input)
  (~> input
      (string-split ",")
      (reverse-map string->number)))

(define (preprocess-input input [noun 12] [verb 2])
  (let ([memory (list->vector input)])
    (begin
      (vector-set! memory 1 noun)
      (vector-set! memory 2 verb)
      memory)))

(define parsed-input
  (input-list input))

(define (computation memory [ip 0])
  (define (mem-get pos) (vector-ref memory pos))
  (define (dereference pos) (mem-get (mem-get pos)))
  (define (execute instruction)
    (let ([op1 (dereference (+ ip 1))]
          [op2 (dereference (+ ip 2))]
          [dst (mem-get (+ ip 3))]
          [op (if (= instruction 1) + *)])
      ;; NOOP when we would get out of bounds
      (when (< dst (vector-length memory))
        (vector-set! memory dst (op op1 op2)))))
  (let ([instruction (vector-ref memory ip)])
    (if (= 99 instruction)
        memory
        (begin
          (execute instruction)
          (computation memory (+ ip 4))))))

;; Tests
(equal? (list->vector (input-list "3500,9,10,70,2,3,11,0,99,30,40,50"))
        (computation (list->vector (input-list "1,9,10,3,2,3,11,0,99,30,40,50"))))

(equal? (list->vector (input-list "2,0,0,0,99"))
        (computation (list->vector (input-list "1,0,0,0,99"))))

(equal? (list->vector (input-list "2,3,0,6,99"))
        (computation (list->vector (input-list "2,3,0,3,99"))))

(equal? (list->vector (input-list "2,4,4,5,99,9801"))
        (computation (list->vector (input-list "2,4,4,5,99,0"))))

(equal? (list->vector (input-list "30,1,1,4,2,5,6,0,99"))
        (computation (list->vector (input-list "1,1,1,4,99,5,6,0,99"))))

(define (solve input [noun 12] [verb 2])
  (vector-ref (computation (preprocess-input input noun verb)) 0))

;; Part 1
(solve parsed-input)

;; Part 2
(for*/first ([noun (in-range 1 100)]
             [verb (in-range 1 100)]
             #:when (= (solve parsed-input noun verb) 19690720))
  (+ (* 100 noun) verb))
