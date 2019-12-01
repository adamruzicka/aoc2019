#lang racket
(require threading)

(define input (open-input-file "input"))

(define modules
  (~>> input
       in-lines
       (sequence-map string->number)))

(define (module-weights modules)
  (sequence-map module-consumption modules))

(define (module-fuel-weights module-weights)
  (sequence-map (lambda (weight) (list weight (with-fuel weight))) module-weights))

(define (module-consumption module)
  (~> module
      (/ 3)
      floor
      (- 2)))

(define (with-fuel fuel [acc fuel])
  (let ([fuel-weight (module-consumption fuel)])
    (if (< fuel-weight 0)
      acc
      (with-fuel fuel-weight (+ acc fuel-weight)))))

(let* ([weights (~> modules
		    module-weights
		    module-fuel-weights)]
       [sums (apply map + (sequence->list weights))])
  (display (format "Part 1: ~a~nPart 2: ~a~n" (first sums) (last sums))))
