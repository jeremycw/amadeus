;(##include "multimethod#.scm")
(##include "proc#.scm")

;(##include "util.scm")
;(##include "multimethod.scm")
;(##include "proc.scm")

(define add (sync-consumer
  (output (+ (car options) (input)))))

(define acc (async-consumer
  (let loop ((data (input)) (a 0))
    (output (+ a data))
    (loop (input) (+ a data)))))

(define flow
  ((--> (add 2)
        (add 3)
        (--< (--> (add 4) (add 7))
             (add 5)
             (add 6))
        (acc))
   display))

(proc-start! flow)
(proc-push flow 1) ;=> 12 23 40
(proc-push flow 5) ;=> 56 71 92
