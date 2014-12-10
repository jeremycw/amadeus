(include "util.scm")
(include "multimethod.scm")
(include "queue.scm")
(include "proc.scm")

;composite procs
(define add (consumer-fn
  (output (+ (car options) (input)))))

(define acc (consumer
  (let loop ((data (input)) (a 0))
    (output (+ a data))
    (loop (input) (+ a data)))))

(define flow
  ((--> (add '(2))
        (add '(3))
        (--< (--> (add '(4)) (add '(7)))
             (add '(5))
             (add '(6)))
        (acc))
   (lambda (data) (display data))))

(proc-start! flow)
(proc-push flow 1)
(proc-push flow 5)
