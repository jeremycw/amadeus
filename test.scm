(include "proc#.scm")

(load "util.scm")
(load "util#.scm")
(load "multimethod.scm")
(load "multimethod#.scm")
(load "proc.scm")
(load "proc#.scm")

(define add (sync-consumer (num)
  (output (+ num (input)))))

(define acc (async-consumer ()
  (let loop ((data (input)) (a 0))
    (output (+ a data))
    (loop (input) (+ a data)))))

(define add-stream
  (stream
    (--> (add 2)
         (if> (λ(> %1 5))
           (--> (add 3) (add 6))
           (--> (add 4) (add 7)))
         (cond> (λ(> %1 5)) (--> (add 3) (add 6))
                (λ(> %1 3)) (--> (add 4) (add 7))
                (add 1))
         (while> (λ(< %1 20))
           (--> (add 1) (add 2)))
         (add 3)
         (--< (--> (add 4) (add 7))
              (add 5)
              (add 6))
         (acc))
    display))

(proc-start! add-stream)
(proc-push add-stream 1) ;=> 12 23 40
(proc-push add-stream 5) ;=> 56 71 92
