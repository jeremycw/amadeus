Amadeus
=======

A library for composing applications.

```scheme
(define add (sync-consumer (num)
  (output (+ num (input)))))

(define acc (async-consumer ()
  (let loop ((data (input)) (a 0))
    (output (+ a data))
    (loop (input) (+ a data)))))

(define add-stream
  (stream
    (--> (add 2)
         (if_ (λ(> %1 5))
           (--> (add 3) (add 6))
           (--> (add 4) (add 7)))
         (add 3)
         (--< (--> (add 4) (add 7))
              (add 5)
              (add 6))
         (acc))
    display))

(proc-start! add-stream)
(proc-push add-stream 1)
(proc-push add-stream 5)
```
