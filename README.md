Amadeus
=======

A library for composing applications.

```scheme
(define add (consumer-fn
  (output (+ (car options) (input)))))

(define sum (consumer
  (let loop ((data (input)))
    (output (apply + data))
    (loop (input)))))

(define flow
  ((==> (add '(2))
        (add '(3))
        (==< (add '(4))
             (add '(5))
             (add '(6)))
        (sum))
   (lambda (data) data)))

(proc-start! flow)
(proc-push flow 1) ;=> 33
(proc-push flow 5) ;=> 45
```
