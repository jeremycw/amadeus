Amadeus
=======

A library for composing applications.

```scheme
(define add (consumer (bar)
  (produce (+ (car options) bar))))

(define sum (consumer (bar)
  (produce (apply + bar))))

(define flow
  ((==> (add '(2))
        (add '(3))
        (==E (add '(4))
             (add '(5))
             (add '(6)))
        (sum))
   (lambda (data) data)))

(proc-start! flow)
(proc-push flow 1) ;=> 33
(proc-push flow 5) ;=> 45
```
