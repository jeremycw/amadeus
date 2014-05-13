(include "util.scm")
(include "multimethod.scm")
(include "queue.scm")
(include "proc.scm")

;composite procs
(define add (consumer-fn
  (output (+ (car options) (input)))))

(define sum (consumer
  (let loop ((data (input)))
    (output (apply + data))
    (loop (input)))))

(define flow
  ((--> (add '(2))
        (add '(3))
        (--< (add '(4))
             (add '(5))
             (add '(6)))
        (sum))
   (lambda (data) (display data))))

(proc-start! flow)
(proc-push flow 1)
(proc-push flow 5)

;y-comb proc
(define y-comb-proc (make-y-combinator (lambda (a) (write a)) 3))

(proc-start! y-comb-proc)
(proc-push y-comb-proc '(0 1))
(proc-push y-comb-proc '(1 1))
(proc-push y-comb-proc '(2 1))

;multi-queues
(define multi-queue (create-multi-queue 1 2 3))

(queue-empty? multi-queue)

(define full-multi-queue
  (multi-enqueue
    (multi-enqueue
      (multi-enqueue multi-queue '(1 4))
      '(2 5))
    '(3 6)))

(dequeue full-multi-queue)

(queue-peek full-multi-queue)

