;composite procs
(define consumer (lambda-consumer (bar)
  (produce (+ (car options) bar))))

(define output (lambda-consumer (bar)
  (print bar)))

(define multiplexer (lambda-consumer (data)
  (for-each (lambda (proc) (proc-push proc data)) options)
  (produce bar)))

(-> (consumer '(2))
    (consumer '(3))
    (output))

;(=> (consumer '(2))
;    (consumer '(3)))

(proc-start! composite)
(proc-push composite 1)

;thread procs
(define thread-proc
  (extend-proc-with-thread
    (make-simple-proc (lambda (a options produce)
                        (produce (+ a (car options))))
                      (lambda (a) (write a))
                      `(1))))

(proc-start! thread-proc)

(proc-push thread-proc 5)

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

