(define-multi (proc-start! proc))
(define-multi (proc-push proc data))

(define-type simple-proc consume produce options)

(define-method simple-proc (proc-start! proc) #t)

(define-method simple-proc (proc-push proc data)
  ((simple-proc-consume proc) data
                              (simple-proc-options proc)
                              (simple-proc-produce proc)))

(define-type thread-proc thread)

(define-method thread-proc (proc-start! proc)
  (thread-start! (thread-proc-thread proc)))

(define-method thread-proc (proc-push proc data)
  (thread-send (thread-proc-thread proc) data))

(define-type composite-proc proc-list)

(define-method composite-proc (proc-start! proc)
  (for-each (lambda (sub-proc) (proc-start! sub-proc))
            (composite-proc-proc-list proc)))

(define-method composite-proc (proc-push proc data)
  (proc-push (car (composite-proc-proc-list proc)) data))

(define (extend-proc-with-thread proc)
  (make-thread-proc
    (make-thread
      (lambda ()
        (let loop ((data (thread-receive)))
          ((simple-proc-consume proc) data
                                      (simple-proc-options proc)
                                      (simple-proc-produce proc))
          (loop (thread-receive)))))))

(define (make-y-combinator cb size)
  (make-thread-proc
    (make-thread
      (lambda ()
        (let loop ((queue (multi-enqueue
                            (apply create-multi-queue
                                   (range 0 size))
                            (thread-receive))))
          (if (not (queue-empty? queue))
            (begin
              (cb (reverse (map cadr (queue-peek queue))))
              (loop (multi-enqueue (dequeue queue) (thread-receive))))
            (loop (multi-enqueue queue (thread-receive)))))))))

(define (chain-procs partial-procs produce)
  (let ((reversed (reverse partial-procs)))
    (let loop ((next-proc ((car reversed) produce))
               (remaining-partials (cdr reversed))
               (proc-list '()))
      (if (eq? '() remaining-partials)
        (cons next-proc proc-list)
        (loop ((car remaining-partials) (lambda (data)
                                          (proc-push next-proc data)))
              (cdr remaining-partials)
              (cons next-proc proc-list))))))

(define (fan-procs partial-procs produce)
  (let ((y-comb (make-y-combinator
                  produce
                  (length partial-procs))))
    (let ((proc-list
            (let loop ((rest partial-procs)
                       (procs '())
                       (id 0))
              (if (eq? rest '())
                procs
                (loop (cdr rest)
                      (cons ((car rest) (lambda (data)
                                          (proc-push y-comb (list id data))))
                            procs)
                      (+ id 1))))))
      (cons ((multiplexer proc-list) (lambda (data) #f))
            (cons y-comb proc-list)))))

(define-macro (consumer signature #!rest body)
  `(lambda (#!optional opts)
    (lambda (producer)
      (make-simple-proc
        (lambda ,(append signature '(options produce)) ,@body)
        producer
        opts))))

(define-macro (threaded-consumer signature #!rest body)
  `(lambda (#!optional opts)
    (lambda (producer)
      (extend-proc-with-thread
        (make-simple-proc
          (lambda ,(append signature '(options produce)) ,@body)
          producer
          opts)))))

(define multiplexer (consumer (data)
  (for-each (lambda (proc)
              (proc-push proc data))
            options)))

(define (-> . params)
  (lambda (produce)
    (make-composite-proc
      (chain-procs params produce))))

(define (Y . params)
  (lambda (produce)
    (make-composite-proc
      (fan-procs params produce))))
