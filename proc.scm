(define-multi (proc-start! proc))
(define-multi (proc-push proc data))

(define-type simple-proc consume produce options)

(define-method simple-proc (proc-start! proc) #t)

(define-method simple-proc (proc-push proc data)
  ((simple-proc-consume proc) (lambda () data)
                              (simple-proc-options proc)
                              (simple-proc-produce proc)
                              (lambda () #t)))

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
      (cons ((broadcaster proc-list) (lambda (data) #f))
            (cons y-comb proc-list)))))

(define-macro (consumer-fn #!rest body)
  `(lambda (#!optional opts)
    (lambda (producer)
      (make-simple-proc
        (lambda (input options output input-ready?) ,@body)
        producer
        opts))))

(define-macro (consumer #!rest body)
  `(lambda (#!optional opts)
    (lambda (producer)
      (make-thread-proc
        (make-thread
          (lambda ()
            ((lambda (input options output input-ready?)
               ,@body)
             (lambda () (thread-receive))
             opts
             producer
             (lambda () (if (thread-mailbox-next 0 #f) #t #f)))))))))

(define broadcaster (consumer
  (for-each (lambda (proc)
              (proc-push proc (input)))
            options)))

(define (==> . params)
  (lambda (produce)
    (make-composite-proc
      (chain-procs params produce))))

(define (==< . params)
  (lambda (produce)
    (make-composite-proc
      (fan-procs params produce))))
