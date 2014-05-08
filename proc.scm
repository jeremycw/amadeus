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

(define (extend-proc-with-y-comb proc size)
  (make-thread-proc
    (make-thread (lambda ()
                   (let loop ((data (thread-receive))
                              (queue (apply create-multi-queue (range 0 size)))
                              (msg-count 0))
                     (if (not (queue-empty? queue))
                       (begin
                         ((simple-proc-produce proc) (queue-peek queue))
                         (loop (thread-receive) (deqeue queue)))
                       (loop (thread-receive) queue)))))))

(define (chain-procs partial-procs)
  (let ((reversed (reverse partial-procs)))
    (let loop ((next-proc ((car reversed) (lambda (in) #f)))
               (remaining-partials (cdr reversed))
               (proc-list '()))
      (if (eq? '() remaining-partials)
        (cons next-proc proc-list)
        (loop ((car remaining-partials) (lambda (data)
                                          (proc-push next-proc data)))
              (cdr remaining-partials)
              (cons next-proc proc-list))))))

(define-macro (lambda-consumer signature #!rest body)
  `(lambda (#!optional opts)
    (lambda (producer)
      (make-simple-proc
        (lambda ,(append signature '(options produce)) ,@body)
        producer
        opts))))

(define (-> . params)
  (make-composite-proc
    (chain-procs params)))
