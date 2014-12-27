(define-macro (sync-consumer . body)
  `(lambda opts
    (lambda (callback)
      (make-simple-proc
        (lambda (input options output) ,@body)
        callback
        opts))))

(define-macro (async-consumer . body)
  `(lambda opts
    (lambda (callback)
      (make-thread-proc
        (make-thread
          (lambda ()
            ((lambda (input options output)
               ,@body)
             (lambda () (thread-receive))
             opts
             callback)))))))
