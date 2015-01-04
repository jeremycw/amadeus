(define-macro (sync-consumer args #!rest body)
  `(lambda opts
    (lambda (callback)
      (make-simple-proc
        (lambda (input output ,@args) ,@body)
        callback
        opts))))

(define-macro (async-consumer args #!rest body)
  `(lambda opts
    (lambda (callback)
      (make-thread-proc
        (make-thread
          (lambda ()
            ((lambda (input output ,@args)
               ,@body)
             (lambda () (thread-receive))
             opts
             callback)))))))
