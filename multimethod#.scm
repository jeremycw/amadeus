(define-macro (define-protocol signature)
  `(define ,signature
     (let ((pair (car (filter (lambda (pair)
                                ((car pair) ,(list-ref signature 1)))
                              (table-ref poly-fn-lookup
                                         (quote ,(car signature)))))))
     ,(cons '(cdr pair) (cdr signature)))))

(define-macro (define-method type signature #!rest body)
  `(define-poly (quote ,(car signature))
                ,(string->symbol (apply string-append
                                        (map symbol->string (list type '?))))
                (lambda ,(cdr signature) ,@body)))
