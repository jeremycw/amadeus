;Defines the protocol for a multimethod. Defines the actual function that does
;the lookup in the function table
(define-macro (define-protocol signature)
  `(define ,signature
     ;filter the implementations by testing the first param with the test
     ;function
     (let ((pair (car (filter (lambda (pair)
                                ((car pair) ,(list-ref signature 1)))
                              (table-ref poly-fn-lookup
                                         (quote ,(car signature)))))))
     ,(cons '(cdr pair) (cdr signature)))))

;Convenience syntax for defining multimethods
(define-macro (define-method type signature #!rest body)
  `(define-poly (quote ,(car signature))
                ,(string->symbol (apply string-append
                                        (map symbol->string (list type '?))))
                (lambda ,(cdr signature) ,@body)))
