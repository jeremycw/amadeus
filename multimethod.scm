(define poly-fn-lookup (make-table))

(define (symbol-append . syms)
  (string->symbol (apply string-append (map symbol->string syms))))

(define-macro (define-multi signature)
  `(define ,signature
     (let ((pair (car (filter (lambda (pair)
                                ((car pair) ,(list-ref signature 1)))
                              (table-ref poly-fn-lookup
                                         (quote ,(car signature)))))))
     ,(cons '(cdr pair) (cdr signature)))))

(define (define-poly fn-name type-check-fn implementation)
  (let ((fn-list (table-ref poly-fn-lookup fn-name #f))
        (pair (cons type-check-fn implementation)))
    (table-set! poly-fn-lookup
                fn-name
                (if fn-list (cons pair fn-list) (list pair)))))

(define-macro (define-method type signature #!rest body)
  `(define-poly (quote ,(car signature))
                ,(symbol-append type '?)
                (lambda ,(cdr signature) ,@body)))
