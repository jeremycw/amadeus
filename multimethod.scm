(declare (standard-bindings) (extended-bindings) (block) (not safe))

(define poly-fn-lookup (make-table))

;Define a polymorphic function. Takes a symbol representing the function name, 
;a test function that returns true when passed a data type that the given
;implementation should be used for and the implementation
(define (define-poly fn-name type-check-fn implementation)
  (let ((fn-list (table-ref poly-fn-lookup fn-name #f))
        (pair (cons type-check-fn implementation)))
    ;stores the test function and the implementation as a pair in a list of pairs
    ;in a lookup table where the key is the name of the polymorphic function
    (table-set! poly-fn-lookup
                fn-name
                (if fn-list (cons pair fn-list) (list pair)))))
