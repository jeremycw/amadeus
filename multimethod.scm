(declare (standard-bindings) (extended-bindings) (block) (not safe))

(define poly-fn-lookup (make-table))

(define (define-poly fn-name type-check-fn implementation)
  (let ((fn-list (table-ref poly-fn-lookup fn-name #f))
        (pair (cons type-check-fn implementation)))
    (table-set! poly-fn-lookup
                fn-name
                (if fn-list (cons pair fn-list) (list pair)))))
