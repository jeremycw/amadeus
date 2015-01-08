(define-macro (->> x #!rest forms)
  (let loop ((x x) (forms forms))
    (if (eq? '() forms)
      x
      (loop (append (car forms) (list x)) (cdr forms)))))

(define-macro (-> x #!rest forms)
  (let loop ((x x) (forms forms))
    (if (eq? '() forms)
      x
      (let ((form (car forms)))
        (loop (cons (car form) (cons x (cdr form))) (cdr forms))))))

(define-macro (λ . body)
  (letrec ((find-recursive
             (lambda (term input)
               (let loop ((input input))
                 (if (eq? input '())
                   #f
                   (let ((val (car input))
                         (rest (cdr input)))
                     (if (list? val)
                       (if (find-recursive term val)
                         #t
                         (loop rest))
                       (if (eq? val term)
                         #t
                         (loop rest)))))))))
       (let loop ((count 1)
                  (args '()))
         (let ((term (string->symbol
                       (string-append "%" (number->string count)))))
           (if (find-recursive term body)
             (loop (+ count 1) (cons term args))
             `(lambda (,@(reverse args)) ,@body))))))

