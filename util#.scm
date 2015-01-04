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
