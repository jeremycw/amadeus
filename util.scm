(define (filter f lst)
  (define (iter lst result)
    (cond
      ((null? lst) (reverse result))
      ((f (car lst)) (iter (cdr lst)
                           (cons (car lst) result)))
      (else (iter (cdr lst)
                  result))))
  (iter lst '()))

(define (find-by query l)
  (car (filter query l)))

(define (range start end)
  (let loop ((results '())
             (current (- end 1)))
    (if (eq? current start)
      (cons start results)
      (loop (cons current results) (- current 1)))))

(define (contains? l item)
  (let loop ((rest l))
    (cond ((eq? rest '()) #f)
          ((eq? (car rest) item) #t)
          (else (loop (cdr rest))))))

(define (reduce fn l init)
  (let loop ((rest l)
             (memo init))
    (if (eq? rest '())
      memo
      (loop (cdr rest) (fn memo (car rest))))))

(define (any? l)
  (contains? l #t))

(define (every? l)
  (reduce (lambda (memo elem) (and memo elem)) l #t))

(define (none? l)
  (not (any? l)))
