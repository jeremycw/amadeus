(declare (standard-bindings) (extended-bindings) (block) (not safe))

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

;(define (replace-recursive input term value)
;  (reverse
;    (let loop ((input input)
;               (output '()))
;      (if (eq? input '())
;        output
;        (let ((val-to-check (car input))
;              (rest (cdr input)))
;          (if (list? val-to-check)
;            (loop rest (cons (find-and-replace val-to-check term value) output))
;            (if (eq? val-to-check term)
;              (loop rest (cons value output))
;              (loop rest (cons val-to-check output)))))))))
;
;(define (find-recursive term input)
;  (let loop ((input input))
;    (if (eq? input '())
;      #f
;      (let ((val (car input))
;            (rest (cdr input)))
;        (if (list? val)
;          (if (find-recursive term val)
;            #t
;            (loop rest))
;          (if (eq? val term)
;            #t
;            (loop rest)))))))

