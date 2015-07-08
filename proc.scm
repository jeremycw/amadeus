(declare (standard-bindings) (extended-bindings) (block) (not safe))

(include "multimethod#.scm")
(include "proc#.scm")
(include "util#.scm")

(define-protocol (proc-start! proc))
(define-protocol (proc-push proc data))

(define-structure simple-proc consume produce options)

(define-method simple-proc (proc-start! proc) #t)

(define-method simple-proc (proc-push proc data)
  (apply (simple-proc-consume proc) (->> (simple-proc-options proc)
                                         (cons (simple-proc-produce proc))
                                         (cons (lambda () data)))))

(define-structure thread-proc thread)

(define-method thread-proc (proc-start! proc)
  (thread-start! (thread-proc-thread proc)))

(define-method thread-proc (proc-push proc data)
  (thread-send (thread-proc-thread proc) data))

(define-structure composite-proc proc-list)

(define-method composite-proc (proc-start! proc)
  (for-each (lambda (sub-proc) (proc-start! sub-proc))
            (composite-proc-proc-list proc)))

(define-method composite-proc (proc-push proc data)
  (proc-push (car (composite-proc-proc-list proc)) data))

(define (chain-procs partial-procs callback)
  (let ((reversed (reverse partial-procs)))
    (let loop ((next-proc ((car reversed) callback))
               (remaining-partials (cdr reversed))
               (proc-list '()))
      (if (eq? '() remaining-partials)
        (cons next-proc proc-list)
        (loop ((car remaining-partials) (lambda (data)
                                          (proc-push next-proc data)))
              (cdr remaining-partials)
              (cons next-proc proc-list))))))

(define (fan-procs partial-procs callback)
  (let ((proc-list
          (let loop ((rest partial-procs)
                     (procs '())
                     (id 0))
            (if (eq? rest '())
              procs
              (loop (cdr rest)
                    (cons ((car rest) callback) procs)
                    (+ id 1))))))
    (cons ((apply broadcaster proc-list) (lambda (data) #f)) proc-list)))

(define broadcaster (sync-consumer options
  (for-each (lambda (proc)
              (proc-push proc (input)))
            options)))

(define if-pusher (sync-consumer (cmp-fn proc-a proc-b)
  (if (eq? (cmp-fn (input)) #t)
    (proc-push proc-a (input))
    (proc-push proc-b (input)))))

(define cond-pusher (sync-consumer (cond-list)
  (let loop ((cond-list cond-list))
    (cond 
      ((eq? cond-list '()) #f)
      ((eq? (cdr cond-list) '()) (proc-push (car cond-list) (input)))
      (((car cond-list) (input)) (proc-push (cadr cond-list) (input)))
      (else (loop (cddr cond-list)))))))

(define (if> cmp-fn branch-a branch-b)
  (lambda (callback)
    (let* ((proc-a (branch-a callback))
           (proc-b (branch-b callback))
           (if-proc ((if-pusher cmp-fn proc-a proc-b) (lambda (data) #f))))
      (make-composite-proc (list if-proc proc-a proc-b)))))

(define (cond> . params)
  (lambda (callback)
    (let loop ((proc-list '())
               (params params)
               (cond-list '()))
      (cond
        ((eq? params '())
          (make-composite-proc
            (cons ((cond-pusher (reverse cond-list)) (lambda (data) #f))
                  proc-list)))
        ((eq? (cdr params) '())
          (let* ((proc ((car params) callback))
                 (cond-list (cons proc cond-list))
                 (proc-list (cons proc proc-list)))
            (make-composite-proc
              (cons ((cond-pusher (reverse cond-list)) (lambda (data) #f))
                    proc-list))))
        (else
          (let ((proc ((cadr params) callback))
                (condition (car params)))
            (loop (cons proc proc-list)
                  (cddr params)
                  (->> cond-list
                       (cons condition)
                       (cons proc)))))))))

(define (while> condition proc-def)
  (lambda (callback)
    (let* ((while-csmr (lambda (input exit-loop enter-loop condition)
                         (if (condition (input))
                           (enter-loop (input))
                           (exit-loop (input)))))
           (while-proc (make-simple-proc while-csmr callback #f))
           (loop-body (proc-def (lambda (data) (proc-push while-proc data))))
           (loop-fn (lambda (data) (proc-push loop-body data))))
      (simple-proc-options-set! while-proc (list loop-fn condition))
      (make-composite-proc (list while-proc loop-body)))))

(define (--> . params)
  (lambda (callback)
    (make-composite-proc
      (chain-procs params callback))))

(define (--< . params)
  (lambda (callback)
    (make-composite-proc
      (fan-procs params callback))))

(define (stream stream-decl callback)
  (stream-decl callback))
