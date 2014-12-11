(##include "multimethod#.scm")
(##include "proc#.scm")

(define-multi (proc-start! proc))
(define-multi (proc-push proc data))

(define-structure simple-proc consume produce options)

(define-method simple-proc (proc-start! proc) #t)

(define-method simple-proc (proc-push proc data)
  ((simple-proc-consume proc) (lambda () data)
                              (simple-proc-options proc)
                              (simple-proc-produce proc)))

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

(define broadcaster (sync-consumer
  (for-each (lambda (proc)
              (proc-push proc (input)))
            options)))

(define (--> . params)
  (lambda (callback)
    (make-composite-proc
      (chain-procs params callback))))

(define (--< . params)
  (lambda (callback)
    (make-composite-proc
      (fan-procs params callback))))
