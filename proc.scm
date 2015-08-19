(declare (standard-bindings) (extended-bindings) (block) (not safe))

(include "multimethod#.scm")
(include "proc#.scm")
(include "util#.scm")

;******************************************************************************
;
; Terminology
;
;   Proc: An abstract data type for processing information that responds to
;   proc-start! and proc-push functions. (proc-start! proc) will do any
;   initialization required by the concrete Proc implementation.
;   (proc-push proc data) 'pushes' the data through the Proc. The data will be
;   processed and a callback will be called with the resulting output.
;
;   Proc Partial: a partially applied construction of a Proc. When applied with
;   a callback it returns a fully conastructed Proc.
;
;   Partial Proc Partial: a partially applied construction of a Proc. When
;   applied with an options list it returns a Proc Partial. 
;
;******************************************************************************

(define-protocol (proc-start! proc))
(define-protocol (proc-push proc data))

;A simple Proc processes input with a function and calls its callback with the
;processed output of that function
(define-structure simple-proc consume produce options)

(define-method simple-proc (proc-start! proc) #t)

(define-method simple-proc (proc-push proc data)
  (apply (simple-proc-consume proc) (->> (simple-proc-options proc)
                                         (cons (simple-proc-produce proc))
                                         (cons (lambda () data)))))

;A thread Proc does its processing in a separate thread
(define-structure thread-proc thread)

(define-method thread-proc (proc-start! proc)
  (thread-start! (thread-proc-thread proc)))

(define-method thread-proc (proc-push proc data)
  (thread-send (thread-proc-thread proc) data))

;A composite Proc is a Proc made up of smaller Procs
(define-structure composite-proc proc-list)

(define-method composite-proc (proc-start! proc)
  (for-each (lambda (sub-proc) (proc-start! sub-proc))
            (composite-proc-proc-list proc)))

(define-method composite-proc (proc-push proc data)
  (proc-push (car (composite-proc-proc-list proc)) data))

;Takes in a list of Proc Partials and callback. This creates a list of Procs
;whos callbacks push their output into the next Proc.
(define (chain-procs partial-procs callback)
  (let ((reversed (reverse partial-procs)))
    ;apply the last Proc Partial to the callback creating a Proc
    (let loop ((next-proc ((car reversed) callback))
               (remaining-partials (cdr reversed))
               (proc-list '()))
      (if (eq? '() remaining-partials)
        (cons next-proc proc-list)
        ;loop through Partial Procs applying them to callbacks that push their
        ;output into the previous proc
        (loop ((car remaining-partials) (lambda (data)
                                          (proc-push next-proc data)))
              (cdr remaining-partials)
              (cons next-proc proc-list))))))

;Takes in a list of Proc Partials and a callback. Returns a list of Procs where
;the first Proc pushes its output to all the other Procs.
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

;Creates a Partial Proc Partial that takes a list of Procs as its options list
;and pushes its output into every proc in its options list.
(define broadcaster (sync-consumer options
  (for-each (lambda (proc)
              (proc-push proc (input)))
            options)))

;Creates a Partial Proc Partial that takes a test function with the signature:
; (test data) -> bool 
;and two Procs in its options list and pushes data that tests true to the first
;Proc and data that tests false to the second Proc.
(define if-pusher (sync-consumer (cmp-fn proc-a proc-b)
  (if (eq? (cmp-fn (input)) #t)
    (proc-push proc-a (input))
    (proc-push proc-b (input)))))

;Takes in a test function and two Partial Procs. 
(define (if> cmp-fn branch-a branch-b)
  (lambda (callback)
    (let* ((proc-a (branch-a callback))
           (proc-b (branch-b callback))
           (if-proc ((if-pusher cmp-fn proc-a proc-b) (lambda (data) #f))))
      (make-composite-proc (list if-proc proc-a proc-b)))))

;Creates a Partial Proc Partial that takes an option list of test function Proc
;pairs. If a test function tests true output gets pushed to the paired Proc.
(define cond-pusher (sync-consumer (cond-list)
  (let loop ((cond-list cond-list))
    (cond 
      ;do nothing if it's the end of the list
      ((eq? cond-list '()) #f)
      ;if it's the last item it's not a pair it's just the else Proc
      ((eq? (cdr cond-list) '()) (proc-push (car cond-list) (input)))
      ;test the input with the test function push it to the paired Proc if true
      (((car cond-list) (input)) (proc-push (cadr cond-list) (input)))
      ;else try the next test Proc pair
      (else (loop (cddr cond-list)))))))

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

;The API function for chaining Procs together
(define (--> . params)
  (lambda (callback)
    (make-composite-proc
      (chain-procs params callback))))

;The API function for fanning Procs
(define (--< . params)
  (lambda (callback)
    (make-composite-proc
      (fan-procs params callback))))

(define (stream stream-decl callback)
  (stream-decl callback))
