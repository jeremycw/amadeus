(define-multi (enqueue queue thing))
(define-multi (dequeue queue))
(define-multi (queue-peek queue))
(define-multi (queue-empty? queue))

(define-structure queue (elements))

(define-method queue (enqueue queue thing)
  (make-queue (cons thing (queue-elements queue))))

(define-method queue (queue-peek queue)
  (car (reverse (queue-elements queue))))

(define-method queue (dequeue queue)
  (make-queue
    (reverse (cdr (reverse (queue-elements queue))))))

(define-method queue (queue-empty? queue)
  (eq? (queue-elements queue) '()))

(define-structure multi-queue (sub-queues))

(define (multi-queue-replace-sub-queues queue name-sub-queues)
  (let ((names (map car name-sub-queues)))
    (make-multi-queue
      (append name-sub-queues
              (filter (lambda (name-queue)
                        (not (contains? names (car name-queue))))
                      (multi-queue-sub-queues queue))))))

(define (multi-queue-sub-op queue op name)
  (op (cadr (assoc name (multi-queue-sub-queues queue)))))

(define (multi-enqueue queue name-value)
  (let* ((queue-name (car name-value))
         (queue-value (cadr name-value))
         (new-name-queue
           (cons queue-name
             (list (multi-queue-sub-op
                     queue
                     (lambda (queue) (enqueue queue queue-value))
                     queue-name)))))
         (multi-queue-replace-sub-queues queue (list new-name-queue))))

(define-method multi-queue (dequeue queue)
  (make-multi-queue
    (map (lambda (name-queue)
           (cons (car name-queue) (list (dequeue (cadr name-queue)))))
         (multi-queue-sub-queues queue))))

(define-method multi-queue (queue-peek queue)
  (map (lambda (name-queue)
         (list (car name-queue) (queue-peek (cadr name-queue))))
       (multi-queue-sub-queues queue)))

(define-method multi-queue (queue-empty? queue)
  (any?
    (map (lambda (sub-queue)
           (queue-empty? (cadr sub-queue)))
         (multi-queue-sub-queues queue))))

(define (create-multi-queue . names)
  (make-multi-queue
    (map (lambda (name)
           (list name (make-queue '()))) names)))

