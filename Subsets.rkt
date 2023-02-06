(define (solve lst)
  (let ([result (list)] [task (list)])
    
    (struct cc (proc rest))
    
    (define (task-next!)
      (let ([work (car task)])
        (set! task (cdr task))
        ((cc-proc work) (cc (cc-proc work) (cc-rest work)))))

    (define (inner work lst)
      (let* ([c (call/cc (lambda (c) (cc c lst)))]
             [rest (cc-rest c)])
        (cond
          [(and (null? work) (null? rest)) result]
          [(null? rest) (set! result (append result (list work))) (task-next!)]
          [else (set! task (append task (list (cc (cc-proc c) (cdr rest)))))
                (inner (append work (list (car rest))) (cdr rest))])))
    (append (inner (list) lst) (list (list)))))

(define/contract (subsets nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (solve nums))