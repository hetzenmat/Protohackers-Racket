#lang racket

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #f))

  (define logger
    (thread
     (λ ()
       (let ([open-cons 0]
             [max-open-cons 0]
             [next-id 0]
             [stats (make-hash)])
         (let loop ()
           (match (thread-receive)
             [(list 'con thd)
              (set! open-cons (+ open-cons 1))
              (set! max-open-cons (max max-open-cons (+ open-cons 1)))
              (thread-send thd next-id)
              (hash-set! stats next-id 0)
              (set! next-id (+ 1 next-id))
              (loop)]
             [(list 'recv id)
              (hash-set! stats id (+ (hash-ref stats id) 1))
              (loop)]
             ['close
              (set! open-cons (- open-cons 1))
              (loop)]
             ['exit (printf "~a max open connections~n" max-open-cons)
                    (for ([(k v) stats])
                      (printf "Cliend ~a received ~a bytes~n" k v))]))))))

  (define (loop)
    (accept-and-handle listener logger)
    (loop))
  
  (define t (thread loop))

  (λ ()
    (kill-thread t)
    (thread-send logger 'exit)
    (tcp-close listener)))

(define (accept-and-handle listener logger)
  (define-values (in out) (tcp-accept listener))
  (thread-send logger (list 'con (current-thread)))
  (define id (thread-receive))
  (thread
   (λ ()
     (handle in out logger id)
     (close-input-port in)
     (close-output-port out)
     (thread-send logger 'close))))

(define (handle in out logger id)
  (define a (read-byte in))
  (unless (eof-object? a)
    (write-byte a out)
    (flush-output out)
    (thread-send logger (list 'recv id))
    (handle in out logger id)))
