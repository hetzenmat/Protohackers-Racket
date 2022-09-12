#lang racket

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #f))

  (define (loop)
    (accept-and-handle listener)
    (loop))
  
  (define t (thread loop))

  (λ ()
    (kill-thread t)
    (tcp-close listener)))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (thread
   (λ ()
     (handle in out (empty-tree))
     (close-input-port in)
     (close-output-port out))))

(struct node ([left #:mutable] [key #:mutable] [value #:mutable] [right #:mutable]) #:transparent)

; (struct leaf ([key #:mutable] [value #:mutable]) #:transparent)

(define (empty-tree) null)

(define (insert! tree key value)
  (cond
    [(null? tree) (node null key value null)]
    [(key . < . (node-key tree))
     (if (null? (node-left tree))
         (set-node-left! tree (node null key value null))
         (insert! (node-left tree) key value))
     tree]
    [(key . > . (node-key tree))
     (if (null? (node-right tree))
         (set-node-right! tree (node null key value null))
         (insert! (node-right tree) key value))
     tree]
    [else tree]))

(define (do-insert timestamp price state)
  (insert! state timestamp price))

(define (query-tree mintime maxtime tree)
  (cond
    [(null? tree) (values 0 0)]
    [((node-key tree) . < . mintime)
     (query-tree mintime maxtime (node-right tree))]
    ; key >= mintime
    [((node-key tree) . > . maxtime)
     (query-tree mintime maxtime (node-left tree))]
    [else ; key <= maxtime
     (define-values (sl cl) (query-tree mintime maxtime (node-left tree)))
     (define-values (sr cr) (query-tree mintime maxtime (node-right tree)))
     (values (+ sl sr (node-value tree)) (+ cl cr 1))]))

(define (do-query mintime maxtime state)
  (define-values (s c) (query-tree mintime maxtime state))
  (if (= c 0) 0 (quotient s c)))

(define (read-int32 in)
  (integer-bytes->integer (bytes (read-byte in) (read-byte in) (read-byte in) (read-byte in)) #t #t))

(define (handle in out state)
    (case (read-byte in)
      [(#x49)
       (define next-state (do-insert (read-int32 in) (read-int32 in) state))
       (handle in out next-state)]
      [(#x51)
       (let* ([mean (do-query (read-int32 in) (read-int32 in) state)]
              [res  (integer->integer-bytes mean 4 #t #t)])
         (map (λ (v) (write-byte v out)) (bytes->list res)))
       (flush-output out)
       (handle in out state)]
      [else   (void)]))
