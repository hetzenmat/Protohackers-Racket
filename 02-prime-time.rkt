#lang racket

(require json)
(require (only-in math/number-theory
                  prime?))

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
     (handle in out)
     (close-input-port in)
     (close-output-port out))))

(define (is-valid expr)
  (and (equal? "isPrime" (hash-ref expr 'method null))
       (let ([n (hash-ref expr 'number null)])
         (or (integer? n) (rational? n)))))

(define (handle in out)
  (define line (read-line in))
  (with-handlers ([exn:fail? (λ (exn)
                               (newline out)
                               (flush-output out))])
      (define msg (with-input-from-string line read-json))
      (when (not (is-valid msg)) (error 'invalid))
      (write-json (hash 'method "isPrime" 'prime (let ([n (hash-ref msg 'number)]) (and (integer? n) (> n 0) (prime? n)))) out)
      (newline out)
      (flush-output out)
      (handle in out)))