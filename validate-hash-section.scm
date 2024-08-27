(define-module (validate-hash-section)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (validate-hash-section))

(define (validate-hash-section hash-table dynsym-table)
  (let* ((hash-size (bytevector-length hash-table))
         (dynsym-entry-size 24)  ; Size of each dynamic symbol entry
         (num-symbols (/ (bytevector-length dynsym-table) dynsym-entry-size))
         (expected-buckets (next-prime (max 1 (floor (/ num-symbols 2)))))
         (expected-size (+ 8 (* 4 expected-buckets) (* 4 num-symbols))))
    
    (format #t "Hash table size: ~a bytes~%" hash-size)
    (format #t "Number of dynamic symbols: ~a~%" num-symbols)
    (format #t "Expected number of buckets: ~a~%" expected-buckets)
    (format #t "Expected hash table size: ~a bytes~%" expected-size)
    
    (if (< hash-size expected-size)
        (begin
          (format #t "Error: Hash table is too small (is ~a, expected at least ~a)~%"
                  hash-size expected-size)
          #f)
        (begin
          (format #t "Hash table size is adequate~%")
          #t))))

;; Helper function to find the next prime number
(define (next-prime n)
  (if (prime? n)
      n
      (next-prime (+ n 1))))

(define (prime? n)
  (if (<= n 1)
      #f
      (let loop ((i 2))
        (cond
         ((> (* i i) n) #t)
         ((zero? (modulo n i)) #f)
         (else (loop (+ i 1)))))))
