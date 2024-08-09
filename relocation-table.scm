(define-module (relocation-table)
  #:use-module (rnrs bytevectors)
  #:export (create-relocation-table))

;; Section creation functions

(define (create-relocation-table symbol-addresses)
  (let* ((reloc-count (length symbol-addresses))
         (table-size (* reloc-count 24))
         (table (make-bytevector table-size 0)))
    (let loop ((symbols symbol-addresses)
               (index 0))
      (if (null? symbols)
        table
        (let* ((symbol (car symbols))
               (name (car symbol))
               (address (cdr symbol)))
          (bytevector-u64-set! table (* index 24) address (endianness little))  ; r_offset
          (bytevector-u64-set! table (+ (* index 24) 8)
                               (logior (ash index 32) 1) (endianness little))  ; r_info (1 = R_X86_64_64)
          (bytevector-u64-set! table (+ (* index 24) 16) 0 (endianness little))  ; r_addend
          (loop (cdr symbols) (+ index 1)))))))
