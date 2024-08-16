(define-module (relocation-table)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (create-relocation-table))

(define* (create-relocation-table symbol-addresses 
                                  #:optional (options '()))
  (let ((reloc-entry-size (or (assoc-ref options 'reloc-entry-size) 24))
        (r-offset-offset (or (assoc-ref options 'r-offset-offset) 0))
        (r-info-offset (or (assoc-ref options 'r-info-offset) 8))
        (r-addend-offset (or (assoc-ref options 'r-addend-offset) 16))
        (reloc-types (list (cons 'r-x86-64-64 (or (assoc-ref options 'r-x86-64-64) 1))
                           (cons 'r-x86-64-pc32 (or (assoc-ref options 'r-x86-64-pc32) 2))
                           (cons 'r-x86-64-got32 (or (assoc-ref options 'r-x86-64-got32) 3))
                           (cons 'r-x86-64-plt32 (or (assoc-ref options 'r-x86-64-plt32) 4))))
        (symbol-index-start (or (assoc-ref options 'symbol-index-start) 1)))
    (let* ((reloc-count (length symbol-addresses))
           (table-size (* reloc-count reloc-entry-size))
           (table (make-bytevector table-size 0)))
      (let loop ((symbols symbol-addresses)
                 (index 0))
        (if (null? symbols)
            table
            (let* ((symbol (car symbols))
                   (address (cond
                              ((pair? symbol) (if (number? (car symbol)) (car symbol) (cdr symbol)))
                              ((number? symbol) symbol)
                              (else (error "Invalid symbol format" symbol))))
                   (entry-offset (* index reloc-entry-size))
                   (reloc-type (list-ref (map cdr reloc-types) (remainder index (length reloc-types))))
                   (symbol-index (+ symbol-index-start index))
                   (r-info (logior (ash symbol-index 32) reloc-type)))
              (bytevector-u64-set! table 
                                   (+ entry-offset r-offset-offset) 
                                   address 
                                   (endianness little))  ; r_offset
              (bytevector-u64-set! table 
                                   (+ entry-offset r-info-offset)
                                   r-info 
                                   (endianness little))  ; r_info
              (bytevector-u64-set! table 
                                   (+ entry-offset r-addend-offset) 
                                   0 
                                   (endianness little))  ; r_addend
              (loop (cdr symbols) (+ index 1))))))))