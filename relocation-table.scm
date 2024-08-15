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
        (r-x86-64-64 (or (assoc-ref options 'r-x86-64-64) 1)))
    (let* ((reloc-count (length symbol-addresses))
           (table-size (* reloc-count reloc-entry-size))
           (table (make-bytevector table-size 0)))
      (let loop ((symbols symbol-addresses)
                 (index 0))
        (if (null? symbols)
            table
            (let* ((symbol (car symbols))
                   (address (cdr symbol))
                   (entry-offset (* index reloc-entry-size))
                   (symbol-index (+ index 1))  ; Adjust symbol index to start from 1
                   (r-info (logior (ash symbol-index 32) r-x86-64-64)))  ; Use R_X86_64_64 relocation type
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
