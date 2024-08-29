(define-module (relocation-table)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:export (create-relocation-table))

(define* (create-relocation-table symbol-addresses 
                                  #:optional (options '()))
  (let ((reloc-entry-size (or (assoc-ref options 'reloc-entry-size) 24))
        (r-offset-offset (or (assoc-ref options 'r-offset-offset) 0))
        (r-info-offset (or (assoc-ref options 'r-info-offset) 8))
        (r-addend-offset (or (assoc-ref options 'r-addend-offset) 16))
        (reloc-type (or (assoc-ref options 'r-x86-64-glob-dat) 6))
        (symbol-index-start (or (assoc-ref options 'symbol-index-start) 1))
        ;; Update the GOT base with the new offset from the .got section
        (got-base (or (assoc-ref options 'got-base) #x3120))) ; Updated GOT base

    (let* ((non-function-symbols 
            (if (hash-table? symbol-addresses)
                (filter (lambda (entry) (not (cdr (cdr entry))))
                        (hash-map->list cons symbol-addresses))
                (filter (lambda (symbol) (not (and (pair? symbol) (cdr symbol))))
                        symbol-addresses)))
           (reloc-count (length non-function-symbols))
           (table-size (* reloc-count reloc-entry-size))
           (table (make-bytevector table-size 0)))

      (let loop ((symbols non-function-symbols)
                 (index 0))
        (if (null? symbols)
            table
            (let* ((symbol (car symbols))
                   (name (if (hash-table? symbol-addresses)
                             (car symbol)
                             (if (pair? symbol) (car symbol) symbol)))
                   (value (if (hash-table? symbol-addresses)
                              (car (cdr symbol))
                              (if (pair? symbol) (car symbol) symbol)))
                   (address (if (pair? value) (car value) value))
                   (entry-offset (* index reloc-entry-size))
                   (symbol-index (+ symbol-index-start index))
                   (got-entry (+ got-base (* index 8))) ; Each GOT entry is 8 bytes in size
                   (r-info (logior (ash symbol-index 32) reloc-type)))
              
              (bytevector-u64-set! table 
                                   (+ entry-offset r-offset-offset) 
                                   got-entry 
                                   (endianness little))  ; r_offset points to GOT entry
              
              (bytevector-u64-set! table 
                                   (+ entry-offset r-info-offset)
                                   r-info 
                                   (endianness little))  ; r_info
              
              (bytevector-u64-set! table 
                                   (+ entry-offset r-addend-offset) 
                                   0 
                                   (endianness little))  ; r_addend
              
              (loop (cdr symbols) (+ index 1))))))))
