(define-module (relocation-table)
  #:use-module (rnrs bytevectors)
  #:export (create-relocation-table))

;; Constants for relocation entry
(define RELOC_ENTRY_SIZE 24)
(define R_OFFSET_OFFSET 0)
(define R_INFO_OFFSET 8)
(define R_ADDEND_OFFSET 16)

;; Constants for relocation types
(define R_X86_64_64 1)

;; Function to create relocation table
(define (create-relocation-table symbol-addresses)
  (let* ((reloc-count (length symbol-addresses))
         (table-size (* reloc-count RELOC_ENTRY_SIZE))
         (table (make-bytevector table-size 0)))
    (let loop ((symbols symbol-addresses)
               (index 0))
      (if (null? symbols)
        table
        (let* ((symbol (car symbols))
               (name (car symbol))
               (address (cdr symbol))
               (entry-offset (* index RELOC_ENTRY_SIZE)))
          (bytevector-u64-set! table 
                               (+ entry-offset R_OFFSET_OFFSET) 
                               address 
                               (endianness little))  ; r_offset
          (bytevector-u64-set! table 
                               (+ entry-offset R_INFO_OFFSET)
                               (logior (ash index 32) R_X86_64_64) 
                               (endianness little))  ; r_info
          (bytevector-u64-set! table 
                               (+ entry-offset R_ADDEND_OFFSET) 
                               0 
                               (endianness little))  ; r_addend
          (loop (cdr symbols) (+ index 1)))))))
