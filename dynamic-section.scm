(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section dynstr-offset dynsym-offset dynstr-size dynsym-size rela-offset rela-size)
  (let* ((num-entries 8)
         (entry-size 16)
         (section (make-bytevector (* num-entries entry-size) 0)))
    
    ;; DT_STRTAB
    (bytevector-u64-set! section 0 5 (endianness little))
    (bytevector-u64-set! section 8 dynstr-offset (endianness little))
    
    ;; DT_SYMTAB
    (bytevector-u64-set! section 16 6 (endianness little))
    (bytevector-u64-set! section 24 dynsym-offset (endianness little))
    
    ;; DT_STRSZ
    (bytevector-u64-set! section 32 10 (endianness little))
    (bytevector-u64-set! section 40 dynstr-size (endianness little))
    
    ;; DT_SYMENT
    (bytevector-u64-set! section 48 11 (endianness little))
    (bytevector-u64-set! section 56 24 (endianness little))
    
    ;; DT_RELA
    (bytevector-u64-set! section 64 7 (endianness little))
    (bytevector-u64-set! section 72 rela-offset (endianness little))
    
    ;; DT_RELASZ
    (bytevector-u64-set! section 80 8 (endianness little))
    (bytevector-u64-set! section 88 rela-size (endianness little))
    
    ;; DT_RELAENT
    (bytevector-u64-set! section 96 9 (endianness little))
    (bytevector-u64-set! section 104 24 (endianness little))
    
    ;; DT_NULL
    (bytevector-u64-set! section 112 0 (endianness little))
    (bytevector-u64-set! section 120 0 (endianness little))
    
    section))