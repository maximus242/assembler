(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  (let ((section (make-bytevector (* 9 16) 0)))  ; 9 entries including DT_NULL
    (bytevector-u64-set! section 0 5 (endianness little))  ; DT_STRTAB
    (bytevector-u64-set! section 8 dynstr-offset (endianness little))
    (bytevector-u64-set! section 16 6 (endianness little))  ; DT_SYMTAB
    (bytevector-u64-set! section 24 dynsym-offset (endianness little))
    (bytevector-u64-set! section 32 10 (endianness little))  ; DT_STRSZ
    (bytevector-u64-set! section 40 strtab-size (endianness little))
    (bytevector-u64-set! section 48 11 (endianness little))  ; DT_SYMENT
    (bytevector-u64-set! section 56 24 (endianness little))
    (bytevector-u64-set! section 64 7 (endianness little))  ; DT_RELA
    (bytevector-u64-set! section 72 rela-offset (endianness little))
    (bytevector-u64-set! section 80 8 (endianness little))  ; DT_RELASZ
    (bytevector-u64-set! section 88 rela-size (endianness little))
    (bytevector-u64-set! section 96 9 (endianness little))  ; DT_RELAENT
    (bytevector-u64-set! section 104 24 (endianness little))
    (bytevector-u64-set! section 112 0 (endianness little))  ; DT_NULL
    (bytevector-u64-set! section 120 0 (endianness little))
    section))
