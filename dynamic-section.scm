(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size got-offset)
  (let ((section (make-bytevector (* 10 16) 0)))  ; 10 entries including DT_NULL and DT_PLTGOT
    ;; DT_STRTAB
    (bytevector-u64-set! section (* 0 16) 5 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 0 16) 8) dynstr-offset (endianness little))  ; value
    
    ;; DT_SYMTAB
    (bytevector-u64-set! section (* 1 16) 6 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 1 16) 8) dynsym-offset (endianness little))  ; value
    
    ;; DT_STRSZ
    (bytevector-u64-set! section (* 2 16) 10 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 2 16) 8) strtab-size (endianness little))  ; value
    
    ;; DT_SYMENT
    (bytevector-u64-set! section (* 3 16) 11 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 3 16) 8) 24 (endianness little))  ; value
    
    ;; DT_RELA
    (bytevector-u64-set! section (* 4 16) 7 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 4 16) 8) rela-offset (endianness little))  ; value
    
    ;; DT_RELASZ
    (bytevector-u64-set! section (* 5 16) 8 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 5 16) 8) rela-size (endianness little))  ; value
    
    ;; DT_RELAENT
    (bytevector-u64-set! section (* 6 16) 9 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 6 16) 8) 24 (endianness little))  ; value
    
    ;; DT_PLTGOT (new entry)
    (bytevector-u64-set! section (* 7 16) 3 (endianness little))  ; tag (3 is DT_PLTGOT)
    (bytevector-u64-set! section (+ (* 7 16) 8) got-offset (endianness little))  ; value
    
    ;; DT_NULL (end of dynamic section)
    (bytevector-u64-set! section (* 8 16) 0 (endianness little))  ; tag
    (bytevector-u64-set! section (+ (* 8 16) 8) 0 (endianness little))  ; value
    
    section))
