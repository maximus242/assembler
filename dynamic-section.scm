(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  (let ((dynamic-section (make-bytevector 144 0)))  ; 9 entries * 16 bytes each
    ;; DT_STRTAB
    (bytevector-u64-set! dynamic-section 0 5 (endianness little))
    (bytevector-u64-set! dynamic-section 8 dynstr-offset (endianness little))
    
    ;; DT_SYMTAB
    (bytevector-u64-set! dynamic-section 16 6 (endianness little))
    (bytevector-u64-set! dynamic-section 24 dynsym-offset (endianness little))
    
    ;; DT_STRSZ
    (bytevector-u64-set! dynamic-section 32 10 (endianness little))
    (bytevector-u64-set! dynamic-section 40 strtab-size (endianness little))
    
    ;; DT_SYMENT
    (bytevector-u64-set! dynamic-section 48 11 (endianness little))
    (bytevector-u64-set! dynamic-section 56 24 (endianness little))  ; Size of Elf64_Sym
    
    ;; DT_RELA
    (bytevector-u64-set! dynamic-section 64 7 (endianness little))
    (bytevector-u64-set! dynamic-section 72 rela-offset (endianness little))
    
    ;; DT_RELASZ
    (bytevector-u64-set! dynamic-section 80 8 (endianness little))
    (bytevector-u64-set! dynamic-section 88 rela-size (endianness little))
    
    ;; DT_RELAENT
    (bytevector-u64-set! dynamic-section 96 9 (endianness little))
    (bytevector-u64-set! dynamic-section 104 24 (endianness little))  ; Size of Elf64_Rela
    
    ;; DT_NULL (terminator)
    (bytevector-u64-set! dynamic-section 112 0 (endianness little))
    (bytevector-u64-set! dynamic-section 120 0 (endianness little))
    
    dynamic-section))
