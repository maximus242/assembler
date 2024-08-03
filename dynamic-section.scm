(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  (let ((dynamic-section (make-bytevector 176 0)))  ; Increased size to accommodate all entries
    ;; DT_NEEDED (assuming we need libc.so.6, adjust as necessary)
    (bytevector-u64-set! dynamic-section 0 1 (endianness little))
    (bytevector-u64-set! dynamic-section 8 1 (endianness little))  ; Offset in dynstr for "libc.so.6"
    
    ;; DT_STRTAB
    (bytevector-u64-set! dynamic-section 16 5 (endianness little))
    (bytevector-u64-set! dynamic-section 24 dynstr-offset (endianness little))
    
    ;; DT_SYMTAB
    (bytevector-u64-set! dynamic-section 32 6 (endianness little))
    (bytevector-u64-set! dynamic-section 40 dynsym-offset (endianness little))
    
    ;; DT_RELA
    (bytevector-u64-set! dynamic-section 48 7 (endianness little))
    (bytevector-u64-set! dynamic-section 56 rela-offset (endianness little))
    
    ;; DT_RELASZ
    (bytevector-u64-set! dynamic-section 64 8 (endianness little))
    (bytevector-u64-set! dynamic-section 72 rela-size (endianness little))
    
    ;; DT_RELAENT
    (bytevector-u64-set! dynamic-section 80 9 (endianness little))
    (bytevector-u64-set! dynamic-section 88 24 (endianness little))  ; Size of Elf64_Rela
    
    ;; DT_STRSZ
    (bytevector-u64-set! dynamic-section 96 10 (endianness little))
    (bytevector-u64-set! dynamic-section 104 strtab-size (endianness little))
    
    ;; DT_SYMENT
    (bytevector-u64-set! dynamic-section 112 11 (endianness little))
    (bytevector-u64-set! dynamic-section 120 24 (endianness little))  ; Size of Elf64_Sym
    
    ;; DT_DEBUG (optional, for dynamic linker)
    (bytevector-u64-set! dynamic-section 128 21 (endianness little))
    (bytevector-u64-set! dynamic-section 136 0 (endianness little))
    
    ;; DT_PLTGOT (if you have a GOT)
    (bytevector-u64-set! dynamic-section 144 3 (endianness little))
    (bytevector-u64-set! dynamic-section 152 0 (endianness little))  ; Fill in actual GOT address if you have one
    
    ;; DT_NULL (terminator)
    (bytevector-u64-set! dynamic-section 160 0 (endianness little))
    (bytevector-u64-set! dynamic-section 168 0 (endianness little))
    
    dynamic-section))