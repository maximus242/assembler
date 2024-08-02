(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section strtab-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  (let ((dynamic-entries (make-bytevector 160 0))) ; 10 entries * 16 bytes each
    ; DT_STRTAB
    (bytevector-u64-set! dynamic-entries 0 5 (endianness little))
    (bytevector-u64-set! dynamic-entries 8 strtab-offset (endianness little))
    
    ; DT_SYMTAB
    (bytevector-u64-set! dynamic-entries 16 6 (endianness little))
    (bytevector-u64-set! dynamic-entries 24 dynsym-offset (endianness little))
    
    ; DT_STRSZ
    (bytevector-u64-set! dynamic-entries 32 10 (endianness little))
    (bytevector-u64-set! dynamic-entries 40 strtab-size (endianness little))
    
    ; DT_SYMENT
    (bytevector-u64-set! dynamic-entries 48 11 (endianness little))
    (bytevector-u64-set! dynamic-entries 56 24 (endianness little)) ; Size of Elf64_Sym
    
    ; DT_RELA
    (bytevector-u64-set! dynamic-entries 64 7 (endianness little))
    (bytevector-u64-set! dynamic-entries 72 rela-offset (endianness little))
    
    ; DT_RELASZ
    (bytevector-u64-set! dynamic-entries 80 8 (endianness little))
    (bytevector-u64-set! dynamic-entries 88 rela-size (endianness little))
    
    ; DT_RELAENT
    (bytevector-u64-set! dynamic-entries 96 9 (endianness little))
    (bytevector-u64-set! dynamic-entries 104 24 (endianness little)) ; Size of Elf64_Rela
    
    ; DT_FLAGS_1 (optional, for example)
    (bytevector-u64-set! dynamic-entries 112 #x6ffffffb (endianness little))
    (bytevector-u64-set! dynamic-entries 120 1 (endianness little)) ; DF_1_NOW

    ; DT_NULL (always last)
    (bytevector-u64-set! dynamic-entries 128 0 (endianness little))
    (bytevector-u64-set! dynamic-entries 136 0 (endianness little))
    
    dynamic-entries))
