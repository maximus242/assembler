(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  ;; Create a bytevector for the dynamic section (9 entries * 16 bytes each)
  (let ((dynamic-section (make-bytevector 144 0)))

    ;; DT_STRTAB: Points to the dynamic string table
    ;; Tag value: 5
    (bytevector-u64-set! dynamic-section 0 5 (endianness little))
    ;; Address of .dynstr section
    (bytevector-u64-set! dynamic-section 8 dynstr-offset (endianness little))
    
    ;; DT_SYMTAB: Points to the dynamic symbol table
    ;; Tag value: 6
    (bytevector-u64-set! dynamic-section 16 6 (endianness little))
    ;; Address of .dynsym section
    (bytevector-u64-set! dynamic-section 24 dynsym-offset (endianness little))
    
    ;; DT_STRSZ: Size of the dynamic string table
    ;; Tag value: 10
    (bytevector-u64-set! dynamic-section 32 10 (endianness little))
    ;; Size of .dynstr section
    (bytevector-u64-set! dynamic-section 40 strtab-size (endianness little))
    
    ;; DT_SYMENT: Size of a symbol table entry
    ;; Tag value: 11
    (bytevector-u64-set! dynamic-section 48 11 (endianness little))
    ;; Size of Elf64_Sym structure (24 bytes)
    (bytevector-u64-set! dynamic-section 56 24 (endianness little))
    
    ;; DT_RELA: Address of relocation table
    ;; Tag value: 7
    (bytevector-u64-set! dynamic-section 64 7 (endianness little))
    ;; Address of .rela.dyn section
    (bytevector-u64-set! dynamic-section 72 rela-offset (endianness little))
    
    ;; DT_RELASZ: Total size of relocation table
    ;; Tag value: 8
    (bytevector-u64-set! dynamic-section 80 8 (endianness little))
    ;; Size of .rela.dyn section
    (bytevector-u64-set! dynamic-section 88 rela-size (endianness little))
    
    ;; DT_RELAENT: Size of a relocation table entry
    ;; Tag value: 9
    (bytevector-u64-set! dynamic-section 96 9 (endianness little))
    ;; Size of Elf64_Rela structure (24 bytes)
    (bytevector-u64-set! dynamic-section 104 24 (endianness little))
    
    ;; DT_NULL: Marks the end of the dynamic section
    ;; Tag value: 0
    (bytevector-u64-set! dynamic-section 112 0 (endianness little))
    ;; Value (always 0 for DT_NULL)
    (bytevector-u64-set! dynamic-section 120 0 (endianness little))
    
    ;; Return the completed dynamic section
    dynamic-section))
