(define-module (section-headers)
  #:use-module (rnrs bytevectors)
  #:export (create-section-headers))

(define (create-section-headers code-size data-size symtab-size strtab-size shstrtab-size)
  (let* ((num-sections 6)  ; .text, .data, .symtab, .strtab, .shstrtab, and a null section
         (section-header-size 64)
         (headers (make-bytevector (* num-sections section-header-size) 0)))
    ;; Null section
    (bytevector-u32-set! headers 0 0 (endianness little))
    
    ;; .text section
    (bytevector-u32-set! headers 64 1 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 68 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 72 6 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_EXECINSTR)
    (bytevector-u64-set! headers 80 #x1000 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 88 #x1000 (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 96 code-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 104 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 108 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 112 16 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 120 0 (endianness little))  ; sh_entsize
    
    ;; .data section
    (bytevector-u32-set! headers 128 7 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 132 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 136 3 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_WRITE)
    (bytevector-u64-set! headers 144 (+ #x1000 code-size) (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 152 (+ #x1000 code-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 160 data-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 168 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 172 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 176 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 184 0 (endianness little))  ; sh_entsize
    
    ;; .symtab section
    (bytevector-u32-set! headers 192 13 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 196 2 (endianness little))  ; sh_type (SHT_SYMTAB)
    (bytevector-u64-set! headers 200 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 208 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 216 (+ #x2000 code-size data-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 224 symtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 232 4 (endianness little))  ; sh_link (index of .strtab)
    (bytevector-u32-set! headers 236 5 (endianness little))  ; sh_info (one greater than the symbol table index of the last local symbol)
    (bytevector-u64-set! headers 240 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 248 24 (endianness little))  ; sh_entsize (size of a symbol table entry)
    
    ;; .strtab section
    (bytevector-u32-set! headers 256 21 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 260 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 264 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 272 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 280 (+ #x2000 code-size data-size symtab-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 288 strtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 296 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 300 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 304 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 312 0 (endianness little))  ; sh_entsize
    
    ;; .shstrtab section
    (bytevector-u32-set! headers 320 29 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 324 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 328 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 336 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 344 (+ #x2000 code-size data-size symtab-size strtab-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 352 shstrtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 360 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 364 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 368 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 376 0 (endianness little))  ; sh_entsize
    
    headers))