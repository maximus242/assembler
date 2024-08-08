(define-module (section-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:use-module (string-table)
  #:export (create-section-headers))

(define (create-section-headers code-size data-size symtab-size strtab-size shstrtab-size dynsym-size dynstr-size rela-size total-dynamic-size dynamic-size)
  (let* ((num-sections 14)
         (section-header-size 64)
         (headers (make-bytevector (* num-sections section-header-size) 0))
         (text-addr #x1000)
         (data-addr (+ text-addr (align-to code-size #x1000)))
         (dynamic-addr (align-to (+ data-addr data-size) #x1000))
         (dynsym-addr (+ dynamic-addr dynamic-size))
         (dynstr-addr (+ dynsym-addr dynsym-size))
         (rela-addr (+ dynstr-addr dynstr-size))
         (shstrtab-addr #x3f94)  ; Hard-coded address based on analysis
         (shstrtab-size 108))    ; Hard-coded size based on analysis

    ;; Null section
    (bytevector-u32-set! headers 0 0 (endianness little))

    ;; .text section
    (bytevector-u32-set! headers 64 1 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 68 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 72 6 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_EXECINSTR)
    (bytevector-u64-set! headers 80 text-addr (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 88 text-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 96 code-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 104 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 108 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 112 16 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 120 0 (endianness little))  ; sh_entsize

    ;; .data section
    (bytevector-u32-set! headers 128 7 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 132 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 136 3 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_WRITE)
    (bytevector-u64-set! headers 144 data-addr (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 152 data-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 160 data-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 168 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 172 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 176 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 184 0 (endianness little))  ; sh_entsize

    ;; .bss section
    (bytevector-u32-set! headers 192 13 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 196 8 (endianness little))  ; sh_type (SHT_NOBITS)
    (bytevector-u64-set! headers 200 3 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_WRITE)
    (bytevector-u64-set! headers 208 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 216 0 (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 224 0 (endianness little))  ; sh_size
    (bytevector-u32-set! headers 232 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 236 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 240 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 248 0 (endianness little))  ; sh_entsize

    ;; .rodata section
    (bytevector-u32-set! headers 256 18 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 260 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 264 2 (endianness little))  ; sh_flags (SHF_ALLOC)
    (bytevector-u64-set! headers 272 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 280 0 (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 288 0 (endianness little))  ; sh_size
    (bytevector-u32-set! headers 296 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 300 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 304 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 312 0 (endianness little))  ; sh_entsize

    ;; .symtab section
    (bytevector-u32-set! headers 320 26 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 324 2 (endianness little))  ; sh_type (SHT_SYMTAB)
    (bytevector-u64-set! headers 328 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 336 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 344 (+ #x2000 code-size data-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 352 symtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 360 6 (endianness little))  ; sh_link (index of .strtab)
    (bytevector-u32-set! headers 364 5 (endianness little))  ; sh_info (one greater than the symbol table index of the last local symbol)
    (bytevector-u64-set! headers 368 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 376 24 (endianness little))  ; sh_entsize (size of a symbol table entry)

    ;; .strtab section
    (bytevector-u32-set! headers 384 34 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 388 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 392 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 400 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 408 (+ #x2000 code-size data-size symtab-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 416 strtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 424 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 428 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 432 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 440 0 (endianness little))  ; sh_entsize

    ;; .shstrtab section
    (bytevector-u32-set! headers 448 42 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 452 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 456 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 464 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 472 shstrtab-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 480 shstrtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 488 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 492 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 496 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 504 0 (endianness little))  ; sh_entsize

    ;; .dynamic section (Section 8)
    (bytevector-u32-set! headers 512 52 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 516 6 (endianness little))  ; sh_type (SHT_DYNAMIC)
    (bytevector-u64-set! headers 520 3 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_WRITE)
    (bytevector-u64-set! headers 528 dynamic-addr (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 536 dynamic-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 544 dynamic-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 552 10 (endianness little))  ; sh_link (index of .dynstr section)
    (bytevector-u32-set! headers 556 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 560 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 568 16 (endianness little))  ; sh_entsize (size of each dynamic entry)

    ;; .dynsym section (Section 9)
    (bytevector-u32-set! headers 576 63 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 580 11 (endianness little))  ; sh_type (SHT_DYNSYM)
    (bytevector-u64-set! headers 584 2 (endianness little))  ; sh_flags (SHF_ALLOC)
    (bytevector-u64-set! headers 592 dynsym-addr (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 600 dynsym-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 608 dynsym-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 616 10 (endianness little))  ; sh_link (index of .dynstr section)
    (bytevector-u32-set! headers 620 1 (endianness little))  ; sh_info (index of first non-local symbol)
    (bytevector-u64-set! headers 624 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 632 24 (endianness little))  ; sh_entsize (size of each symbol entry)

    ;; .dynstr section (Section 10)
    (bytevector-u32-set! headers 640 72 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 644 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 648 2 (endianness little))  ; sh_flags (SHF_ALLOC)
    (bytevector-u64-set! headers 656 dynstr-addr (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 664 dynstr-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 672 dynstr-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 680 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 684 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 688 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 696 0 (endianness little))  ; sh_entsize

    ;; .rela.dyn section (Section 11)
    (bytevector-u32-set! headers 704 80 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 708 4 (endianness little))  ; sh_type (SHT_RELA)
    (bytevector-u64-set! headers 712 2 (endianness little))  ; sh_flags (SHF_ALLOC)
    (bytevector-u64-set! headers 720 rela-addr (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 728 rela-addr (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 736 rela-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 744 9 (endianness little))  ; sh_link (index of .dynsym section)
    (bytevector-u32-set! headers 748 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 752 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 760 24 (endianness little))  ; sh_entsize (size of each relocation entry)

    (format #t "Dynamic section header: addr=0x~x, offset=0x~x, size=0x~x~%"
            dynamic-addr dynamic-addr dynamic-size)
    (format #t "Dynsym section header: addr=0x~x, offset=0x~x, size=0x~x~%"
            dynsym-addr dynsym-addr dynsym-size)
    (format #t "Dynstr section header: addr=0x~x, offset=0x~x, size=0x~x~%"
            dynstr-addr dynstr-addr dynstr-size)
    (format #t "Rela section header: addr=0x~x, offset=0x~x, size=0x~x~%"
            rela-addr rela-addr rela-size)

    headers))
