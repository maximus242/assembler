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
         (got-addr (+ rela-addr rela-size))
         (plt-addr (+ got-addr #x18))  ; Assuming a small GOT size
         (shstrtab-addr #x3f94)
         (shstrtab-size 108))

    ;; Null section
    (set-section-header! headers 0 0 0 0 0 0 0 0 0 0 0)

    ;; .text section
    (set-section-header! headers 1 1 1 6 text-addr text-addr code-size 0 0 16 0)

    ;; .data section
    (set-section-header! headers 2 7 1 3 data-addr data-addr data-size 0 0 8 0)

    ;; .bss section
    (set-section-header! headers 3 13 8 3 (+ data-addr data-size) (+ data-addr data-size) 0 0 0 8 0)

    ;; .rodata section
    (set-section-header! headers 4 18 1 2 (+ text-addr code-size) (+ text-addr code-size) 0 0 0 8 0)

    ;; .dynamic section
    (set-section-header! headers 5 63 6 3 dynamic-addr dynamic-addr dynamic-size 7 0 8 16)

    ;; .dynsym section
    (set-section-header! headers 6 80 11 2 dynsym-addr dynsym-addr dynsym-size 7 5 8 24)

    ;; .dynstr section
    (set-section-header! headers 7 72 3 2 dynstr-addr dynstr-addr dynstr-size 0 0 1 0)

    ;; .rela.dyn section
    (set-section-header! headers 8 88 4 2 rela-addr rela-addr rela-size 6 0 8 24)

    ;; .got section
    (set-section-header! headers 9 98 1 3 got-addr got-addr #x18 0 0 8 8)

    ;; .plt section
    (set-section-header! headers 10 103 1 6 plt-addr plt-addr #x20 0 0 16 16)

    ;; .symtab section
    (set-section-header! headers 11 26 2 0 0 (+ #x2000 code-size data-size) symtab-size 12 5 8 24)  ; sh_info = 1 for first global symbol

    ;; .strtab section
    (set-section-header! headers 12 34 3 0 0 (+ #x2000 code-size data-size symtab-size) strtab-size 0 0 1 0)

    ;; .shstrtab section
    (set-section-header! headers 13 42 3 0 0 shstrtab-addr shstrtab-size 0 0 1 0)

    (format #t "Dynamic section header: addr=0x~x, offset=0x~x, size=0x~x~%" dynamic-addr dynamic-addr dynamic-size)
    (format #t "Dynsym section header: addr=0x~x, offset=0x~x, size=0x~x~%" dynsym-addr dynsym-addr dynsym-size)
    (format #t "Dynstr section header: addr=0x~x, offset=0x~x, size=0x~x~%" dynstr-addr dynstr-addr dynstr-size)
    (format #t "Rela section header: addr=0x~x, offset=0x~x, size=0x~x~%" rela-addr rela-addr rela-size)

    headers))

;; Helper function to set a section header
(define (set-section-header! headers index name type flags addr offset size link info align entsize)
  (let ((base (* index 64)))
    (bytevector-u32-set! headers (+ base 0) name (endianness little))
    (bytevector-u32-set! headers (+ base 4) type (endianness little))
    (bytevector-u64-set! headers (+ base 8) flags (endianness little))
    (bytevector-u64-set! headers (+ base 16) addr (endianness little))
    (bytevector-u64-set! headers (+ base 24) offset (endianness little))
    (bytevector-u64-set! headers (+ base 32) size (endianness little))
    (bytevector-u32-set! headers (+ base 40) link (endianness little))
    (bytevector-u32-set! headers (+ base 44) info (endianness little))
    (bytevector-u64-set! headers (+ base 48) align (endianness little))
    (bytevector-u64-set! headers (+ base 56) entsize (endianness little))))
