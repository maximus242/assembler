(define-module (section-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:use-module (string-table)
  #:use-module (srfi srfi-9)
  #:use-module (config)
  #:export (create-section-headers))

;; Define a record type for section headers
(define-record-type <section-header>
  (make-section-header name type flags addr offset size link info align entsize)
  section-header?
  (name sh-name)
  (type sh-type)
  (flags sh-flags)
  (addr sh-addr)
  (offset sh-offset)
  (size sh-size)
  (link sh-link)
  (info sh-info)
  (align sh-align)
  (entsize sh-entsize))

;; Main function to create section headers
(define (create-section-headers
          text-addr code-size data-size symtab-size strtab-size shstrtab-size
          dynsym-size dynstr-size rela-size total-dynamic-size dynamic-size
          rela-offset got-size data-addr dynamic-addr dynsym-addr dynstr-addr
          rela-addr got-addr plt-addr symtab-offset strtab-offset shstrtab-addr
          plt-size plt-got-addr plt-got-size rela-plt-addr rela-plt-size
          got-plt-addr got-plt-size rodata-offset gnu-version-addr gnu-version-r-addr
          gnu-version-size gnu-version-r-size gnu-version-d-offset gnu-version-d-size 
          hash-offset hash-size)

  (let ((headers
          (list
            ;; Null section
            (make-section-header 0 sht-null 0 0 0 0 0 0 0 0)

            ;; .text section
            (make-section-header 1 sht-progbits (logior shf-alloc shf-execinstr)
                                 text-addr text-addr code-size 0 0 16 0)

            ;; .data section
            (make-section-header 7 sht-progbits (logior shf-write shf-alloc)
                                 data-addr data-addr data-size 0 0 8 0)

            ;; .bss section
            (make-section-header 13 sht-nobits (logior shf-write shf-alloc)
                                 (+ data-addr data-size) (+ data-addr data-size) 0 0 0 8 0)

            ;; .rodata section
            (make-section-header 18 sht-progbits shf-alloc
                                 rodata-offset rodata-offset 0 0 0 8 0)

            ;; .dynamic section
            (make-section-header 63 sht-dynamic (logior shf-write shf-alloc)
                                 dynamic-addr dynamic-addr dynamic-size 7 0 8 dynamic-entry-size)

            ;; .dynsym section
            (make-section-header 80 sht-dynsym shf-alloc
                                 dynsym-addr dynsym-addr dynsym-size 7 1 8 24)

            ;; .dynstr section
            (make-section-header 72 sht-strtab shf-alloc
                                 dynstr-addr dynstr-addr dynstr-size 0 0 1 0)

            ;; .rela.dyn section
            (make-section-header 88 sht-rela shf-alloc
                                 rela-addr rela-addr rela-size 6 0 14 24)

            ;; .got section
            (make-section-header 98 sht-progbits (logior shf-write shf-alloc)
                                 got-addr got-addr got-size 0 0 8 got-entry-size)

            ;; .plt section
            (make-section-header 103 sht-progbits (logior shf-alloc shf-execinstr)
                                 plt-addr plt-addr plt-size 0 0 16 16)

            ;; .symtab section
            (make-section-header 26 sht-symtab 0 0 symtab-offset symtab-size 12 1 8 24)

            ;; .strtab section
            (make-section-header 34 sht-strtab 0 0 strtab-offset strtab-size 0 0 1 0)

            ;; .shstrtab section
            (make-section-header 42 sht-strtab 0 0 shstrtab-addr shstrtab-size 0 0 1 0)

            ;; .rela.plt section
            (make-section-header 126 sht-rela shf-alloc
                                 rela-plt-addr rela-plt-addr rela-plt-size 6 16 14 24)

            ;; .plt.got section
            (make-section-header 108 sht-progbits (logior shf-alloc shf-execinstr)
                                 plt-got-addr plt-got-addr plt-got-size 0 0 8 8)

            ;; .got.plt section
            (make-section-header 117 sht-progbits (logior shf-write shf-alloc)
                                 got-plt-addr got-plt-addr got-plt-size 0 0 8 8)

            ;; .hash section
            (make-section-header 164 sht-hash shf-alloc
                                 hash-offset hash-offset hash-size 6 0 8 4)

            ;; .gnu.version section
            (make-section-header 136 #x6fffffff shf-alloc
                                 gnu-version-addr gnu-version-addr gnu-version-size 6 0 2 2)

            ;; .gnu.version_r section
            ;;(make-section-header 149 #x6ffffffe shf-alloc
            ;;                     gnu-version-r-addr gnu-version-r-addr gnu-version-r-size 7 1 4 0)
            ;; .gnu.version_d section
            (make-section-header 170 #x6ffffffd shf-alloc
                                 gnu-version-d-offset gnu-version-d-offset gnu-version-d-size 7 1 4 0)

          )))

    ;; Log the final section headers for debugging
    (log-section-headers headers)

    ;; Convert the section headers to a bytevector
    (section-headers->bytevector headers)))

;; Helper function to log the final section headers
(define (log-section-headers headers)
  (format #t "Final section headers:\n")
  (for-each
    (lambda (index header)
      (format #t "Section [~a]: name=0x~x, type=0x~x, flags=0x~x, addr=0x~x, offset=0x~x, size=0x~x, link=0x~x, info=0x~x, align=0x~x, entsize=0x~x\n"
              index
              (sh-name header) (sh-type header) (sh-flags header)
              (sh-addr header) (sh-offset header) (sh-size header)
              (sh-link header) (sh-info header) (sh-align header) (sh-entsize header)))
    (iota (length headers))
    headers))

;; Convert section headers to bytevector
(define (section-headers->bytevector headers)
  (let* ((bv (make-bytevector (* (length headers) section-header-size) 0)))
    (for-each
      (lambda (index header)
        (let ((base (* index section-header-size)))
          (bytevector-u32-set! bv (+ base 0) (sh-name header) (endianness little))
          (bytevector-u32-set! bv (+ base 4) (sh-type header) (endianness little))
          (bytevector-u64-set! bv (+ base 8) (sh-flags header) (endianness little))
          (bytevector-u64-set! bv (+ base 16) (sh-addr header) (endianness little))
          (bytevector-u64-set! bv (+ base 24) (sh-offset header) (endianness little))
          (bytevector-u64-set! bv (+ base 32) (sh-size header) (endianness little))
          (bytevector-u32-set! bv (+ base 40) (sh-link header) (endianness little))
          (bytevector-u32-set! bv (+ base 44) (sh-info header) (endianness little))
          (bytevector-u64-set! bv (+ base 48) (sh-align header) (endianness little))
          (bytevector-u64-set! bv (+ base 56) (sh-entsize header) (endianness little))))
      (iota (length headers))
      headers)
    bv))
