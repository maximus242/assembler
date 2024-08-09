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

(define (create-section-headers code-size data-size symtab-size strtab-size shstrtab-size 
                                dynsym-size dynstr-size rela-size total-dynamic-size 
                                dynamic-size rela-offset got-size)
  (let* ((data-addr (+ text-addr (align-to code-size alignment)))
         (dynamic-addr (align-to (+ data-addr data-size) alignment))
         (dynsym-addr (+ dynamic-addr dynamic-size))
         (dynstr-addr (+ dynsym-addr dynsym-size))
         (rela-addr rela-offset)
         (got-addr (align-to (+ rela-addr rela-size) 8))
         (plt-addr (+ got-addr got-size))
         (symtab-offset (+ section-offset code-size data-size))
         (strtab-offset (+ symtab-offset symtab-size)))

    (log-addresses-and-sizes text-addr data-addr dynamic-addr dynsym-addr 
                             dynstr-addr rela-addr got-addr plt-addr 
                             shstrtab-addr shstrtab-size got-size
                             code-size data-size symtab-size strtab-size
                             dynsym-size dynstr-size rela-size total-dynamic-size 
                             dynamic-size rela-offset)

    (let ((headers
           (list
            (make-section-header 0 sht-null 0 0 0 0 0 0 0 0)
            (make-section-header 1 sht-progbits (logior shf-alloc shf-execinstr) 
                                 text-addr text-addr code-size 0 0 16 0)
            (make-section-header 7 sht-progbits (logior shf-write shf-alloc) 
                                 data-addr data-addr data-size 0 0 8 0)
            (make-section-header 13 sht-nobits (logior shf-write shf-alloc) 
                                 (+ data-addr data-size) (+ data-addr data-size) 0 0 0 8 0)
            (make-section-header 18 sht-progbits shf-alloc 
                                 (+ text-addr code-size) (+ text-addr code-size) 0 0 0 8 0)
            (make-section-header 63 sht-dynamic (logior shf-write shf-alloc) 
                                 dynamic-addr dynamic-addr dynamic-size 7 0 8 dynamic-entry-size)
            (make-section-header 80 sht-dynsym shf-alloc 
                                 dynsym-addr dynsym-addr dynsym-size 7 5 8 24)
            (make-section-header 72 sht-strtab shf-alloc 
                                 dynstr-addr dynstr-addr dynstr-size 0 0 1 0)
            (make-section-header 88 sht-rela shf-alloc 
                                 rela-addr rela-addr rela-size 6 0 8 24)
            (make-section-header 98 sht-progbits (logior shf-write shf-alloc) 
                                 got-addr got-addr got-size 0 0 8 got-entry-size)
            (make-section-header 103 sht-progbits (logior shf-alloc shf-execinstr) 
                                 plt-addr plt-addr #x20 0 0 16 16)
            (make-section-header 26 sht-symtab 0 0 
                                 symtab-offset symtab-size 12 5 8 24)
            (make-section-header 34 sht-strtab 0 0 
                                 strtab-offset strtab-size 0 0 1 0)
            (make-section-header 42 sht-strtab 0 0 
                                 shstrtab-addr shstrtab-size 0 0 1 0))))

      (log-section-headers headers)
      (section-headers->bytevector headers))))

(define (log-addresses-and-sizes text-addr data-addr dynamic-addr dynsym-addr 
                                 dynstr-addr rela-addr got-addr plt-addr 
                                 shstrtab-addr shstrtab-size got-size
                                 code-size data-size symtab-size strtab-size
                                 dynsym-size dynstr-size rela-size total-dynamic-size 
                                 dynamic-size rela-offset)
  (format #t "Computed addresses and sizes:\n")
  (for-each (lambda (name value)
              (format #t "  ~a=0x~x\n" name value))
            '(text-addr data-addr dynamic-addr dynsym-addr dynstr-addr 
              rela-addr got-addr plt-addr shstrtab-addr shstrtab-size got-size
              code-size data-size symtab-size strtab-size dynsym-size dynstr-size
              rela-size total-dynamic-size dynamic-size rela-offset)
            (list text-addr data-addr dynamic-addr dynsym-addr dynstr-addr 
                  rela-addr got-addr plt-addr shstrtab-addr shstrtab-size got-size
                  code-size data-size symtab-size strtab-size dynsym-size dynstr-size
                  rela-size total-dynamic-size dynamic-size rela-offset)))

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