(define-module (section-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:use-module (string-table)
  #:use-module (srfi srfi-9)
  #:export (create-section-headers))

;; Define constants for section types and flags
(define SHT_NULL 0)
(define SHT_PROGBITS 1)
(define SHT_SYMTAB 2)
(define SHT_STRTAB 3)
(define SHT_RELA 4)
(define SHT_DYNAMIC 6)
(define SHT_NOBITS 8)
(define SHT_DYNSYM 11)

(define SHF_WRITE #x1)
(define SHF_ALLOC #x2)
(define SHF_EXECINSTR #x4)

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
  (let* ((text-addr #x1000)
         (data-addr (+ text-addr (align-to code-size #x1000)))
         (dynamic-addr (align-to (+ data-addr data-size) #x1000))
         (dynsym-addr (+ dynamic-addr dynamic-size))
         (dynstr-addr (+ dynsym-addr dynsym-size))
         (rela-addr rela-offset)
         (got-addr (align-to (+ rela-addr rela-size) 8))
         (plt-addr (+ got-addr got-size))
         (shstrtab-addr #x3f94)
         (shstrtab-size 108))

    (log-addresses-and-sizes text-addr data-addr dynamic-addr dynsym-addr 
                             dynstr-addr rela-addr got-addr plt-addr 
                             shstrtab-addr shstrtab-size got-size
                             code-size data-size symtab-size strtab-size
                             dynsym-size dynstr-size rela-size total-dynamic-size 
                             dynamic-size rela-offset)

    (let ((headers
           (list
            (make-section-header 0 SHT_NULL 0 0 0 0 0 0 0 0)
            (make-section-header 1 SHT_PROGBITS (logior SHF_ALLOC SHF_EXECINSTR) 
                                 text-addr text-addr code-size 0 0 16 0)
            (make-section-header 7 SHT_PROGBITS (logior SHF_WRITE SHF_ALLOC) 
                                 data-addr data-addr data-size 0 0 8 0)
            (make-section-header 13 SHT_NOBITS (logior SHF_WRITE SHF_ALLOC) 
                                 (+ data-addr data-size) (+ data-addr data-size) 0 0 0 8 0)
            (make-section-header 18 SHT_PROGBITS SHF_ALLOC 
                                 (+ text-addr code-size) (+ text-addr code-size) 0 0 0 8 0)
            (make-section-header 63 SHT_DYNAMIC (logior SHF_WRITE SHF_ALLOC) 
                                 dynamic-addr dynamic-addr dynamic-size 7 0 8 16)
            (make-section-header 80 SHT_DYNSYM SHF_ALLOC 
                                 dynsym-addr dynsym-addr dynsym-size 7 5 8 24)
            (make-section-header 72 SHT_STRTAB SHF_ALLOC 
                                 dynstr-addr dynstr-addr dynstr-size 0 0 1 0)
            (make-section-header 88 SHT_RELA SHF_ALLOC 
                                 rela-addr rela-addr rela-size 6 0 8 24)
            (make-section-header 98 SHT_PROGBITS (logior SHF_WRITE SHF_ALLOC) 
                                 got-addr got-addr got-size 0 0 8 8)
            (make-section-header 103 SHT_PROGBITS (logior SHF_ALLOC SHF_EXECINSTR) 
                                 plt-addr plt-addr #x20 0 0 16 16)
            (make-section-header 26 SHT_SYMTAB 0 0 
                                 (+ #x2000 code-size data-size) symtab-size 12 5 8 24)
            (make-section-header 34 SHT_STRTAB 0 0 
                                 (+ #x2000 code-size data-size symtab-size) strtab-size 0 0 1 0)
            (make-section-header 42 SHT_STRTAB 0 0 
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
  (let* ((section-header-size 64)
         (bv (make-bytevector (* (length headers) section-header-size) 0)))
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
