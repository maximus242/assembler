(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

;; Constants for dynamic section entry tags
(define DT_NULL    0)
(define DT_PLTGOT  3)
(define DT_HASH    4)
(define DT_STRTAB  5)
(define DT_SYMTAB  6)
(define DT_RELA    7)
(define DT_RELASZ  8)
(define DT_RELAENT 9)
(define DT_STRSZ   10)
(define DT_SYMENT  11)
(define DT_VERSYM  #x6ffffff0)
(define DT_VERNEED #x6ffffffe)
(define DT_VERNEEDNUM #x6fffffff)

;; Constants for sizes and offsets
(define ENTRY_SIZE 16)
(define VALUE_OFFSET 8)
(define SYMENT_SIZE 24)
(define RELAENT_SIZE 24)

;; Helper function to set a dynamic section entry
(define (set-dynamic-entry! section index tag value)
  (let ((offset (* index ENTRY_SIZE)))
    (bytevector-u64-set! section offset tag (endianness little))
    (bytevector-u64-set! section (+ offset VALUE_OFFSET) value (endianness little))))

(define (create-dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size 
                                rela-offset rela-size got-offset hash-offset
                                gnu-version-offset gnu-version-r-offset
                                gnu-version-r-size)
  (let* ((num-entries (if (> gnu-version-r-size 0) 13 11))
         (section (make-bytevector (* num-entries ENTRY_SIZE) 0)))
    (set-dynamic-entry! section 0 DT_HASH hash-offset)
    (set-dynamic-entry! section 1 DT_STRTAB dynstr-offset)
    (set-dynamic-entry! section 2 DT_SYMTAB dynsym-offset)
    (set-dynamic-entry! section 3 DT_STRSZ strtab-size)
    (set-dynamic-entry! section 4 DT_SYMENT SYMENT_SIZE)
    (set-dynamic-entry! section 5 DT_RELA rela-offset)
    (set-dynamic-entry! section 6 DT_RELASZ rela-size)
    (set-dynamic-entry! section 7 DT_RELAENT RELAENT_SIZE)
    (set-dynamic-entry! section 8 DT_PLTGOT got-offset)
    (set-dynamic-entry! section 9 DT_VERSYM gnu-version-offset)
    (if (> gnu-version-r-size 0)
        (begin
          (set-dynamic-entry! section 10 DT_VERNEED gnu-version-r-offset)
          (set-dynamic-entry! section 11 DT_VERNEEDNUM 1)
          (set-dynamic-entry! section 12 DT_NULL 0))
        (set-dynamic-entry! section 10 DT_NULL 0))
    section))
