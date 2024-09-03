(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io simple)
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
(define DT_INIT    12)
(define DT_FINI    13)
(define DT_JMPREL  23)
(define DT_PLTRELSZ 2)
(define DT_PLTREL  20)
(define DT_VERSYM  #x6ffffff0)
(define DT_VERDEF  #x6ffffffc)
(define DT_VERDEFNUM #x6ffffffd)

;; Constants for sizes and offsets
(define ENTRY_SIZE 16)
(define VALUE_OFFSET 8)
(define SYMENT_SIZE 24)
(define RELAENT_SIZE 24)

;; Helper function to set a dynamic section entry
(define (set-dynamic-entry! section index tag value)
  (let ((offset (* index ENTRY_SIZE)))
    (bytevector-u64-set! section offset tag (endianness little))
    (bytevector-u64-set! section (+ offset VALUE_OFFSET) value (endianness little))
    (format #t "Set entry: index=~a, tag=~a, value=~a~%" index tag value)))

(define (create-dynamic-section
          dynstr-offset dynsym-offset strtab-size dynsym-size 
          rela-offset rela-size got-offset hash-offset
          gnu-version-offset gnu-version-d-offset gnu-version-d-size
          plt-offset plt-size jmprel-offset jmprel-size got-plt-offset)
  (let* ((num-entries 19)  ;; Adjusted for all possible entries
         (section (make-bytevector (* num-entries ENTRY_SIZE) 0)))
    (format #t "Creating dynamic section with num-entries=~a~%" num-entries)
    
    ;; Log initial parameters
    (format #t "Parameters: jmprel-offset=~a, jmprel-size=~a~%" jmprel-offset jmprel-size)
    
    (set-dynamic-entry! section 0 DT_HASH hash-offset)
    (set-dynamic-entry! section 1 DT_STRTAB dynstr-offset)
    (set-dynamic-entry! section 2 DT_SYMTAB dynsym-offset)
    (set-dynamic-entry! section 3 DT_STRSZ strtab-size)
    (set-dynamic-entry! section 4 DT_SYMENT SYMENT_SIZE)
    (set-dynamic-entry! section 5 DT_RELA rela-offset)
    (set-dynamic-entry! section 6 DT_RELASZ rela-size)
    (set-dynamic-entry! section 7 DT_RELAENT RELAENT_SIZE)
    (set-dynamic-entry! section 8 DT_PLTGOT got-plt-offset)
    
    ;; Set DT_JMPREL and DT_PLTRELSZ
    (set-dynamic-entry! section 9 DT_PLTRELSZ jmprel-size)
    (set-dynamic-entry! section 10 DT_PLTREL DT_RELA)
    (set-dynamic-entry! section 11 DT_JMPREL jmprel-offset)
    
    ;; Set DT_INIT and DT_FINI to 0 if not used
    (set-dynamic-entry! section 12 DT_INIT #x1052)
    (set-dynamic-entry! section 13 DT_FINI 0)
    
    ;; Set GNU version entries
    (set-dynamic-entry! section 14 DT_VERSYM gnu-version-offset)
    (set-dynamic-entry! section 15 DT_VERDEF gnu-version-d-offset)
    (set-dynamic-entry! section 16 DT_VERDEFNUM 1)
    
    ;; Set DT_NULL to mark the end of the dynamic section
    (set-dynamic-entry! section 17 DT_NULL 0)
    
    ;; Final logging
    (format #t "Dynamic section creation complete.~%")
    section))
