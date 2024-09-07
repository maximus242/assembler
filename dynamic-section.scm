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

(define (add-padding-to-align section size desired-size)
  (let ((padding (- desired-size size)))
    (if (> padding 0)
        (let ((padded-section (make-bytevector (+ size padding) 0)))
          ;; Copy original section to the padded section
          (bytevector-copy! section 0 padded-section 0 size)
          padded-section)
        section)))

(define (create-dynamic-section
          dynstr-offset dynsym-offset strtab-size dynsym-size 
          rela-offset rela-size got-offset hash-offset
          gnu-version-offset gnu-version-d-offset gnu-version-d-size
          plt-offset plt-size jmprel-offset jmprel-size got-plt-offset)
  (let* ((num-entries 9)  ;; Adjusted for the new number of entries
         (section-size (* num-entries ENTRY_SIZE)) ;; Initial section size
         (section (make-bytevector section-size 0))
         (final-size #xE0)) ;; Desired size with padding (0xE0 in hex) TODO Refactor once working
    
    (format #t "Creating dynamic section with num-entries=~a~%" num-entries)
    
    ;; Log initial parameters (keeping this for consistency with the original function)
    (format #t "Parameters: jmprel-offset=~a, jmprel-size=~a~%" jmprel-offset jmprel-size)
    
    (set-dynamic-entry! section 0 DT_HASH hash-offset)
    (set-dynamic-entry! section 1 DT_STRTAB dynstr-offset)
    (set-dynamic-entry! section 2 DT_SYMTAB dynsym-offset)
    (set-dynamic-entry! section 3 DT_STRSZ strtab-size)
    (set-dynamic-entry! section 4 DT_SYMENT SYMENT_SIZE)
    (set-dynamic-entry! section 5 DT_RELA rela-offset)
    (set-dynamic-entry! section 6 DT_RELASZ rela-size)
    (set-dynamic-entry! section 7 DT_RELAENT RELAENT_SIZE)
    (set-dynamic-entry! section 8 DT_NULL 0)
    
    ;; Add padding to align the section to the final size (0xE0)
    (let ((padded-section (add-padding-to-align section section-size final-size)))
      (format #t "Dynamic section creation complete. Size with padding: ~a bytes~%" (bytevector-length padded-section))
      padded-section)))
