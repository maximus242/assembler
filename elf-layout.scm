(define-module (elf-layout)
  #:use-module (config)
  #:use-module (utils)
  #:use-module (symbol-table)
  #:use-module (string-table)
  #:use-module (section-headers)
  #:use-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (relocation-table)  ; Added this line
  #:export (calculate-elf-layout
            program-headers-offset
            code-size
            data-size
            symtab
            symtab-size
            strtab
            strtab-size
            shstrtab
            shstrtab-size
            dynamic-symbol-table
            dynamic-symbol-table-size
            relocation-table
            relocation-table-size
            got-size
            data-offset
            dynamic-offset
            dynamic-size
            dynsym-offset
            dynstr-offset
            rela-offset
            got-offset
            plt-offset
            total-dynamic-size
            section-headers-offset
            program-headers
            program-headers-size
            num-program-headers
            section-headers-size
            total-size))

(define program-headers-offset #f)
(define code-size #f)
(define data-size #f)
(define symtab #f)
(define symtab-size #f)
(define strtab #f)
(define strtab-size #f)
(define shstrtab #f)
(define shstrtab-size #f)
(define dynamic-symbol-table #f)
(define dynamic-symbol-table-size #f)
(define relocation-table #f)
(define relocation-table-size #f)
(define got-size #f)
(define data-offset #f)
(define dynamic-offset #f)
(define dynamic-size #f)
(define dynsym-offset #f)
(define dynstr-offset #f)
(define rela-offset #f)
(define got-offset #f)
(define plt-offset #f)
(define total-dynamic-size #f)
(define section-headers-offset #f)
(define program-headers #f)
(define program-headers-size #f)
(define num-program-headers #f)
(define section-headers-size #f)
(define total-size #f)

(define (calculate-elf-layout code data-sections symbol-addresses)
  (set! program-headers-offset elf-header-size)
  (set! code-size (bytevector-length code))
  (set! data-size (apply + (map (lambda (pair) (bytevector-length (cdr pair))) data-sections)))
  (set! symtab (create-symbol-table symbol-addresses))
  (set! symtab-size (bytevector-length symtab))
  (set! strtab (create-string-table symbol-addresses))
  (set! strtab-size (bytevector-length strtab))
  (set! shstrtab (create-section-header-string-table))
  (set! shstrtab-size (bytevector-length shstrtab))
  (set! dynamic-symbol-table (create-dynamic-symbol-table symbol-addresses))
  (set! dynamic-symbol-table-size (bytevector-length dynamic-symbol-table))
  (set! relocation-table (create-relocation-table symbol-addresses))
  (set! relocation-table-size (bytevector-length relocation-table))
  (set! got-size (* (length symbol-addresses) got-entry-size))
  (set! data-offset (align-to (+ code-offset code-size) alignment))
  (set! dynamic-offset (align-to (+ data-offset data-size) alignment))
  (set! dynamic-size (* 8 dynamic-entry-size))
  (set! dynsym-offset (align-to (+ dynamic-offset dynamic-size) 8))
  (set! dynstr-offset (align-to (+ dynsym-offset dynamic-symbol-table-size) 8))
  (set! rela-offset (align-to (+ dynstr-offset strtab-size) 8))
  (set! got-offset (align-to (+ rela-offset relocation-table-size) 8))
  (set! plt-offset (align-to (+ got-offset got-size) 16))
  (set! total-dynamic-size (- plt-offset dynamic-offset))
  (set! section-headers-offset (align-to (+ dynamic-offset total-dynamic-size) alignment))
  (set! program-headers (create-program-headers
                         code-size data-size total-dynamic-size dynamic-offset dynamic-size
                         got-offset got-size))
  (set! program-headers-size (bytevector-length program-headers))
  (set! num-program-headers (/ program-headers-size program-header-size))
  (set! section-headers-size (* num-sections section-header-size))
  (set! total-size (+ section-headers-offset section-headers-size)))