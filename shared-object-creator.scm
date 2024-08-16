(define-module (shared-object-creator)
               #:use-module (config)
               #:use-module (elf-header)
               #:use-module (program-headers)
               #:use-module (section-headers)
               #:use-module (dynamic-section)
               #:use-module (symbol-table)
               #:use-module (string-table)
               #:use-module (utils)
               #:use-module (rnrs bytevectors)
               #:use-module (rnrs io ports)
               #:use-module (ice-9 format)
               #:use-module (relocation-table)
               #:use-module (elf-layout-calculator)
               #:use-module (elf-dynamic-calculator)
               #:export (create-shared-object
                        custom-assert
                        verify-dynamic-section
                        check-section-overlaps
                        verify-segment-contents))

(define (custom-assert condition message)
  (unless condition
    (error message)))

(define (print-relocation-table table)
  (let ((size (bytevector-length table)))
    (do ((i 0 (+ i 8)))
        ((>= i size))
      (let ((value (bytevector-u64-ref table i (endianness little))))
        (format #t "~8,'0x: ~16,'0x~%" i value)))))

(define (get-tag-name tag)
  (case tag
    ((1) "DT_NEEDED")
    ((5) "DT_STRTAB")
    ((6) "DT_SYMTAB")
    ((7) "DT_RELA")
    ((8) "DT_RELASZ")
    ((9) "DT_RELAENT")
    ((10) "DT_STRSZ")
    ((11) "DT_SYMENT")
    ((0) "DT_NULL")
    (else (format #f "Unknown tag: ~a" tag))))

(define (verify-dynamic-section dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  (let loop ((offset 0))
    (when (< offset (bytevector-length dynamic-section))
      (let* ((tag (bytevector-u64-ref dynamic-section offset (endianness little)))
             (value (bytevector-u64-ref dynamic-section (+ offset 8) (endianness little))))
        (case tag
          ((5) (custom-assert (= value dynstr-offset) "DT_STRTAB value mismatch"))
          ((6) (custom-assert (= value dynsym-offset) "DT_SYMTAB value mismatch"))
          ((7) (custom-assert (= value rela-offset) "DT_RELA value mismatch"))
          ((8) (custom-assert (= value rela-size) "DT_RELASZ value mismatch"))
          ((9) (custom-assert (= value 24) "DT_RELAENT value mismatch"))
          ((10) (custom-assert (= value strtab-size) "DT_STRSZ value mismatch"))
          ((11) (custom-assert (= value 24) "DT_SYMENT value mismatch")))
        (if (not (= tag 0))
            (loop (+ offset 16)))))))

(define (check-section-overlaps sections)
  (let loop ((remaining sections))
    (when (> (length remaining) 1)
      (let* ((current (car remaining))
             (others (cdr remaining)))
        (for-each
          (lambda (other)
            (custom-assert (or (<= (+ (car current) (cadr current)) (car other))
                               (>= (car current) (+ (car other) (cadr other))))
                           (format #f "Section overlap: ~a and ~a" (caddr current) (caddr other))))
          others)
        (loop (cdr remaining))))))

(define (verify-segment-contents data-segment-start data-segment-end dynstr-offset dynsym-offset rela-offset relocation-table-size)
  (custom-assert (<= data-segment-start dynstr-offset data-segment-end) ".dynstr not in LOAD segment")
  (custom-assert (<= data-segment-start dynsym-offset data-segment-end) ".dynsym not in LOAD segment")
  (custom-assert (<= data-segment-start rela-offset data-segment-end) ".rela.dyn not in LOAD segment")
  (custom-assert (<= (+ rela-offset relocation-table-size) data-segment-end) ".rela.dyn exceeds LOAD segment"))

(define (create-shared-object code data-sections output-file symbol-addresses label-positions)
  (let* ((layout (calculate-elf-layout code data-sections symbol-addresses))
         (program-headers-offset (assoc-ref layout 'program-headers-offset))
         (code-size (assoc-ref layout 'code-size))
         (data-size (assoc-ref layout 'data-size))
         (symtab-size (assoc-ref layout 'symtab-size))
         (strtab-size (assoc-ref layout 'strtab-size))
         (shstrtab-size (assoc-ref layout 'shstrtab-size))
         (dynamic-symbol-table-size (assoc-ref layout 'dynamic-symbol-table-size))
         (relocation-table-size (assoc-ref layout 'relocation-table-size))
         (got-size (assoc-ref layout 'got-size))
         (data-offset (assoc-ref layout 'data-offset))
         (dynamic-offset (assoc-ref layout 'dynamic-offset))
         (dynamic-size (assoc-ref layout 'dynamic-size))
         (dynsym-offset (assoc-ref layout 'dynsym-offset))
         (dynstr-offset (assoc-ref layout 'dynstr-offset))
         (rodata-size (assoc-ref layout 'rodata-size))
         (rela-offset (assoc-ref layout 'rela-offset))
         (got-offset (assoc-ref layout 'got-offset))
         (plt-offset (assoc-ref layout 'plt-offset))
         (total-dynamic-size (assoc-ref layout 'total-dynamic-size))
         (section-headers-offset (assoc-ref layout 'section-headers-offset))
         (shstrtab-addr (assoc-ref layout 'shstrtab-addr))
         (total-size (assoc-ref layout 'total-size))
         (data-addr (assoc-ref layout 'data-addr))
         (dynamic-addr (assoc-ref layout 'dynamic-addr))
         (bss-size (assoc-ref layout 'bss-size))
         (plt-size (assoc-ref layout 'plt-size))
         (hash-offset (assoc-ref layout 'hash-offset))
         (hash-size (assoc-ref layout 'hash-size))
         (symtab-offset (assoc-ref layout 'symtab-offset))
         (strtab-offset (assoc-ref layout 'strtab-offset))
         (symtab-and-strtab (create-symbol-table symbol-addresses))
         (symtab (car symtab-and-strtab))
         (strtab (cdr symtab-and-strtab))
         (shstrtab (create-section-header-string-table))
         (dynamic-symbol-table (create-dynamic-symbol-table symbol-addresses))
         (relocation-table (create-relocation-table symbol-addresses))
         (dynamic-symbol-table (create-dynamic-symbol-table symbol-addresses))
         (print-relocation-table relocation-table)
         (print-relocation-table relocation-table)
         (dynamic-section (create-dynamic-section
                            dynstr-offset
                            dynsym-offset
                            strtab-size
                            dynamic-symbol-table-size
                            rela-offset
                            relocation-table-size
                            got-offset
                            hash-offset))
         (section-headers (create-section-headers
                            text-addr
                            code-size
                            data-size
                            symtab-size
                            strtab-size
                            shstrtab-size
                            dynamic-symbol-table-size
                            strtab-size
                            relocation-table-size
                            total-dynamic-size
                            dynamic-size
                            rela-offset
                            got-size
                            (assoc-ref layout 'data-addr)
                            (assoc-ref layout 'dynamic-addr)
                            (assoc-ref layout 'dynsym-addr)
                            (assoc-ref layout 'dynstr-addr)
                            (assoc-ref layout 'rela-addr)
                            (assoc-ref layout 'got-addr)
                            (assoc-ref layout 'plt-addr)
                            symtab-offset
                            strtab-offset
                            shstrtab-addr))
         (program-headers (create-program-headers 
                            elf-header-size
                            program-header-size
                            num-program-headers
                            text-addr
                            code-size
                            rodata-size
                            bss-size
                            data-size
                            dynamic-addr
                            dynamic-offset
                            dynamic-size
                            got-offset
                            got-size
                            plt-offset
                            plt-size
                            total-size
                            alignment))
         (program-headers-size (bytevector-length program-headers))
         (num-program-headers (/ program-headers-size program-header-size))
         (section-headers-size (* num-sections section-header-size))
         (total-size (+ section-headers-offset section-headers-size))
         (text-section-offset (calculate-text-section-offset elf-header-size program-headers-size))
         (entry-point (calculate-entry-point text-addr text-section-offset))
         (elf-header (create-elf-header
                       entry-point
                       program-headers-offset
                       program-headers-size
                       section-headers-offset
                       num-program-headers
                       num-sections
                       total-size
                       shstrtab-index
                       hash-offset
                       hash-size)))

    (verify-dynamic-section dynamic-section dynstr-offset dynsym-offset strtab-size 
                            dynamic-symbol-table-size rela-offset relocation-table-size)

    (let* ((data-segment-start data-offset)
           (data-segment-end (+ dynamic-offset total-dynamic-size))
           (data-segment-size (- data-segment-end data-segment-start)))

      (verify-segment-contents data-segment-start data-segment-end 
                               dynstr-offset dynsym-offset rela-offset relocation-table-size))

    (check-section-overlaps
      (list (list dynamic-offset dynamic-size ".dynamic")
            (list dynstr-offset strtab-size ".dynstr")
            (list dynsym-offset dynamic-symbol-table-size ".dynsym")
            (list rela-offset relocation-table-size ".rela.dyn")))

    (let ((elf-file (make-bytevector total-size 0)))
      (bytevector-copy! elf-header 0 elf-file 0 (bytevector-length elf-header))
      (bytevector-copy! program-headers 0 elf-file program-headers-offset program-headers-size)
      (bytevector-copy! code 0 elf-file code-offset code-size)

      (for-each (lambda (pair)
                  (bytevector-copy! (cdr pair) 0 elf-file data-offset (bytevector-length (cdr pair)))
                  (set! data-offset (+ data-offset (bytevector-length (cdr pair)))))
                data-sections)

      (bytevector-copy! dynamic-section 0 elf-file dynamic-offset dynamic-size)
      (bytevector-copy! strtab 0 elf-file dynstr-offset strtab-size)
      (bytevector-copy! dynamic-symbol-table 0 elf-file dynsym-offset dynamic-symbol-table-size)
      (bytevector-copy! relocation-table 0 elf-file rela-offset relocation-table-size)
      
      ;; Add .symtab section
      (bytevector-copy! symtab 0 elf-file symtab-offset (bytevector-length symtab))
      
      ;; Add .strtab section
      (bytevector-copy! strtab 0 elf-file strtab-offset (bytevector-length strtab))
      
      (bytevector-copy! shstrtab 0 elf-file (- section-headers-offset shstrtab-size) shstrtab-size)
      (bytevector-copy! section-headers 0 elf-file section-headers-offset section-headers-size)

      (call-with-output-file output-file
                             (lambda (port)
                               (put-bytevector port elf-file)))

      total-size)))