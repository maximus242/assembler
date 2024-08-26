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
               #:use-module (plt-section)
               #:use-module (rela-plt-section)
               #:use-module (got-plt-section)
               #:use-module (plt-got-section)
               #:export (create-shared-object
                          custom-assert
                          verify-dynamic-section
                          check-section-overlaps
                          verify-segment-contents))

(define (create-got-section got-size)
  (make-bytevector got-size 0))

(define (custom-assert condition message)
  (unless condition
    (error message)))

(define (print-relocation-table table)
  (let ((size (bytevector-length table)))
    (do ((i 0 (+ i 8)))
      ((>= i size))
      (let ((value (bytevector-u64-ref table i (endianness little))))
        (format #t "~8,'0x: ~16,'0x~%" i value)))))

(define (get-dynsym-indices symtab-hash)
  (let ((indices (make-hash-table)))
    (let loop ((index 0)
               (entries (hash-map->list cons symtab-hash)))
      (if (null? entries)
        indices
        (let* ((entry (car entries))
               (name (car entry))
               (value (cdr entry)))
          (hash-set! indices name index)
          (format #t "Symbol: ~a, Index: ~a~%" name index)
          (loop (+ index 1) (cdr entries)))))))

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

(define (create-gnu-version-r-section)
  (let ((bv (make-bytevector 16 0)))  ; Minimal size for one entry
    (bytevector-u16-set! bv 0 1 (endianness little))  ; version
    (bytevector-u16-set! bv 2 1 (endianness little))  ; cnt
    (bytevector-u32-set! bv 4 0 (endianness little))  ; file offset
    (bytevector-u32-set! bv 8 0 (endianness little))  ; hash
    (bytevector-u16-set! bv 12 0 (endianness little))  ; flags
    (bytevector-u16-set! bv 14 0 (endianness little))  ; other
    bv))

(define (create-gnu-version-section dynsym-count)
  (let ((version-section (make-bytevector (* 2 dynsym-count) 0)))
    (do ((i 0 (+ i 1)))
      ((= i dynsym-count) version-section)
      (bytevector-u16-set! version-section (* i 2) 
                           (if (= i 0) 0 1)  ; 0 for local, 1 for global
                           (endianness little)))))

(define (create-shared-object code data-sections output-file symbol-addresses label-positions)
  (let* ((layout (calculate-elf-layout code data-sections symbol-addresses label-positions))
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
         (rodata-size (assoc-ref layout 'rodata-size))
         (plt-size (assoc-ref layout 'plt-size))
         (section-headers-offset (assoc-ref layout 'section-headers-offset))
         (shstrtab-addr (assoc-ref layout 'shstrtab-addr))
         (dynamic-addr (assoc-ref layout 'dynamic-addr))
         (bss-size (assoc-ref layout 'bss-size))
         (symtab-offset (assoc-ref layout 'symtab-offset))
         (strtab-offset (assoc-ref layout 'strtab-offset))
         (symtab-hash (create-symbol-table symbol-addresses label-positions))
         (dynsym-indices (get-dynsym-indices symtab-hash))
         (symtab-and-strtab (create-dynamic-symbol-table symtab-hash))
         (symtab-bv (car symtab-and-strtab))
         (strtab (cdr symtab-and-strtab))
         (dynsym-size (bytevector-length symtab-bv))
         (dynstr-size (bytevector-length strtab))
         (rodata-offset #x2000)
         (dynamic-offset (align-to dynamic-addr word-size))
         (dynamic-size (assoc-ref layout 'dynamic-size))
         (rela-offset (align-to #x200 word-size))
         (relocation-table (create-relocation-table symtab-hash))
         (relocation-table-size (bytevector-length relocation-table))
         (hash-offset (align-to (+ rela-offset relocation-table-size) word-size))
         (hash-table (create-hash-section symtab-bv))
         (hash-size (bytevector-length hash-table))
         (gnu-version-offset (align-to (+ hash-offset hash-size) word-size))
         (gnu-version-r-offset (align-to (+ gnu-version-offset (* 2 (/ dynsym-size 24))) word-size))
         (gnu-version-r-size 0)  ; Since we're creating an empty .gnu.version_r section
         (got-offset (align-to (+ dynamic-offset dynamic-size) word-size))
         (got-plt-offset (align-to (+ got-offset got-size) word-size))
         (got-plt-size (* (+ (length (hash-map->list cons label-positions)) 3) 8))  ; 3 reserved entries + function entries
         (dynsym-offset (align-to (+ got-plt-offset got-plt-size) word-size))
         (dynstr-offset (align-to (+ dynsym-offset dynsym-size) word-size))
         (got-size (assoc-ref layout 'got-size))
         (plt-offset (+ text-addr code-size))
         (plt-section (create-plt-section label-positions got-offset))
         (plt-size (bytevector-length plt-section))
         (plt-got-offset (align-to (+ plt-offset plt-size) word-size))
         (plt-got-size #x0)  ; Adjust this size as needed
         (data-addr (align-to (+ got-plt-offset got-plt-size) word-size))
         (rela-plt-offset (align-to (+ rela-offset relocation-table-size) word-size))
         (rela-addr (+ dynamic-addr (- rela-offset dynamic-offset)))
         ;; Create .rela.plt section


         (rela-plt-section (create-rela-plt-section 
                             (hash-map->list cons label-positions)
                             got-plt-offset
                             dynsym-indices))
         (rela-plt-size (bytevector-length rela-plt-section))

         ;; Create .got.plt section
         (got-plt-section (create-got-plt-section 
                            (hash-map->list cons label-positions)
                            dynamic-addr
                            plt-offset))

         (total-dynamic-size (- (+ got-plt-offset got-plt-size) dynamic-offset))
         (data-segment-size (+ data-size total-dynamic-size))
         (shstrtab (create-section-header-string-table))
         (dynamic-section (create-dynamic-section
                            dynstr-offset
                            dynsym-offset
                            dynstr-size
                            dynsym-size
                            rela-offset
                            relocation-table-size
                            got-offset
                            hash-offset
                            gnu-version-offset
                            gnu-version-r-offset
                            gnu-version-r-size
                            plt-offset
                            plt-size
                            rela-plt-offset
                            rela-plt-size))
         (section-headers (create-section-headers
                            text-addr
                            code-size
                            data-size
                            symtab-size
                            strtab-size
                            shstrtab-size
                            dynsym-size
                            dynstr-size
                            relocation-table-size
                            total-dynamic-size
                            dynamic-size
                            rela-offset
                            got-size
                            data-addr
                            dynamic-addr
                            (+ dynamic-addr (- dynsym-offset dynamic-offset))
                            (+ dynamic-addr (- dynstr-offset dynamic-offset))
                            rela-addr
                            (+ dynamic-addr (- got-offset dynamic-offset))
                            (+ dynamic-addr (- plt-offset dynamic-offset))
                            symtab-offset
                            strtab-offset
                            shstrtab-addr
                            plt-size
                            (+ dynamic-addr (- plt-got-offset dynamic-offset))
                            plt-got-size
                            rela-plt-offset
                            rela-plt-size
                            (+ dynamic-addr (- got-plt-offset dynamic-offset))
                            got-plt-size
                            rodata-offset
                            (+ dynamic-addr (- gnu-version-offset dynamic-offset))
                            (+ dynamic-addr (- gnu-version-r-offset dynamic-offset))
                            (* 2 (/ dynsym-size 24))  ; gnu-version-size
                            gnu-version-r-size))
         (program-headers (create-program-headers 
                            elf-header-size
                            program-header-size
                            num-program-headers
                            text-addr
                            code-size
                            rodata-size
                            bss-size
                            data-segment-size
                            dynamic-addr
                            dynamic-offset
                            dynamic-size
                            total-dynamic-size
                            got-offset
                            got-size
                            plt-offset
                            plt-size
                            (+ data-addr data-segment-size)
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

    (let ((elf-file (make-bytevector total-size 0)))
      (bytevector-copy! elf-header 0 elf-file 0 (bytevector-length elf-header))
      (bytevector-copy! program-headers 0 elf-file program-headers-offset program-headers-size)
      (bytevector-copy! code 0 elf-file code-offset code-size)

      (bytevector-copy! dynamic-section 0 elf-file dynamic-offset dynamic-size)
      (bytevector-copy! symtab-bv 0 elf-file dynsym-offset dynsym-size)
      (bytevector-copy! strtab 0 elf-file dynstr-offset dynstr-size)

      ;; Create .gnu.version section
      ;; (let* ((dynsym-count (/ (bytevector-length symtab-bv) 24))  ; Assuming 24 bytes per symbol
      ;;       (gnu-version (create-gnu-version-section dynsym-count)))
      ;;  (bytevector-copy! gnu-version 0 elf-file gnu-version-offset (bytevector-length gnu-version)))

      ;; Create .gnu.version_r section (empty in this case)
      ;;(let ((gnu-version-r (create-gnu-version-r-section)))
      ;;  (bytevector-copy! gnu-version-r 0 elf-file gnu-version-r-offset (bytevector-length gnu-version-r)))

      (bytevector-copy! relocation-table 0 elf-file rela-offset relocation-table-size)
      (bytevector-copy! hash-table 0 elf-file hash-offset hash-size)

      ;; Add .symtab section
      (bytevector-copy! symtab-bv 0 elf-file symtab-offset dynsym-size)

      ;; Add .strtab section
      (bytevector-copy! strtab 0 elf-file strtab-offset dynstr-size)

      ;; Add .got section
      (let ((got-section (create-got-section got-size)))
        (bytevector-copy! got-section 0 elf-file got-offset got-size))

      ;; Add .plt section
      (bytevector-copy! plt-section 0 elf-file plt-offset plt-size)

      ;; Add .rela.plt section
      (bytevector-copy! rela-plt-section 0 elf-file rela-plt-offset rela-plt-size)

      ;; Add .got.plt section
      (bytevector-copy! got-plt-section 0 elf-file got-plt-offset got-plt-size)

      (bytevector-copy! shstrtab 0 elf-file (- section-headers-offset shstrtab-size) shstrtab-size)
      (bytevector-copy! section-headers 0 elf-file section-headers-offset section-headers-size)

      (call-with-output-file output-file
                             (lambda (port)
                               (put-bytevector port elf-file)))

      total-size)))
