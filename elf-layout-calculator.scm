(define-module (elf-layout-calculator)
  #:use-module (config)
  #:use-module (utils)
  #:use-module (rnrs bytevectors)
  #:use-module (symbol-table)
  #:use-module (string-table)
  #:use-module (relocation-table)
  #:export (calculate-elf-layout
            calculate-phdr-size
            calculate-text-segment-size
            calculate-text-segment-end
            calculate-data-segment-start
            calculate-data-segment-file-size
            calculate-data-segment-mem-size
            calculate-relro-size))

(define (align-up address alignment)
  (let ((remainder (modulo address alignment)))
    (if (zero? remainder)
        address
        (+ address (- alignment remainder)))))

(define (calculate-phdr-size num-program-headers program-header-size)
  (* num-program-headers program-header-size))

(define (calculate-text-segment-size code-size rodata-size plt-size)
  (+ code-size rodata-size plt-size))

(define (calculate-text-segment-end text-addr text-segment-size)
  (+ text-addr text-segment-size))

(define (calculate-data-segment-start text-segment-end alignment)
  (align-up text-segment-end alignment))

(define (calculate-data-segment-file-size total-size data-segment-start)
  (- total-size data-segment-start))

(define (calculate-data-segment-mem-size data-segment-file-size bss-size)
  (+ data-segment-file-size bss-size))

(define (calculate-relro-size got-offset data-segment-start)
  (- got-offset data-segment-start))

(define (calculate-program-headers-offset)
  elf-header-size)

(define (calculate-code-size code)
  (bytevector-length code))

(define (calculate-rodata-size)
  0)  ; You may need to implement this based on your actual data

(define (calculate-bss-size)
  0)  ; You may need to implement this based on your actual data

(define (calculate-data-size data-sections)
  (apply + (map (lambda (pair) (bytevector-length (cdr pair))) data-sections)))

(define (calculate-symtab-size symbol-addresses label-positions)
  (let* ((symbol-table (convert-old-format-to-new symbol-addresses label-positions))
         (dynsym-table (create-dynamic-symbol-table symbol-table))
         (size (bytevector-length dynsym-table)))
    size))

(define (calculate-strtab-size symbol-addresses)
  (bytevector-length (create-string-table symbol-addresses)))

(define (calculate-shstrtab-size)
  (bytevector-length (create-section-header-string-table)))

(define (calculate-dynamic-symbol-table-size symbol-addresses)
  (bytevector-length (create-dynamic-symbol-table symbol-addresses)))

(define (calculate-relocation-table-size symbol-addresses)
  (bytevector-length (create-relocation-table symbol-addresses)))

(define (calculate-got-size symbol-addresses)
  (* (length symbol-addresses) got-entry-size))

(define (calculate-plt-size symbol-addresses)
  (* (length symbol-addresses) plt-entry-size))

(define (calculate-data-offset code-size rodata-size)
  (align-to (+ code-offset code-size rodata-size) alignment))

(define (calculate-dynamic-offset data-offset data-size)
  (align-to (+ data-offset data-size) alignment))

(define (calculate-dynamic-size)
  (* word-size dynamic-entry-size))

(define (calculate-dynsym-offset dynamic-offset dynamic-size)
  (align-to (+ dynamic-offset dynamic-size) word-size))

(define (calculate-dynstr-offset dynsym-offset dynamic-symbol-table-size)
  (align-to (+ dynsym-offset dynamic-symbol-table-size) word-size))

(define (calculate-rela-offset dynstr-offset strtab-size)
  (align-to (+ dynstr-offset strtab-size) word-size))

(define (calculate-got-offset rela-offset relocation-table-size)
  (align-to (+ rela-offset relocation-table-size) word-size))

(define (calculate-plt-offset got-offset got-size)
  (align-to (+ got-offset got-size) double-word-size))

(define (calculate-total-dynamic-size plt-offset dynamic-offset)
  (- plt-offset dynamic-offset))

(define (calculate-section-headers-offset dynamic-offset total-dynamic-size)
  (align-to (+ dynamic-offset total-dynamic-size) alignment))

(define (calculate-hash-size symbol-addresses)
  (let ((num-symbols (length symbol-addresses)))
    (* 4 (+ 2 num-symbols))))  ; 2 for nbucket and nchain, then 4 bytes per symbol

(define (calculate-hash-offset dynamic-offset dynamic-size)
  (align-to (+ dynamic-offset dynamic-size) word-size))

(define (calculate-shstrtab-addr section-headers-offset shstrtab-size)
  (- section-headers-offset shstrtab-size))

(define (calculate-data-addr text-addr code-size rodata-size)
  (+ text-addr (align-to (+ code-size rodata-size) alignment)))

(define (calculate-dynamic-addr data-addr data-size)
  (align-to (+ data-addr data-size) alignment))

(define (calculate-dynsym-addr dynamic-addr dynamic-size)
  (+ dynamic-addr dynamic-size))

(define (calculate-dynstr-addr dynsym-addr dynamic-symbol-table-size)
  (+ dynsym-addr dynamic-symbol-table-size))

(define (calculate-rela-addr rela-offset)
  rela-offset)

(define (calculate-got-addr rela-addr relocation-table-size)
  (align-to (+ rela-addr relocation-table-size) word-size))

(define (calculate-plt-addr got-addr got-size)
  (+ got-addr got-size))

(define (calculate-symtab-offset section-offset code-size rodata-size data-size)
  (+ section-offset code-size rodata-size data-size))

(define (calculate-total-size section-headers-offset)
  (+ section-headers-offset (* num-sections section-header-size)))

(define (calculate-strtab-offset symtab-offset symtab-size)
  (+ symtab-offset symtab-size))

(define (calculate-elf-layout code data-sections symbol-addresses label-positions)
  (let* ((program-headers-offset (calculate-program-headers-offset))
         (code-size (calculate-code-size code))
         (rodata-size (calculate-rodata-size))
         (bss-size (calculate-bss-size))
         (data-size (calculate-data-size data-sections))
         (symtab-size (calculate-symtab-size symbol-addresses label-positions))
         (strtab-size (calculate-strtab-size symbol-addresses))
         (shstrtab-size (calculate-shstrtab-size))
         (dynamic-symbol-table-size (calculate-dynamic-symbol-table-size symbol-addresses))
         (relocation-table-size (calculate-relocation-table-size symbol-addresses))
         (got-size (calculate-got-size symbol-addresses))
         (plt-size (calculate-plt-size symbol-addresses))
         (data-offset (calculate-data-offset code-size rodata-size))
         (dynamic-offset (calculate-dynamic-offset data-offset data-size))
         (dynamic-size (calculate-dynamic-size))
         (dynsym-offset (calculate-dynsym-offset dynamic-offset dynamic-size))
         (dynstr-offset (calculate-dynstr-offset dynsym-offset dynamic-symbol-table-size))
         (rela-offset (calculate-rela-offset dynstr-offset strtab-size))
         (got-offset (calculate-got-offset rela-offset relocation-table-size))
         (plt-offset (calculate-plt-offset got-offset got-size))
         (total-dynamic-size (calculate-total-dynamic-size plt-offset dynamic-offset))
         (section-headers-offset (calculate-section-headers-offset dynamic-offset total-dynamic-size))
         (hash-size (calculate-hash-size symbol-addresses))
         (hash-offset (calculate-hash-offset dynamic-offset dynamic-size))
         (shstrtab-addr (calculate-shstrtab-addr section-headers-offset shstrtab-size))
         (data-addr (calculate-data-addr text-addr code-size rodata-size))
         (dynamic-addr (calculate-dynamic-addr data-addr data-size))
         (dynsym-addr (calculate-dynsym-addr dynamic-addr dynamic-size))
         (dynstr-addr (calculate-dynstr-addr dynsym-addr dynamic-symbol-table-size))
         (rela-addr (calculate-rela-addr rela-offset))
         (got-addr (calculate-got-addr rela-addr relocation-table-size))
         (plt-addr (calculate-plt-addr got-addr got-size))
         (symtab-offset (calculate-symtab-offset section-offset code-size rodata-size data-size))
         (total-size (calculate-total-size section-headers-offset))
         (strtab-offset (calculate-strtab-offset symtab-offset symtab-size)))

    (list
      (cons 'program-headers-offset program-headers-offset)
      (cons 'code-size code-size)
      (cons 'rodata-size rodata-size)
      (cons 'bss-size bss-size)
      (cons 'data-size data-size)
      (cons 'symtab-size symtab-size)
      (cons 'strtab-size strtab-size)
      (cons 'shstrtab-size shstrtab-size)
      (cons 'dynamic-symbol-table-size dynamic-symbol-table-size)
      (cons 'relocation-table-size relocation-table-size)
      (cons 'got-size got-size)
      (cons 'plt-size plt-size)
      (cons 'data-offset data-offset)
      (cons 'dynamic-offset dynamic-offset)
      (cons 'dynamic-size dynamic-size)
      (cons 'dynsym-offset dynsym-offset)
      (cons 'dynstr-offset dynstr-offset)
      (cons 'rela-offset rela-offset)
      (cons 'got-offset got-offset)
      (cons 'plt-offset plt-offset)
      (cons 'total-dynamic-size total-dynamic-size)
      (cons 'section-headers-offset section-headers-offset)
      (cons 'shstrtab-addr shstrtab-addr)
      (cons 'data-addr data-addr)
      (cons 'dynamic-addr dynamic-addr)
      (cons 'dynsym-addr dynsym-addr)
      (cons 'dynstr-addr dynstr-addr)
      (cons 'rela-addr rela-addr)
      (cons 'got-addr got-addr)
      (cons 'plt-addr plt-addr)
      (cons 'symtab-offset symtab-offset)
      (cons 'strtab-offset strtab-offset)
      (cons 'total-size total-size)
      (cons 'hash-size hash-size)
      (cons 'hash-offset hash-offset)
      (cons 'text-addr text-addr))))
