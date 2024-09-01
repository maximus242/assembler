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
                          align-to
                          calculate-relro-size))

(define (align-to value alignment)
  (if (= (modulo value alignment) 0)
      value
      (+ value (- alignment (modulo value alignment)))))

(define (align-up address alignment)
  (let ((remainder (modulo address alignment)))
    (if (zero? remainder)
      address
      (+ address (- alignment remainder)))))

(define (calculate-code-offset elf-header-size program-headers-offset)
  (let ((offset (align-to (+ elf-header-size program-headers-offset) alignment)))
    (format #t "Calculated code offset: ~a~%" offset)
    offset))

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
  (let ((total-size 0))
    (for-each (lambda (pair)
                (let* ((name (car pair))
                       (data (cdr pair))
                       (size (bytevector-length data)))
                  (format #t "Data section ~a size: ~a bytes~%" name size)
                  (set! total-size (+ total-size size))))
              data-sections)
    (format #t "Total calculated data size: ~a bytes~%" total-size)
    total-size))

(define (calculate-symtab-size symbol-addresses label-positions)
  (let* ((symbol-table (create-symbol-table symbol-addresses label-positions))
         (dynsym-and-strtab (create-dynamic-symbol-table symbol-addresses label-positions))
         (size (bytevector-length (car dynsym-and-strtab))))
    size))

(define (calculate-strtab-size symbol-addresses label-positions)
  (let* ((symbol-table (create-symbol-table symbol-addresses label-positions))
         (dynsym-and-strtab (create-dynamic-symbol-table symbol-addresses label-positions))
         (size (bytevector-length (cdr dynsym-and-strtab))))
    size))

(define (calculate-shstrtab-size)
  (bytevector-length (create-section-header-string-table)))

(define (calculate-dynamic-symbol-table-size symbol-addresses)
  (let* ((symbol-table (create-symbol-table symbol-addresses '()))
         (dynsym-and-strtab (create-dynamic-symbol-table symbol-addresses '()))
         (size (bytevector-length (car dynsym-and-strtab))))
    size))

(define (calculate-relocation-table-size symbol-addresses)
  (bytevector-length (create-relocation-table symbol-addresses)))

(define (calculate-got-size symbol-addresses)
  (* (length symbol-addresses) got-entry-size 4))

(define (calculate-plt-size symbol-addresses)
  (* (length symbol-addresses) plt-entry-size))

(define (calculate-data-offset code-size rodata-size)
  (align-to (+ code-offset code-size rodata-size) alignment))

(define (calculate-dynamic-offset data-offset data-size)
  (align-to (+ data-offset data-size) alignment))

(define (calculate-dynamic-size)
  (let* ((calculated-size (* num-dynamic-entries dynamic-entry-size))
         (hex-size (number->string calculated-size 16)))
    (format #t "Calculating dynamic size:~%")
    (format #t "  Number of dynamic entries: ~a~%" num-dynamic-entries)
    (format #t "  Size of each dynamic entry: 0x~a bytes~%" 
            (number->string dynamic-entry-size 16))
    (format #t "  Calculated size: 0x~a bytes~%" hex-size)
    (format #t "  Decimal size: ~a bytes~%" calculated-size)
    calculated-size))

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

(define (calculate-total-dynamic-size dynamic-offset dynamic-size got-offset got-size plt-offset plt-size bss-size alignment)
  (format #t "Calculating total dynamic size:~%")
  (format #t "  Dynamic offset:        0x~8,'0x~%" dynamic-offset)
  (format #t "  Dynamic size:          0x~8,'0x (~a bytes)~%" dynamic-size dynamic-size)
  (format #t "  GOT offset:            0x~8,'0x~%" got-offset)
  (format #t "  GOT size:              0x~8,'0x (~a bytes)~%" got-size got-size)
  (format #t "  PLT offset:            0x~8,'0x~%" plt-offset)
  (format #t "  PLT size:              0x~8,'0x (~a bytes)~%" plt-size plt-size)
  (format #t "  BSS size:              0x~8,'0x (~a bytes)~%" bss-size bss-size)
  (let* ((last-offset (+ plt-offset plt-size))
         (total-size (- last-offset dynamic-offset))
         (aligned-size (align-up total-size alignment))
         (total-size-with-bss (+ aligned-size bss-size)))
    (format #t "Total dynamic size (without BSS):     0x~8,'0x (~a bytes)~%" total-size total-size)
    (format #t "Aligned total dynamic size:           0x~8,'0x (~a bytes)~%" aligned-size aligned-size)
    (format #t "Total dynamic size (with BSS):        0x~8,'0x (~a bytes)~%" total-size-with-bss total-size-with-bss)
    (format #t "Difference from actual (0x1f0):       0x~8,'0x (~a bytes)~%" 
            (- #x1f0 aligned-size) (- #x1f0 aligned-size))
    aligned-size))

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

(define (calculate-rela-addr dynamic-addr rela-offset dynamic-offset)
  (+ dynamic-addr (- rela-offset dynamic-offset)))

(define (calculate-got-addr dynamic-addr got-offset dynamic-offset)
  (+ dynamic-addr (- got-offset dynamic-offset)))

(define (calculate-plt-addr dynamic-addr plt-offset dynamic-offset)
  (+ dynamic-addr (- plt-offset dynamic-offset)))

(define (calculate-symtab-offset section-offset code-size rodata-size data-size)
  (+ section-offset code-size rodata-size data-size))

(define (calculate-total-size section-headers-offset)
  (+ section-headers-offset (* num-sections section-header-size)))

(define (calculate-strtab-offset symtab-offset symtab-size)
  (+ symtab-offset symtab-size))

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

(define (calculate-elf-layout code data-sections symbol-addresses label-positions)
  (let* ((program-headers-offset (calculate-program-headers-offset))
         (code-size (calculate-code-size code))
         (rodata-size (calculate-rodata-size))
         (bss-size (calculate-bss-size))
         (data-size (calculate-data-size data-sections))
         (symtab-size (calculate-symtab-size symbol-addresses label-positions))
         (strtab-size (calculate-strtab-size symbol-addresses label-positions))
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
         (symtab-hash (create-symbol-table symbol-addresses label-positions))
         (dynsym-indices (get-dynsym-indices symtab-hash))
         (symtab-and-strtab (create-dynamic-symbol-table symbol-addresses label-positions))
         (symtab-bv (car symtab-and-strtab))
         (strtab (cdr symtab-and-strtab))
         (dynsym-size (bytevector-length symtab-bv))
         (dynstr-size (bytevector-length strtab))
         (total-dynamic-size 
           (calculate-total-dynamic-size 
             dynamic-offset 
             dynamic-size 
             got-offset 
             got-size 
             plt-offset 
             plt-size 
             bss-size
             alignment))
         (section-headers-offset (calculate-section-headers-offset dynamic-offset total-dynamic-size))
         (hash-size (calculate-hash-size symbol-addresses))
         (hash-offset (calculate-hash-offset dynamic-offset dynamic-size))
         (shstrtab-addr (calculate-shstrtab-addr section-headers-offset shstrtab-size))
         (data-addr (calculate-data-addr text-addr code-size rodata-size))
         (dynamic-addr (calculate-dynamic-addr data-addr data-size))
         (dynsym-addr (calculate-dynsym-addr dynamic-addr dynamic-size))
         (dynstr-addr (calculate-dynstr-addr dynsym-addr dynamic-symbol-table-size))
         (rela-addr (calculate-rela-addr dynamic-addr rela-offset dynamic-offset))
         (got-addr (calculate-got-addr dynamic-addr got-offset dynamic-offset))
         (plt-addr (calculate-plt-addr dynamic-addr plt-offset dynamic-offset))
         (symtab-offset (calculate-symtab-offset section-offset code-size rodata-size data-size))
         (total-size (calculate-total-size section-headers-offset))
         (code-offset (calculate-code-offset elf-header-size program-headers-offset))
         (init-offset (align-to text-addr 16))
         (init-size 16)
         (fini-size 16)
         (fini-offset (align-to (+ init-offset init-size) 16))
         (strtab-offset (calculate-strtab-offset symtab-offset symtab-size)))

    (list
      (cons 'program-headers-offset program-headers-offset)
      (cons 'init-offset init-offset)
      (cons 'init-size init-size)
      (cons 'code-size code-size)
      (cons 'code-offset code-offset)
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
      (cons 'symtab-hash symtab-hash)
      (cons 'dynsym-indices dynsym-indices)
      (cons 'symtab-and-strtab symtab-and-strtab)
      (cons 'symtab-bv symtab-bv)
      (cons 'strtab strtab)
      (cons 'dynsym-size dynsym-size)
      (cons 'dynstr-size dynstr-size)
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
