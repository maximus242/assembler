(define-module (string-table)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (create-string-table
            create-section-header-string-table
            string-table-offset
            get-section-name-offset))

(define* (create-string-table symbol-addresses #:optional (options '()))
  (let ((null-terminator-size (or (assoc-ref options 'null-terminator-size) 1)))
    (let* ((names (cons "" (map (lambda (pair) (symbol->string (car pair))) symbol-addresses)))
           (total-length (apply + (map (lambda (name) (+ (string-length name) null-terminator-size)) names)))
           (table (make-bytevector total-length 0))
           (offset 0))
      (for-each (lambda (name)
                  (let ((len (string-length name)))
                    (bytevector-copy! (string->utf8 name) 0 table offset len)
                    (set! offset (+ offset len null-terminator-size))))
                names)
      table)))

(define* (create-section-header-string-table #:optional (options '()))
  (let ((null-terminator-size (or (assoc-ref options 'null-terminator-size) 1))
        (byte-size (or (assoc-ref options 'byte-size) 8))
        (hex-bytes-per-line (or (assoc-ref options 'hex-bytes-per-line) 16))
        (section-names (or (assoc-ref options 'section-names) 
                           '("" ".text" ".data" ".bss" ".rodata" ".symtab" ".strtab" ".shstrtab" 
                             ".rela.text" ".dynamic" ".dynstr" ".dynsym" ".rela.dyn" ".got" ".plt"))))
    (let* ((total-length (apply + (map (lambda (s) (+ (string-length s) null-terminator-size)) section-names)))
           (table (make-bytevector total-length 0))
           (offset 0))
      
      (format #t "Creating section header string table\n")
      (format #t "Total names: ~a\n" (length section-names))
      (format #t "Total length: ~a bytes\n" total-length)
      
      (for-each (lambda (name)
                  (let ((len (string-length name)))
                    (format #t "Adding name: '~a' at offset ~a\n" name offset)
                    (bytevector-copy! (string->utf8 name) 0 table offset len)
                    (set! offset (+ offset len null-terminator-size))))
                section-names)
      
      (format #t "Final offset: ~a\n" offset)
      (format #t "Table length: ~a\n" (bytevector-length table))
      
      ; Add a hexdump of the table
      (format #t "Table contents (hexdump):\n")
      (let ((hex-dump (with-output-to-string
                        (lambda ()
                          (let loop ((i 0))
                            (when (< i (bytevector-length table))
                              (format #t "~2,'0x " (bytevector-u8-ref table i))
                              (when (= (- hex-bytes-per-line 1) (modulo i hex-bytes-per-line))
                                (format #t "\n"))
                              (loop (+ i 1))))))))
        (format #t "~a\n" hex-dump))
      
      table)))

(define (string-table-offset name string-table)
  (let loop ((offset 0))
    (if (>= offset (bytevector-length string-table))
        (begin
          (format #t "String '~a' not found in string table\n" name)
          #f)  ; String not found
        (if (string-match? name string-table offset)
            (begin
              (format #t "String '~a' found at offset ~a\n" name offset)
              offset)
            (loop (+ offset 1))))))

(define (get-section-name-offset shstrtab section-name)
  (format #t "Looking for section name: '~a'\n" section-name)
  (let ((offset (string-table-offset section-name shstrtab)))
    (if offset
        (format #t "Section '~a' found at offset ~a\n" section-name offset)
        (format #t "Section '~a' not found in string table\n" section-name))
    offset))

;; New function to replace symbols->bytevector
(define* (symbols->bytevector entries #:optional (options '()))
  (let ((symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24))
        (st-name-offset (or (assoc-ref options 'st-name-offset) 0))
        (st-info-offset (or (assoc-ref options 'st-info-offset) 4))
        (st-other-offset (or (assoc-ref options 'st-other-offset) 5))
        (st-shndx-offset (or (assoc-ref options 'st-shndx-offset) 6))
        (st-value-offset (or (assoc-ref options 'st-value-offset) 8))
        (st-size-offset (or (assoc-ref options 'st-size-offset) 16))
        (initial-string-offset (or (assoc-ref options 'initial-string-offset) 1))
        (null-terminator-size (or (assoc-ref options 'null-terminator-size) 1)))
    (let* ((table-size (* (length entries) symbol-entry-size))
           (table (make-bytevector table-size 0))
           (string-table-offset initial-string-offset))
      (let loop ((entries entries)
                 (index 0)
                 (str-offset initial-string-offset))
        (if (null? entries)
            table
            (let* ((entry (car entries))
                   (name (symbol->string (car entry)))
                   (address (cdr entry))
                   (entry-offset (* index symbol-entry-size)))
              (bytevector-u32-set! table (+ entry-offset st-name-offset) str-offset (endianness little))
              (bytevector-u64-set! table (+ entry-offset st-value-offset) address (endianness little))
              (format #t "Added symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                      name address str-offset)
              (loop (cdr entries)
                    (+ index 1)
                    (+ str-offset (string-length name) null-terminator-size))))))))
