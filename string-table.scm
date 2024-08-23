(define-module (string-table)
               #:use-module (rnrs bytevectors)
               #:use-module (ice-9 format)
               #:export (create-string-table
                          create-section-header-string-table
                          string-table-offset
                          get-section-name-offset
                          string-match?))

(define (string-match? str bv offset)
  (let ((str-len (string-length str)))
    (and (<= (+ offset str-len) (bytevector-length bv))
         (let loop ((i 0))
           (or (= i str-len)
               (and (= (char->integer (string-ref str i))
                       (bytevector-u8-ref bv (+ offset i)))
                    (loop (+ i 1))))))))

(define* (create-string-table symbol-addresses label-positions #:optional (options '()))
  (let ((null-terminator-size (or (assoc-ref options 'null-terminator-size) 1)))
    (let* ((symbol-names 
            (if (hash-table? symbol-addresses)
                (hash-map->list (lambda (key value) (symbol->string key)) symbol-addresses)
                (map (lambda (pair) (symbol->string (car pair))) symbol-addresses)))
           (label-names
            (if (hash-table? label-positions)
                (hash-map->list (lambda (key value) (symbol->string key)) label-positions)
                (map (lambda (pair) (symbol->string (car pair))) label-positions)))
           (names (cons "" (append symbol-names label-names)))
           (total-length (apply + (map (lambda (name) (+ (string-length name) null-terminator-size)) names)))
           (table (make-bytevector total-length 0))
           (offset 0))
      (for-each (lambda (name)
                  (let ((len (string-length name)))
                    (bytevector-copy! (string->utf8 name) 0 table offset len)
                    (bytevector-u8-set! table (+ offset len) 0)
                    (set! offset (+ offset len null-terminator-size))))
                names)
      table)))


(define* (create-section-header-string-table #:optional (options '()))
  (let ((null-terminator-size (or (assoc-ref options 'null-terminator-size) 1))
        (section-names (or (assoc-ref options 'section-names) 
                           '("" ".text" ".data" ".bss" ".rodata" ".symtab" ".strtab"
                             ".shstrtab" ".rela.text" ".dynamic" ".dynstr" ".dynsym"
                             ".rela.dyn" ".got" ".plt" ".plt.got" ".got.plt" ".rela.plt"))))
    (let* ((total-length (apply + (map (lambda (s) (+ (string-length s) null-terminator-size)) section-names)))
           (table (make-bytevector total-length 0))
           (offset 0))
      (for-each (lambda (name)
                  (let ((len (string-length name)))
                    (bytevector-copy! (string->utf8 name) 0 table offset len)
                    (set! offset (+ offset len null-terminator-size))))
                section-names)
      table)))


(define (string-table-offset name string-table)
  (let loop ((offset 0))
    (if (>= offset (bytevector-length string-table))
      #f  ; String not found
      (if (string-match? name string-table offset)
        offset
        (loop (+ offset 1))))))

(define (get-section-name-offset shstrtab section-name)
  (string-table-offset section-name shstrtab))

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
                   (loop (cdr entries)
                         (+ index 1)
                         (+ str-offset (string-length name) null-terminator-size))))))))
