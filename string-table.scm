(define-module (string-table)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (create-string-table
            create-section-header-string-table
            string-table-offset
            get-section-name-offset))

(define (create-string-table symbol-addresses)
  (let* ((names (cons "" (map (lambda (pair) (symbol->string (car pair))) symbol-addresses)))
         (total-length (apply + (map (lambda (name) (+ (string-length name) 1)) names)))
         (table (make-bytevector total-length 0))
         (offset 0))
    (for-each (lambda (name)
                (let ((len (string-length name)))
                  (bytevector-copy! (string->utf8 name) 0 table offset len)
                  (set! offset (+ offset len 1))))
              names)
    table))

(define (create-section-header-string-table)
  (let* ((names '("" ".text" ".data" ".bss" ".rodata" ".symtab" ".strtab" ".shstrtab" 
                  ".rela.text" ".dynamic" ".dynstr" ".dynsym" ".rela.dyn" ".got" ".plt"))
         (total-length (apply + (map (lambda (s) (+ (string-length s) 1)) names)))
         (table (make-bytevector total-length 0))
         (offset 0))
    
    (format #t "Creating section header string table\n")
    (format #t "Total names: ~a\n" (length names))
    (format #t "Total length: ~a bytes\n" total-length)
    
    (for-each (lambda (name)
                (let ((len (string-length name)))
                  (format #t "Adding name: '~a' at offset ~a\n" name offset)
                  (bytevector-copy! (string->utf8 name) 0 table offset len)
                  (set! offset (+ offset len 1))))
              names)
    
    (format #t "Final offset: ~a\n" offset)
    (format #t "Table length: ~a\n" (bytevector-length table))
    
    ; Add a hexdump of the table
    (format #t "Table contents (hexdump):\n")
    (let ((hex-dump (with-output-to-string
                      (lambda ()
                        (let loop ((i 0))
                          (when (< i (bytevector-length table))
                            (format #t "~2,'0x " (bytevector-u8-ref table i))
                            (when (= 15 (modulo i 16))
                              (format #t "\n"))
                            (loop (+ i 1))))))))
      (format #t "~a\n" hex-dump))
    
    table))

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
