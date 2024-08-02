(define-module (string-table)
  #:use-module (rnrs bytevectors)
  #:export (create-string-table
            create-section-header-string-table))

(define (create-string-table symbol-addresses)
  (let* ((names (map (lambda (pair) (symbol->string (car pair))) symbol-addresses))
         (total-length (+ 1 (apply + (map (lambda (name) (+ (string-length name) 1)) names))))
         (table (make-bytevector total-length 0)))
    (let loop ((names names)
               (offset 1))
      (if (null? names)
          table
          (let ((name (car names)))
            (bytevector-copy! (string->utf8 name) 0 table offset (string-length name))
            (loop (cdr names) (+ offset (string-length name) 1)))))))

(define (create-section-header-string-table)
  (string->utf8 "\0.text\0.data\0.symtab\0.strtab\0.shstrtab\0"))
