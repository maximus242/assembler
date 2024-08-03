(define-module (string-table)
  #:use-module (rnrs bytevectors)
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
    (for-each (lambda (name)
                (let ((len (string-length name)))
                  (bytevector-copy! (string->utf8 name) 0 table offset len)
                  (set! offset (+ offset len 1))))
              names)
    table))

(define (string-table-offset name string-table)
  (let loop ((offset 0))
    (if (>= offset (bytevector-length string-table))
        #f  ; String not found
        (if (string-match? name string-table offset)
            offset
            (loop (+ offset 1))))))

(define (string-match? str bv offset)
  (let ((str-len (string-length str)))
    (and (<= (+ offset str-len) (bytevector-length bv))
         (let loop ((i 0))
           (if (= i str-len)
               #t
               (and (= (char->integer (string-ref str i))
                       (bytevector-u8-ref bv (+ offset i)))
                    (loop (+ i 1))))))))

(define (get-section-name-offset shstrtab section-name)
  (string-table-offset section-name shstrtab))