(define-module (string-table)
  #:use-module (rnrs bytevectors)
  #:export (create-string-table
            create-section-header-string-table
            string-table-offset))

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
  (string->utf8 
   (string-append
    "\0"                ; Null section
    ".text\0"           ; Code section
    ".data\0"           ; Data section
    ".bss\0"            ; Uninitialized data section
    ".rodata\0"         ; Read-only data section
    ".symtab\0"         ; Symbol table
    ".strtab\0"         ; String table
    ".shstrtab\0"       ; Section header string table
    ".rela.text\0"      ; Relocation entries for .text
    ".rela.data\0"      ; Relocation entries for .data
    ".init\0"           ; Initialization code
    ".fini\0"           ; Termination code
    ".dynamic\0"        ; Dynamic linking information
    ".dynsym\0"         ; Dynamic linking symbol table
    ".dynstr\0")))      ; Dynamic linking string table

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
                    (loop (+ i 1))))))))  ; <- Add one more closing parenthesis here
