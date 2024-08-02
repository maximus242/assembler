(define-module (section-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:use-module (string-table)
  #:export (create-section-headers))

(define (create-section-header name type addr offset size link info align entsize string-table)
  (let ((header (make-bytevector 64 0)))
    (bytevector-u32-set! header 0 (string-table-offset name string-table) (endianness little))
    (bytevector-u32-set! header 4 type (endianness little))
    (bytevector-u64-set! header 8 addr (endianness little))
    (bytevector-u64-set! header 16 offset (endianness little))
    (bytevector-u64-set! header 24 size (endianness little))
    (bytevector-u32-set! header 32 link (endianness little))
    (bytevector-u32-set! header 36 info (endianness little))
    (bytevector-u64-set! header 40 align (endianness little))
    (bytevector-u64-set! header 48 entsize (endianness little))
    header))

(define (create-section-headers code-size data-size symtab-size strtab-size shstrtab-size
                                dynamic-symbol-table-size relocation-table-size dynamic-section-size)
  (let* ((string-table (create-section-header-string-table))
         (null-header (create-section-header "" 0 0 0 0 0 0 0 0 string-table))
         (text-offset #x1000)
         (text-header (create-section-header ".text" 1 text-offset text-offset code-size 0 0 16 0 string-table))
         (data-offset (+ text-offset (align-to code-size #x1000)))
         (data-header (create-section-header ".data" 1 data-offset data-offset data-size 0 0 16 0 string-table))
         (symtab-offset (+ data-offset (align-to data-size #x1000)))
         (symtab-header (create-section-header ".symtab" 2 0 symtab-offset symtab-size 0 0 8 24 string-table))
         (strtab-offset (+ symtab-offset symtab-size))
         (strtab-header (create-section-header ".strtab" 3 0 strtab-offset strtab-size 0 0 1 0 string-table))
         (shstrtab-offset (+ strtab-offset strtab-size))
         (shstrtab-header (create-section-header ".shstrtab" 3 0 shstrtab-offset shstrtab-size 0 0 1 0 string-table))
         (dynsym-offset (+ shstrtab-offset shstrtab-size))
         (dynsym-header (create-section-header ".dynsym" 11 0 dynsym-offset dynamic-symbol-table-size 0 0 8 24 string-table))
         (rela-offset (+ dynsym-offset dynamic-symbol-table-size))
         (rela-header (create-section-header ".rela.dyn" 4 0 rela-offset relocation-table-size 0 0 8 24 string-table))
         (dynamic-offset (+ rela-offset relocation-table-size))
         (dynamic-header (create-section-header ".dynamic" 6 dynamic-offset dynamic-offset dynamic-section-size 0 0 8 16 string-table)))
    (bytevector-append
     null-header
     text-header
     data-header
     symtab-header
     strtab-header
     shstrtab-header
     dynsym-header
     rela-header
     dynamic-header)))

(define (create-section-header-string-table)
  (let ((table (make-bytevector 1 0)))  ; Start with null byte
    (define (add-string! str)
      (let* ((utf8 (string->utf8 str))
             (new-size (+ (bytevector-length table) (bytevector-length utf8) 1)))
        (let ((new-table (make-bytevector new-size 0)))
          (bytevector-copy! table 0 new-table 0 (bytevector-length table))
          (bytevector-copy! utf8 0 new-table (bytevector-length table) (bytevector-length utf8))
          (set! table new-table))))
    
    (add-string! ".text")
    (add-string! ".data")
    (add-string! ".symtab")
    (add-string! ".strtab")
    (add-string! ".shstrtab")
    (add-string! ".dynsym")
    (add-string! ".rela.dyn")
    (add-string! ".dynamic")
    table))