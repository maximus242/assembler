(define-module (symbol-table)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (string-table)
  #:export (make-symbol-table
            add-symbol!
            add-label-symbol!
            get-symbol
            symbol-in-table?
            symbol-is-function?
            create-symbol-table
            create-dynamic-symbol-table
            create-hash-section
            make-symbol-entry
            symbol-entry-name
            symbol-entry-address
            symbol-entry-shndx
            convert-old-format-to-new
            create-version-section))

(define *version-string* "VERS_1.0")

(define-record-type <symbol-entry>
  (make-symbol-entry name address info other shndx size)
  symbol-entry?
  (name symbol-entry-name)
  (address symbol-entry-address)
  (info symbol-entry-info)
  (other symbol-entry-other)
  (shndx symbol-entry-shndx)
  (size symbol-entry-size))

(define (make-symbol-table)
  (make-hash-table))

(define (add-symbol! table name address)
  (hash-set! table name (cons address #f)))

(define (add-label-symbol! table name address)
  (hash-set! table name (cons address #t)))

(define (get-symbol table name)
  (let ((entry (hash-ref table name #f)))
    (and entry (car entry))))

(define (symbol-in-table? table name)
  (hash-ref table name #f))

(define (symbol-is-function? table name)
  (let ((entry (hash-ref table name #f)))
    (and entry (cdr entry))))

(define (add-symbols-to-table! table symbols add-func)
  (if (hash-table? symbols)
      (hash-for-each (lambda (name address)
                       (add-func table name address))
                     symbols)
      (for-each (lambda (sym)
                  (add-func table (car sym) (cdr sym)))
                symbols)))

(define (convert-old-format-to-new symbol-addresses label-positions)
  (let ((table (make-symbol-table)))
    (add-symbols-to-table! table symbol-addresses add-symbol!)
    (add-symbols-to-table! table (or label-positions '()) add-label-symbol!)
    table))

(define* (create-symbol-table symbol-addresses label-positions #:optional (options '()))
  (convert-old-format-to-new symbol-addresses label-positions))

(define (create-symbol-entry name-offset address is-function options)
  (let ((st-info (logior (ash (assoc-ref options 'stb-global) 4) 
                         (if is-function (assoc-ref options 'stt-func) (assoc-ref options 'stt-object))))
        (shndx (if is-function (assoc-ref options 'shn-text) (assoc-ref options 'shn-data))))
    (make-symbol-entry name-offset address st-info 0 shndx (if is-function 0 32))))

(define (write-symbol-entry! table entry-offset options entry)
  (let ((write-field (lambda (offset size value)
                       (case size
                         ((1) (bytevector-u8-set! table (+ entry-offset offset) value))
                         ((2) (bytevector-u16-set! table (+ entry-offset offset) value (endianness little)))
                         ((4) (bytevector-u32-set! table (+ entry-offset offset) value (endianness little)))
                         ((8) (bytevector-u64-set! table (+ entry-offset offset) value (endianness little)))))))
    (write-field (assoc-ref options 'st-name-offset) 4 (symbol-entry-name entry))
    (write-field (assoc-ref options 'st-info-offset) 1 (symbol-entry-info entry))
    (write-field (assoc-ref options 'st-other-offset) 1 (symbol-entry-other entry))
    (write-field (assoc-ref options 'st-shndx-offset) 2 (symbol-entry-shndx entry))
    (write-field (assoc-ref options 'st-value-offset) 8 (symbol-entry-address entry))
    (write-field (assoc-ref options 'st-size-offset) 8 (symbol-entry-size entry))))

(define* (create-dynamic-symbol-table symbol-table #:optional (options '()))
  (let* ((default-options '((symbol-entry-size . 24)
                            (st-name-offset . 0)
                            (st-info-offset . 4)
                            (st-other-offset . 5)
                            (st-shndx-offset . 6)
                            (st-value-offset . 8)
                            (st-size-offset . 16)
                            (null-terminator-size . 1)
                            (initial-string-offset . 1)
                            (stt-object . 1)
                            (stt-func . 2)
                            (stb-global . 1)
                            (shn-data . 2)
                            (shn-text . 1)))
         (opts (append options default-options))
         (symbol-entry-size (assoc-ref opts 'symbol-entry-size))
         (symbol-count (+ (hash-count (const #t) symbol-table) 1))
         (table-size (* symbol-count symbol-entry-size))
         (table (make-bytevector table-size 0))
         (string-table-size (+ 1 (string-length *version-string*) 1
                               (hash-fold (lambda (key value acc) 
                                            (+ acc (string-length (symbol->string key)) 1))
                                          0 
                                          symbol-table)))
         (string-table (make-bytevector string-table-size 0))
         (string-table-offset (assoc-ref opts 'initial-string-offset)))

    (write-symbol-entry! table 0 opts (create-symbol-entry 0 0 #f opts))
    
    (bytevector-copy! (string->utf8 *version-string*) 0 string-table string-table-offset (string-length *version-string*))
    (bytevector-u8-set! string-table (+ string-table-offset (string-length *version-string*)) 0)
    (set! string-table-offset (+ string-table-offset (string-length *version-string*) 1))

    (let loop ((symbols (hash-map->list cons symbol-table))
               (index 1)
               (str-offset string-table-offset))
      (if (null? symbols)
          (cons table string-table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (value (cdr symbol))
                 (address (car value))
                 (is-function (cdr value))
                 (name-bytes (string->utf8 name))
                 (entry (create-symbol-entry str-offset address is-function opts))
                 (entry-offset (* index symbol-entry-size)))
            (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
            (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
            (write-symbol-entry! table entry-offset opts entry)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (bytevector-length name-bytes) (assoc-ref opts 'null-terminator-size))))))))

(define* (create-hash-section dynsym-table #:optional (options '()))
  (let* ((opts (append options '((hash-header-size . 8)
                                 (hash-entry-size . 4)
                                 (symbol-entry-size . 24))))
         (nbucket 1)
         (nchain (/ (bytevector-length dynsym-table) (assoc-ref opts 'symbol-entry-size)))
         (hash-size (+ (assoc-ref opts 'hash-header-size)
                       (* (assoc-ref opts 'hash-entry-size) (+ nbucket nchain))))
         (hash-section (make-bytevector hash-size 0)))
    (bytevector-u32-set! hash-section 0 nbucket (endianness little))
    (bytevector-u32-set! hash-section 4 nchain (endianness little))
    (bytevector-u32-set! hash-section 8 0 (endianness little))
    (let loop ((i 0))
      (when (< i nchain)
        (bytevector-u32-set! hash-section 
                             (+ (assoc-ref opts 'hash-header-size) 
                                (* (assoc-ref opts 'hash-entry-size) i))
                             i 
                             (endianness little))
        (loop (+ i 1))))
    hash-section))

(define (create-version-section)
  (let* ((version-bytes (string->utf8 *version-string*))
         (version-section-size (+ (bytevector-length version-bytes) 1))
         (version-section (make-bytevector version-section-size 0)))
    (bytevector-copy! version-bytes 0 version-section 0 (bytevector-length version-bytes))
    version-section))
