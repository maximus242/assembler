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

(define* (create-symbol-table symbol-addresses label-positions #:optional (options '()))
  (let ((table (make-symbol-table)))
    (add-symbols-to-table! table symbol-addresses add-symbol!)
    (add-symbols-to-table! table (or label-positions '()) add-label-symbol!)
    table))

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

(define (write-symbol-entry-field! table entry-offset opts field-name value)
  (let* ((offset (assoc-ref opts field-name))
         (size (case field-name
                 ((st-name-offset st-value-offset st-size-offset) 8)
                 ((st-info-offset st-other-offset) 1)
                 ((st-shndx-offset) 2))))
    (case size
      ((1) (bytevector-u8-set! table (+ entry-offset offset) value))
      ((2) (bytevector-u16-set! table (+ entry-offset offset) value (endianness little)))
      ((4) (bytevector-u32-set! table (+ entry-offset offset) value (endianness little)))
      ((8) (bytevector-u64-set! table (+ entry-offset offset) value (endianness little))))))

(define (calculate-string-table-size symbol-table)
  (+ 1 (string-length *version-string*) 1
     (hash-fold 
      (lambda (key value acc) 
        (+ acc (string-length (symbol->string key)) 1))
      0 
      symbol-table)))

(define (write-initial-data string-table opts)
  (let ((initial-offset (assoc-ref opts 'initial-string-offset)))
    (bytevector-copy! (string->utf8 *version-string*) 0 string-table initial-offset (string-length *version-string*))
    (bytevector-u8-set! string-table (+ initial-offset (string-length *version-string*)) 0)
    (+ initial-offset (string-length *version-string*) 1)))

(define* (create-dynamic-symbol-table dynamic-symbol-addresses symbol-addresses label-positions #:optional (options '()))
  (define (merge-options default-options custom-options)
    (append custom-options default-options))

  (define (create-symbol-table)
    (let ((table (make-symbol-table)))
      (add-symbols-to-table! table symbol-addresses add-symbol!)
      (add-symbols-to-table! table (or label-positions '()) add-label-symbol!)
      table))

  (define (create-dynamic-symbol-table)
    (let ((table (make-symbol-table)))
      (add-symbols-to-table! table symbol-addresses add-symbol!)
      (add-symbols-to-table! table (or label-positions '()) add-label-symbol!)
      (add-symbols-to-table! table dynamic-symbol-addresses add-symbol!)
      table))

  (define (calculate-symbol-table-size symbol-table opts)
    (let* ((symbol-entry-size (assoc-ref opts 'symbol-entry-size))
           (symbol-count (+ (hash-count (const #t) symbol-table) 1))
           (table-size (* symbol-count symbol-entry-size))
           (string-table-size (calculate-string-table-size symbol-table)))
      (cons table-size string-table-size)))

  (define (calculate-dynamic-symbol-table-size dynamic-symbol-table opts)
    (let* ((symbol-entry-size (assoc-ref opts 'symbol-entry-size))
           (symbol-count (+ (hash-count (const #t) dynamic-symbol-table) 1))
           (table-size (* symbol-count symbol-entry-size))
           (string-table-size (calculate-string-table-size dynamic-symbol-table)))
      (cons table-size string-table-size)))

  (define (initialize-symbol-table table-size string-table-size)
    (cons (make-bytevector table-size 0)
          (make-bytevector string-table-size 0)))

  (define (initialize-dynamic-symbol-table table-size string-table-size)
    (cons (make-bytevector table-size 0)
          (make-bytevector string-table-size 0)))

  (define (process-symbol-table symbol-table table string-table opts initial-str-offset)
    (let loop ((symbols (hash-map->list cons symbol-table))
               (index 1)
               (str-offset initial-str-offset))
      (if (null? symbols)
          (cons table string-table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (value (cdr symbol))
                 (address (car value))
                 (is-function (cdr value))
                 (name-bytes (string->utf8 name))
                 (entry (create-symbol-entry str-offset address is-function opts))
                 (entry-offset (* index (assoc-ref opts 'symbol-entry-size))))
            (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
            (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
            (write-symbol-entry! table entry-offset opts entry)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (bytevector-length name-bytes) (assoc-ref opts 'null-terminator-size)))))))

  (define (process-dynamic-symbol-table dynamic-symbol-table table string-table opts initial-str-offset)
    (let loop ((symbols (hash-map->list cons dynamic-symbol-table))
               (index 1)
               (str-offset initial-str-offset))
      (if (null? symbols)
          (cons table string-table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (value (cdr symbol))
                 (address (car value))
                 (is-function (cdr value))
                 (name-bytes (string->utf8 name))
                 (entry (create-symbol-entry str-offset address is-function opts))
                 (entry-offset (* index (assoc-ref opts 'symbol-entry-size))))
            (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
            (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
            (write-symbol-entry! table entry-offset opts entry)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (bytevector-length name-bytes) (assoc-ref opts 'null-terminator-size)))))))

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
         (opts (merge-options default-options options))
         (symbol-table (create-symbol-table))
         (dynamic-symbol-table (create-dynamic-symbol-table))
         (symbol-sizes (calculate-symbol-table-size symbol-table opts))
         (dynamic-sizes (calculate-dynamic-symbol-table-size dynamic-symbol-table opts))
         (symbol-tables (initialize-symbol-table (car symbol-sizes) (cdr symbol-sizes)))
         (dynamic-tables (initialize-dynamic-symbol-table (car dynamic-sizes) (cdr dynamic-sizes)))
         (symbol-table-bytevector (car symbol-tables))
         (symbol-string-table (cdr symbol-tables))
         (dynamic-table-bytevector (car dynamic-tables))
         (dynamic-string-table (cdr dynamic-tables))
         (initial-str-offset (write-initial-data symbol-string-table opts))
         (dynamic-initial-str-offset (write-initial-data dynamic-string-table opts)))
    
    (write-symbol-entry! symbol-table-bytevector 0 opts (create-symbol-entry 0 0 #f opts))
    (write-symbol-entry! dynamic-table-bytevector 0 opts (create-symbol-entry 0 0 #f opts))
    
    (let ((processed-symbol-table (process-symbol-table symbol-table symbol-table-bytevector symbol-string-table opts initial-str-offset))
          (processed-dynamic-table (process-dynamic-symbol-table dynamic-symbol-table dynamic-table-bytevector dynamic-string-table opts dynamic-initial-str-offset)))
      
      (cons processed-symbol-table processed-dynamic-table))))

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
