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
            convert-old-format-to-new))

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
  (hash-set! table name (cons address #f)))  ; #f indicates it's not a function

(define (add-label-symbol! table name address)
  (hash-set! table name (cons address #t)))  ; #t indicates it's a function

(define (get-symbol table name)
  (let ((entry (hash-ref table name #f)))
    (and entry (car entry))))

(define (symbol-in-table? table name)
  (hash-ref table name #f))

(define (symbol-is-function? table name)
  (let ((entry (hash-ref table name #f)))
    (and entry (cdr entry))))

(define (convert-old-format-to-new symbol-addresses label-positions)
  (let ((table (make-symbol-table)))
    (if (hash-table? symbol-addresses)
        (hash-for-each (lambda (name address)
                         (add-symbol! table name address))
                       symbol-addresses)
        (for-each (lambda (sym)
                    (add-symbol! table (car sym) (cdr sym)))
                  symbol-addresses))
    (if (hash-table? label-positions)
        (hash-for-each (lambda (name address)
                         (add-label-symbol! table name address))
                       label-positions)
        (for-each (lambda (label)
                    (add-label-symbol! table (car label) (cdr label)))
                  (if (null? label-positions) '() label-positions)))
    table))

(define* (create-symbol-table symbol-addresses label-positions #:optional (options '()))
  (if (hash-table? symbol-addresses)
      (convert-old-format-to-new symbol-addresses label-positions)
      (convert-old-format-to-new symbol-addresses label-positions)))

(define* (create-dynamic-symbol-table symbol-table #:optional (options '()))
  (let* ((symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24))
         (st-name-offset (or (assoc-ref options 'st-name-offset) 0))
         (st-info-offset (or (assoc-ref options 'st-info-offset) 4))
         (st-other-offset (or (assoc-ref options 'st-other-offset) 5))
         (st-shndx-offset (or (assoc-ref options 'st-shndx-offset) 6))
         (st-value-offset (or (assoc-ref options 'st-value-offset) 8))
         (st-size-offset (or (assoc-ref options 'st-size-offset) 16))
         (null-terminator-size (or (assoc-ref options 'null-terminator-size) 1))
         (initial-string-offset (or (assoc-ref options 'initial-string-offset) 1))
         (stt-object (or (assoc-ref options 'stt-object) 1))
         (stt-func (or (assoc-ref options 'stt-func) 2))
         (stb-global (or (assoc-ref options 'stb-global) 1))
         (shn-data (or (assoc-ref options 'shn-data) 2))
         (shn-text (or (assoc-ref options 'shn-text) 1))
         (symbol-count (+ (hash-count (const #t) symbol-table) 1))
         (table-size (* symbol-count symbol-entry-size))
         (table (make-bytevector table-size 0))
         (string-table-size (+ 1 (hash-fold (lambda (key value acc) 
                                              (+ acc (string-length (symbol->string key)) 1))
                                            0 
                                            symbol-table)))
         (string-table (make-bytevector string-table-size 0))
         (string-table-offset initial-string-offset))
    ;; Initialize the first symbol entry (null symbol)
    (bytevector-u32-set! table st-name-offset 0 (endianness little))
    (bytevector-u8-set! table st-info-offset 0)
    (bytevector-u8-set! table st-other-offset 0)
    (bytevector-u16-set! table st-shndx-offset 0 (endianness little))
    (bytevector-u64-set! table st-value-offset 0 (endianness little))
    (bytevector-u64-set! table st-size-offset 0 (endianness little))
    ;; Process the symbols
    (let loop ((symbols (hash-map->list cons symbol-table))
               (index 1)
               (str-offset initial-string-offset))
      (if (null? symbols)
          (cons table string-table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (value (cdr symbol))
                 (address (car value))
                 (is-function (cdr value))
                 (entry-offset (* index symbol-entry-size))
                 (st-info (logior (ash stb-global 4) (if is-function stt-func stt-object)))
                 (shndx (if is-function shn-text shn-data))
                 (name-bytes (string->utf8 name)))
            ;; Add name to string table
            (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
            (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
            ;; Write the name offset
            (bytevector-u32-set! table (+ entry-offset st-name-offset) str-offset (endianness little))
            ;; Write the info (global object/function) and other fields
            (bytevector-u8-set! table (+ entry-offset st-info-offset) st-info)
            (bytevector-u8-set! table (+ entry-offset st-other-offset) 0)
            (bytevector-u16-set! table (+ entry-offset st-shndx-offset) shndx (endianness little))
            ;; Write the address
            (bytevector-u64-set! table (+ entry-offset st-value-offset) address (endianness little))
            ;; Set the size field
            (bytevector-u64-set! table (+ entry-offset st-size-offset) (if is-function 0 32) (endianness little))
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (bytevector-length name-bytes) null-terminator-size)))))))

(define* (create-hash-section dynsym-table #:optional (options '()))
  (let ((hash-header-size (or (assoc-ref options 'hash-header-size) 8))
        (hash-entry-size (or (assoc-ref options 'hash-entry-size) 4))
        (symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24)))
    (let* ((nbucket 1)
           (nchain (/ (bytevector-length dynsym-table) symbol-entry-size))
           (hash-size (+ hash-header-size
                         (* hash-entry-size nbucket)
                         (* hash-entry-size nchain)))
           (hash-section (make-bytevector hash-size 0)))
      (bytevector-u32-set! hash-section 0 nbucket (endianness little))
      (bytevector-u32-set! hash-section 4 nchain (endianness little))
      (bytevector-u32-set! hash-section 8 0 (endianness little))
      (let loop ((i 0))
        (when (< i nchain)
          (bytevector-u32-set! hash-section (+ hash-header-size (* hash-entry-size i)) i (endianness little))
          (loop (+ i 1))))
      hash-section)))
