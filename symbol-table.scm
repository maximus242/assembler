(define-module (symbol-table)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (string-table)
  #:export (make-symbol-table
            add-symbol!
            get-symbol
            create-symbol-table
            create-dynamic-symbol-table
            create-hash-section
            make-symbol-entry
            symbol-entry-name
            symbol-entry-address
            symbol-entry-shndx))

(define-record-type <symbol-entry>
  (make-symbol-entry name address info other shndx size)
  symbol-entry?
  (name symbol-entry-name)
  (address symbol-entry-address)
  (info symbol-entry-info)
  (other symbol-entry-other)
  (shndx symbol-entry-shndx)
  (size symbol-entry-size))

;; Convert a bytevector to a list of its elements for debugging purposes
(define (bytevector->list bv)
  (let loop ((i 0) (lst '()))
    (if (= i (bytevector-length bv))
        (reverse lst)
        (loop (+ i 1) (cons (bytevector-u8-ref bv i) lst)))))

(define (make-symbol-table)
  (make-hash-table))

(define (add-symbol! table name address)
  (hash-set! table name address))

(define (get-symbol table name)
  (hash-ref table name #f))

(define* (create-symbol-table symbol-addresses #:optional (options '()))
  (let ((symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24))
        (st-name-offset (or (assoc-ref options 'st-name-offset) 0))
        (st-info-offset (or (assoc-ref options 'st-info-offset) 4))
        (st-other-offset (or (assoc-ref options 'st-other-offset) 5))
        (st-shndx-offset (or (assoc-ref options 'st-shndx-offset) 6))
        (st-value-offset (or (assoc-ref options 'st-value-offset) 8))
        (st-size-offset (or (assoc-ref options 'st-size-offset) 16))
        (stt-object (or (assoc-ref options 'stt-object) 1))
        (stb-global (or (assoc-ref options 'stb-global) 1))
        (shn-data (or (assoc-ref options 'shn-data) 2)))
    (let* ((symbol-count (+ (length symbol-addresses) 1))
           (table-size (* symbol-count symbol-entry-size))
           (table (make-bytevector table-size 0))
           (string-table (create-string-table symbol-addresses)))
      ;; Initialize the first symbol entry (null symbol)
      (bytevector-u32-set! table st-name-offset 0 (endianness little))
      (bytevector-u8-set! table st-info-offset 0)
      (bytevector-u8-set! table st-other-offset 0)
      (bytevector-u16-set! table st-shndx-offset 0 (endianness little))
      (bytevector-u64-set! table st-value-offset 0 (endianness little))
      (bytevector-u64-set! table st-size-offset 0 (endianness little))
      ;; Process the symbol addresses
      (let loop ((symbols symbol-addresses)
                 (index 1)
                 (str-offset 1))  ; Start at 1 to skip the null byte
        (if (null? symbols)
            (cons table string-table)
            (let* ((symbol (car symbols))
                   (name (symbol->string (car symbol)))
                   (address (cdr symbol))
                   (entry-offset (* index symbol-entry-size))
                   (st-info (logior (ash stb-global 4) stt-object)))
              ;; Write the name offset
              (bytevector-u32-set! table (+ entry-offset st-name-offset) str-offset (endianness little))
              ;; Write the info (global object) and other fields
              (bytevector-u8-set! table (+ entry-offset st-info-offset) st-info)
              (bytevector-u8-set! table (+ entry-offset st-other-offset) 0)
              (bytevector-u16-set! table (+ entry-offset st-shndx-offset) shn-data (endianness little))
              ;; Write the address
              (bytevector-u64-set! table (+ entry-offset st-value-offset) address (endianness little))
              ;; Set the size field to 8 (64-bit values)
              (bytevector-u64-set! table (+ entry-offset st-size-offset) 8 (endianness little))
              (loop (cdr symbols)
                    (+ index 1)
                    (+ str-offset (string-length name) 1))))))))  ; +1 for null terminator

(define* (create-dynamic-symbol-table symbol-addresses #:optional (options '()))
  (let ((symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24))
        (st-name-offset (or (assoc-ref options 'st-name-offset) 0))
        (st-info-offset (or (assoc-ref options 'st-info-offset) 4))
        (st-other-offset (or (assoc-ref options 'st-other-offset) 5))
        (st-shndx-offset (or (assoc-ref options 'st-shndx-offset) 6))
        (st-value-offset (or (assoc-ref options 'st-value-offset) 8))
        (st-size-offset (or (assoc-ref options 'st-size-offset) 16))
        (null-terminator-size (or (assoc-ref options 'null-terminator-size) 1))
        (initial-string-offset (or (assoc-ref options 'initial-string-offset) 1))
        (stt-object (or (assoc-ref options 'stt-object) 1))
        (stb-global (or (assoc-ref options 'stb-global) 1))
        (shn-data (or (assoc-ref options 'shn-data) 2)))  ; Section index for .data
    (let* ((symbol-count (+ (length symbol-addresses) 1))
           (table-size (* symbol-count symbol-entry-size))
           (table (make-bytevector table-size 0))
           (string-table-offset initial-string-offset))
      ;; Initialize the first symbol entry (null symbol)
      (bytevector-u32-set! table st-name-offset 0 (endianness little))
      (bytevector-u8-set! table st-info-offset 0)
      (bytevector-u8-set! table st-other-offset 0)
      (bytevector-u16-set! table st-shndx-offset 0 (endianness little))
      (bytevector-u64-set! table st-value-offset 0 (endianness little))
      (bytevector-u64-set! table st-size-offset 0 (endianness little))
      ;; Process the symbol addresses
      (let loop ((symbols symbol-addresses)
                 (index 1)
                 (str-offset initial-string-offset))
        (if (null? symbols)
            table
            (let* ((symbol (car symbols))
                   (name (symbol->string (car symbol)))
                   (address (cdr symbol))
                   (entry-offset (* index symbol-entry-size))
                   (st-info (logior (ash stb-global 4) stt-object)))
              ;; Write the name offset
              (bytevector-u32-set! table (+ entry-offset st-name-offset) str-offset (endianness little))
              ;; Write the info (global object) and other fields
              (bytevector-u8-set! table (+ entry-offset st-info-offset) st-info)
              (bytevector-u8-set! table (+ entry-offset st-other-offset) 0)
              (bytevector-u16-set! table (+ entry-offset st-shndx-offset) shn-data (endianness little))
              ;; Write the address
              (bytevector-u64-set! table (+ entry-offset st-value-offset) address (endianness little))
              ;; Set the size field to 0 (or actual size if known)
              (bytevector-u64-set! table (+ entry-offset st-size-offset) 0 (endianness little))
              (loop (cdr symbols)
                    (+ index 1)
                    (+ str-offset (string-length name) null-terminator-size))))))))

(define* (create-table symbol-addresses create-entry-func table-type #:optional (options '()))
  (let ((stt-object (or (assoc-ref options 'stt-object) 1))
        (stt-func (or (assoc-ref options 'stt-func) 2))
        (stb-global (or (assoc-ref options 'stb-global) 1))
        (shn-text (or (assoc-ref options 'shn-text) 1))
        (shn-data (or (assoc-ref options 'shn-data) 2))
        (symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24)))
    (let* ((symbol-count (+ (length symbol-addresses) 1))
           (entries (cons (create-entry-func 'main #x1000 (logior (ash stb-global 4) stt-func) 0 shn-text 0)
                          (map (lambda (sym)
                                 (create-entry-func (car sym) (cdr sym) (logior (ash stb-global 4) stt-object) 0 shn-data 32))
                               symbol-addresses))))
      (symbols->bytevector entries symbol-entry-size))))

(define (create-symbol-entry name address info other shndx size)
  (make-symbol-entry name address info other shndx size))

(define (symbols->bytevector entries symbol-entry-size)
  (let* ((table-size (* (length entries) symbol-entry-size))
         (table (make-bytevector table-size 0)))
    (let loop ((entries entries)
               (index 0))
      (if (null? entries)
          table
          (let* ((entry (car entries))
                 (address (symbol-entry-address entry))
                 (entry-offset (* index symbol-entry-size)))
            (bytevector-u64-set! table entry-offset address (endianness little))
            (loop (cdr entries)
                  (+ index 1)))))))

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
