(define-module (symbol-table)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-symbol-table
            add-symbol!
            get-symbol
            create-symbol-table
            create-dynamic-symbol-table
            create-hash-section))

;; Define a record type for symbols
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
  (hash-set! table name address)
  (format #t "Symbol added: ~a with address: 0x~x~%" name address))

(define (get-symbol table name)
  (let ((address (hash-ref table name #f)))
    (if address
      (format #t "Symbol retrieved: ~a with address: 0x~x~%" name address)
      (format #t "Symbol not found: ~a~%" name))
    address))

(define* (create-symbol-table symbol-addresses #:optional (options '()))
  (let ((stt-object (or (assoc-ref options 'stt-object) 1))
        (stt-func (or (assoc-ref options 'stt-func) 2))
        (stb-global (or (assoc-ref options 'stb-global) 1))
        (shn-text (or (assoc-ref options 'shn-text) 1))
        (shn-data (or (assoc-ref options 'shn-data) 2))
        (symbol-entry-size (or (assoc-ref options 'symbol-entry-size) 24)))
    (create-table symbol-addresses create-symbol-entry "symbol"
                  `((stt-object . ,stt-object)
                    (stt-func . ,stt-func)
                    (stb-global . ,stb-global)
                    (shn-text . ,shn-text)
                    (shn-data . ,shn-data)
                    (symbol-entry-size . ,symbol-entry-size)))))

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
        (stt-object (or (assoc-ref options 'stt-object) 1)))
    (let* ((symbol-count (+ (length symbol-addresses) 1))  ; Add 1 for null symbol
           (table-size (* symbol-count symbol-entry-size))
           (table (make-bytevector table-size 0))
           (string-table-offset initial-string-offset))
      (format #t "Creating dynamic symbol table with ~a symbols (including null symbol)~%" symbol-count)

      ; Create null symbol entry
      (bytevector-u32-set! table st-name-offset 0 (endianness little))
      (bytevector-u8-set! table st-info-offset 0)
      (bytevector-u8-set! table st-other-offset 0)
      (bytevector-u16-set! table st-shndx-offset 0 (endianness little))
      (bytevector-u64-set! table st-value-offset 0 (endianness little))
      (bytevector-u64-set! table st-size-offset 0 (endianness little))
      (format #t "Added null symbol at index 0~%")

      (let loop ((symbols symbol-addresses)
                 (index 1)  ; Start at 1 because 0 is the null symbol
                 (str-offset initial-string-offset))
        (if (null? symbols)
          (begin
            (format #t "Dynamic symbol table created. Size: ~a bytes~%" (bytevector-length table))
            table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (address (cdr symbol))
                 (entry-offset (* index symbol-entry-size)))
            (bytevector-u32-set! table (+ entry-offset st-name-offset) str-offset (endianness little))
            (bytevector-u8-set! table (+ entry-offset st-info-offset) stt-object)
            (bytevector-u8-set! table (+ entry-offset st-other-offset) 0)
            (bytevector-u16-set! table (+ entry-offset st-shndx-offset) 0 (endianness little))
            (bytevector-u64-set! table (+ entry-offset st-value-offset) address (endianness little))
            (bytevector-u64-set! table (+ entry-offset st-size-offset) 0 (endianness little))
            (format #t "Added dynamic symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                    name address str-offset)
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
    (let* ((symbol-count (+ (length symbol-addresses) 1))  ; Add 1 for 'main'
           (entries (cons (create-entry-func 'main #x1000 (logior (ash stb-global 4) stt-func) 0 shn-text 0)
                          (map (lambda (sym)
                                 (create-entry-func (car sym) (cdr sym) (logior (ash stb-global 4) stt-object) 0 shn-data 32))
                               symbol-addresses))))
      (format #t "Creating ~a table with ~a symbols~%" table-type symbol-count)
      (let ((table (symbols->bytevector entries symbol-entry-size)))
        (format #t "~a table created. Size: ~a bytes~%" (string-capitalize table-type) (bytevector-length table))
        table))))

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
                 (name (symbol-entry-name entry))
                 (address (symbol-entry-address entry))
                 (entry-offset (* index symbol-entry-size)))
            (bytevector-u64-set! table entry-offset address (endianness little))
            (format #t "Added symbol: ~a, address: 0x~x, offset in table: ~a~%" 
                    name address entry-offset)
            (loop (cdr entries)
                  (+ index 1)))))))

;; New function to create a hash section
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
      (format #t "Creating hash section with ~a buckets and ~a chain entries~%" nbucket nchain)
      (bytevector-u32-set! hash-section 0 nbucket (endianness little))
      (bytevector-u32-set! hash-section 4 nchain (endianness little))
      ; All symbols hash to bucket 0 in this simple implementation
      (bytevector-u32-set! hash-section 8 0 (endianness little))
      ; Chain is just the index of each symbol
      (let loop ((i 0))
        (when (< i nchain)
          (bytevector-u32-set! hash-section (+ hash-header-size (* hash-entry-size i)) i (endianness little))
          (loop (+ i 1))))
      (format #t "Hash section created. Size: ~a bytes~%" (bytevector-length hash-section))
      hash-section)))
