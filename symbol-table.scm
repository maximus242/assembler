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

;; Define constants
(define STT_OBJECT 1)
(define STT_FUNC 2)
(define STB_GLOBAL 1)
(define SHN_TEXT 1)
(define SHN_DATA 2)

;; Symbol entry constants
(define SYMBOL_ENTRY_SIZE 24)
(define ST_NAME_OFFSET 0)
(define ST_INFO_OFFSET 4)
(define ST_OTHER_OFFSET 5)
(define ST_SHNDX_OFFSET 6)
(define ST_VALUE_OFFSET 8)
(define ST_SIZE_OFFSET 16)

;; String table constants
(define NULL_TERMINATOR_SIZE 1)
(define INITIAL_STRING_OFFSET 1)

;; Hash section constants
(define HASH_HEADER_SIZE 8)
(define HASH_ENTRY_SIZE 4)

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

(define (create-symbol-table symbol-addresses)
  (create-table symbol-addresses create-symbol-entry "symbol"))

(define (create-dynamic-symbol-table symbol-addresses)
  (let* ((symbol-count (+ (length symbol-addresses) 1))  ; Add 1 for null symbol
         (table-size (* symbol-count SYMBOL_ENTRY_SIZE))
         (table (make-bytevector table-size 0))
         (string-table-offset INITIAL_STRING_OFFSET))
    (format #t "Creating dynamic symbol table with ~a symbols (including null symbol)~%" symbol-count)

    ; Create null symbol entry
    (bytevector-u32-set! table ST_NAME_OFFSET 0 (endianness little))
    (bytevector-u8-set! table ST_INFO_OFFSET 0)
    (bytevector-u8-set! table ST_OTHER_OFFSET 0)
    (bytevector-u16-set! table ST_SHNDX_OFFSET 0 (endianness little))
    (bytevector-u64-set! table ST_VALUE_OFFSET 0 (endianness little))
    (bytevector-u64-set! table ST_SIZE_OFFSET 0 (endianness little))
    (format #t "Added null symbol at index 0~%")

    (let loop ((symbols symbol-addresses)
               (index 1)  ; Start at 1 because 0 is the null symbol
               (str-offset INITIAL_STRING_OFFSET))
      (if (null? symbols)
        (begin
          (format #t "Dynamic symbol table created. Size: ~a bytes~%" (bytevector-length table))
          table)
        (let* ((symbol (car symbols))
               (name (symbol->string (car symbol)))
               (address (cdr symbol))
               (entry-offset (* index SYMBOL_ENTRY_SIZE)))
          (bytevector-u32-set! table (+ entry-offset ST_NAME_OFFSET) str-offset (endianness little))
          (bytevector-u8-set! table (+ entry-offset ST_INFO_OFFSET) STT_OBJECT)
          (bytevector-u8-set! table (+ entry-offset ST_OTHER_OFFSET) 0)
          (bytevector-u16-set! table (+ entry-offset ST_SHNDX_OFFSET) 0 (endianness little))
          (bytevector-u64-set! table (+ entry-offset ST_VALUE_OFFSET) address (endianness little))
          (bytevector-u64-set! table (+ entry-offset ST_SIZE_OFFSET) 0 (endianness little))
          (format #t "Added dynamic symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                  name address str-offset)
          (loop (cdr symbols)
                (+ index 1)
                (+ str-offset (string-length name) NULL_TERMINATOR_SIZE)))))))

(define (create-table symbol-addresses create-entry-func table-type)
  (let* ((symbol-count (+ (length symbol-addresses) 1))  ; Add 1 for 'main'
         (entries (cons (create-entry-func 'main #x1000 (logior (ash STB_GLOBAL 4) STT_FUNC) 0 SHN_TEXT 0)
                        (map (lambda (sym)
                               (create-entry-func (car sym) (cdr sym) (logior (ash STB_GLOBAL 4) STT_OBJECT) 0 SHN_DATA 32))
                             symbol-addresses))))
    (format #t "Creating ~a table with ~a symbols~%" table-type symbol-count)
    (let ((table (symbols->bytevector entries)))
      (format #t "~a table created. Size: ~a bytes~%" (string-capitalize table-type) (bytevector-length table))
      table)))

(define (create-symbol-entry name address info other shndx size)
  (make-symbol-entry name address info other shndx size))

(define (symbols->bytevector entries)
  (let* ((table-size (* (length entries) SYMBOL_ENTRY_SIZE))
         (table (make-bytevector table-size 0))
         (string-table-offset INITIAL_STRING_OFFSET))
    (let loop ((entries entries)
               (index 0)
               (str-offset INITIAL_STRING_OFFSET))
      (if (null? entries)
        table
        (let* ((entry (car entries))
               (name (symbol->string (symbol-entry-name entry)))
               (entry-offset (* index SYMBOL_ENTRY_SIZE)))
          (bytevector-u32-set! table (+ entry-offset ST_NAME_OFFSET) str-offset (endianness little))
          (bytevector-u8-set! table (+ entry-offset ST_INFO_OFFSET) (symbol-entry-info entry))
          (bytevector-u8-set! table (+ entry-offset ST_OTHER_OFFSET) (symbol-entry-other entry))
          (bytevector-u16-set! table (+ entry-offset ST_SHNDX_OFFSET) (symbol-entry-shndx entry) (endianness little))
          (bytevector-u64-set! table (+ entry-offset ST_VALUE_OFFSET) (symbol-entry-address entry) (endianness little))
          (bytevector-u64-set! table (+ entry-offset ST_SIZE_OFFSET) (symbol-entry-size entry) (endianness little))
          (format #t "Added symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                  name (symbol-entry-address entry) str-offset)
          (loop (cdr entries)
                (+ index 1)
                (+ str-offset (string-length name) NULL_TERMINATOR_SIZE)))))))

;; New function to create a hash section
(define (create-hash-section dynsym-table)
  (let* ((nbucket 1)
         (nchain (/ (bytevector-length dynsym-table) SYMBOL_ENTRY_SIZE))
         (hash-size (+ HASH_HEADER_SIZE
                       (* HASH_ENTRY_SIZE nbucket)
                       (* HASH_ENTRY_SIZE nchain)))
         (hash-section (make-bytevector hash-size 0)))
    (format #t "Creating hash section with ~a buckets and ~a chain entries~%" nbucket nchain)
    (bytevector-u32-set! hash-section 0 nbucket (endianness little))
    (bytevector-u32-set! hash-section 4 nchain (endianness little))
    ; All symbols hash to bucket 0 in this simple implementation
    (bytevector-u32-set! hash-section 8 0 (endianness little))
    ; Chain is just the index of each symbol
    (let loop ((i 0))
      (when (< i nchain)
        (bytevector-u32-set! hash-section (+ HASH_HEADER_SIZE (* HASH_ENTRY_SIZE i)) i (endianness little))
        (loop (+ i 1))))
    (format #t "Hash section created. Size: ~a bytes~%" (bytevector-length hash-section))
    hash-section))
