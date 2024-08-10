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
         (table-size (* symbol-count 24))  ; Each symbol entry is 24 bytes
         (table (make-bytevector table-size 0))
         (string-table-offset 1))  ; Start at 1 to account for null byte at beginning of string table
    (format #t "Creating dynamic symbol table with ~a symbols (including null symbol)~%" symbol-count)
    
    ; Create null symbol entry
    (bytevector-u32-set! table 0 0 (endianness little))  ; st_name = 0
    (bytevector-u8-set! table 4 0)  ; st_info = 0
    (bytevector-u8-set! table 5 0)  ; st_other = 0
    (bytevector-u16-set! table 6 0 (endianness little))  ; st_shndx = 0
    (bytevector-u64-set! table 8 0 (endianness little))  ; st_value = 0
    (bytevector-u64-set! table 16 0 (endianness little))  ; st_size = 0
    (format #t "Added null symbol at index 0~%")
    
    (let loop ((symbols symbol-addresses)
               (index 1)  ; Start at 1 because 0 is the null symbol
               (str-offset 1))
      (if (null? symbols)
        (begin
          (format #t "Dynamic symbol table created. Size: ~a bytes~%" (bytevector-length table))
          table)
        (let* ((symbol (car symbols))
               (name (symbol->string (car symbol)))
               (address (cdr symbol)))
          (bytevector-u32-set! table (* index 24) str-offset (endianness little))  ; st_name
          (bytevector-u8-set! table (+ (* index 24) 4) 1)  ; st_info (1 = STT_OBJECT)
          (bytevector-u8-set! table (+ (* index 24) 5) 0)  ; st_other
          (bytevector-u16-set! table (+ (* index 24) 6) 0 (endianness little))  ; st_shndx
          (bytevector-u64-set! table (+ (* index 24) 8) address (endianness little))  ; st_value
          (bytevector-u64-set! table (+ (* index 24) 16) 0 (endianness little))  ; st_size
          (format #t "Added dynamic symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                  name address str-offset)
          (loop (cdr symbols)
                (+ index 1)
                (+ str-offset (string-length name) 1)))))))

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
  (let* ((table-size (* (length entries) 24))
         (table (make-bytevector table-size 0))
         (string-table-offset 1))
    (let loop ((entries entries)
               (index 0)
               (str-offset 1))
      (if (null? entries)
          table
          (let* ((entry (car entries))
                 (name (symbol->string (symbol-entry-name entry))))
            (bytevector-u32-set! table (* index 24) str-offset (endianness little))  ; st_name
            (bytevector-u8-set! table (+ (* index 24) 4) (symbol-entry-info entry))  ; st_info
            (bytevector-u8-set! table (+ (* index 24) 5) (symbol-entry-other entry))  ; st_other
            (bytevector-u16-set! table (+ (* index 24) 6) (symbol-entry-shndx entry) (endianness little))  ; st_shndx
            (bytevector-u64-set! table (+ (* index 24) 8) (symbol-entry-address entry) (endianness little))  ; st_value
            (bytevector-u64-set! table (+ (* index 24) 16) (symbol-entry-size entry) (endianness little))  ; st_size
            (format #t "Added symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                    name (symbol-entry-address entry) str-offset)
            (loop (cdr entries)
                  (+ index 1)
                  (+ str-offset (string-length name) 1)))))))

;; New function to create a hash section
(define (create-hash-section dynsym-table)
  (let* ((nbucket 1)
         (nchain (/ (bytevector-length dynsym-table) 24))  ; Each symbol is 24 bytes
         (hash-size (+ (* 4 2) ; nbucket and nchain
                       (* 4 nbucket)
                       (* 4 nchain)))
         (hash-section (make-bytevector hash-size 0)))
    (format #t "Creating hash section with ~a buckets and ~a chain entries~%" nbucket nchain)
    (bytevector-u32-set! hash-section 0 nbucket (endianness little))
    (bytevector-u32-set! hash-section 4 nchain (endianness little))
    ; All symbols hash to bucket 0 in this simple implementation
    (bytevector-u32-set! hash-section 8 0 (endianness little))
    ; Chain is just the index of each symbol
    (let loop ((i 0))
      (when (< i nchain)
        (bytevector-u32-set! hash-section (+ 12 (* 4 i)) i (endianness little))
        (loop (+ i 1))))
    (format #t "Hash section created. Size: ~a bytes~%" (bytevector-length hash-section))
    hash-section))
