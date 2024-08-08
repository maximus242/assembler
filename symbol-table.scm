(define-module (symbol-table)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:export (make-symbol-table
            add-symbol!
            get-symbol
            create-symbol-table
            create-dynamic-symbol-table))

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
  (let* ((symbol-count (+ (length symbol-addresses) 1))  ; Add 1 for 'main'
         (table-size (* symbol-count 24))
         (table (make-bytevector table-size 0))
         (string-table-offset 1))  ; Start at 1 to account for null byte at beginning of string table

    (format #t "Creating symbol table with ~a symbols~%" symbol-count)

    ; Add 'main' symbol
    (bytevector-u32-set! table 0 1 (endianness little))  ; st_name (offset in string table)
    (bytevector-u8-set! table 4 18)  ; st_info (global function)
    (bytevector-u8-set! table 5 0)   ; st_other
    (bytevector-u16-set! table 6 1 (endianness little))  ; st_shndx (1 for .text)
    (bytevector-u64-set! table 8 #x1000 (endianness little))  ; st_value (entry point)
    (bytevector-u64-set! table 16 0 (endianness little))  ; st_size (unknown)

    (format #t "Added symbol: main, address: 0x1000, offset in string table: 1~%")

    (let loop ((symbols symbol-addresses)
               (index 1)  ; Start at 1 because 'main' is index 0
               (str-offset 6))  ; 'main\0' takes 5 bytes
      (if (null? symbols)
          (begin
            (format #t "Symbol table created. Size: ~a bytes~%" (bytevector-length table))
            table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (address (cdr symbol)))
            (bytevector-u32-set! table (* index 24) str-offset (endianness little))  ; st_name
            (bytevector-u8-set! table (+ (* index 24) 4) 17)  ; st_info (17 = STT_OBJECT | STB_GLOBAL)
            (bytevector-u8-set! table (+ (* index 24) 5) 0)  ; st_other
            (bytevector-u16-set! table (+ (* index 24) 6) 2 (endianness little))  ; st_shndx (2 = .data section)
            (bytevector-u64-set! table (+ (* index 24) 8) address (endianness little))  ; st_value
            (bytevector-u64-set! table (+ (* index 24) 16) 32 (endianness little))  ; st_size (assuming 32-byte size for data objects)
            (format #t "Added symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                    name address str-offset)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (string-length name) 1)))))))  ; Correctly update the string offset

(define (create-dynamic-symbol-table symbol-addresses)
  (let* ((symbol-count (+ (length symbol-addresses) 1))  ; Add 1 for 'main'
         (table-size (* symbol-count 24))
         (table (make-bytevector table-size 0))
         (string-table-offset 1))

    (format #t "Creating dynamic symbol table with ~a symbols~%" symbol-count)

    ; Add 'main' symbol
    (bytevector-u32-set! table 0 1 (endianness little))  ; st_name (offset in string table)
    (bytevector-u8-set! table 4 18)  ; st_info (global function)
    (bytevector-u8-set! table 5 0)   ; st_other
    (bytevector-u16-set! table 6 1 (endianness little))  ; st_shndx (1 for .text)
    (bytevector-u64-set! table 8 #x1000 (endianness little))  ; st_value (entry point)
    (bytevector-u64-set! table 16 0 (endianness little))  ; st_size (unknown)

    (format #t "Added dynamic symbol: main, address: 0x1000, offset in string table: 1~%")

    (let loop ((symbols symbol-addresses)
               (index 1)  ; Start at 1 because 'main' is index 0
               (str-offset 6))  ; 'main\0' takes 5 bytes
      (if (null? symbols)
          (begin
            (format #t "Dynamic symbol table created. Size: ~a bytes~%" (bytevector-length table))
            table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (address (cdr symbol)))
            (bytevector-u32-set! table (* index 24) str-offset (endianness little))  ; st_name
            (bytevector-u8-set! table (+ (* index 24) 4) 17)  ; st_info (17 = STT_OBJECT | STB_GLOBAL)
            (bytevector-u8-set! table (+ (* index 24) 5) 0)  ; st_other
            (bytevector-u16-set! table (+ (* index 24) 6) 2 (endianness little))  ; st_shndx (2 = .data section)
            (bytevector-u64-set! table (+ (* index 24) 8) address (endianness little))  ; st_value
            (bytevector-u64-set! table (+ (* index 24) 16) 32 (endianness little))  ; st_size (assuming 32-byte size for data objects)
            (format #t "Added dynamic symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                    name address str-offset)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (string-length name) 1)))))))
