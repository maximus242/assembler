(define-module (linker)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (srfi srfi-11)  ; for let-values
  #:export (link-code resolve-references))

(define (process-relocation-table relocation-table)
  (define (remove-duplicates lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (remove-duplicates 
               (filter (lambda (x) (not (equal? x (car lst))))
                       (cdr lst))))))

  (define (extract-symbols table)
    (define (strip-got-suffix symbol)
      (let* ((sym-str (symbol->string symbol))
             (at-index (string-index sym-str #\@)))
        (if at-index
            (string->symbol (substring sym-str 0 at-index))
            symbol)))

    (remove-duplicates
     (map (lambda (reloc)
            (strip-got-suffix (caddr reloc)))
          table)))

  (let* ((symbols (extract-symbols relocation-table))
         (base-address #x0)
         (address-step 32))
    (map (lambda (symbol index)
           (cons symbol (+ base-address (* index address-step))))
         symbols
         (iota (length symbols)))))

;; Constants
(define base-address-difference #x1000)

(define (strip-got-suffix symbol)
  (let* ((sym-str (symbol->string symbol))
         (at-index (string-index sym-str #\@)))
    (if at-index
        (string->symbol (substring sym-str 0 at-index))
        symbol)))

(define (safe-ref table key)
  (let* ((stripped-key (strip-got-suffix key))
         (result (cond
                   ((hash-table? table) (hash-ref table stripped-key))
                   ((list? table) (let ((pair (assoc stripped-key table)))
                                    (and pair (cdr pair))))
                   (else #f))))
    (log-message "safe-ref: original key: ~a, stripped key: ~a, result: ~a" 
                 key stripped-key result)
    result))

(define (bytevector->hex-string bv start len)
  (string-join
   (map (lambda (i)
          (format #f "~2,'0x" (bytevector-u8-ref bv i)))
        (iota len start))
   " "))

(define (adjust-address address)
  (let ((result (+ address base-address-difference)))
    (log-message "adjust-address: input: ~a, output: ~a" address result)
    result))

(define (log-message . args)
  (apply format (current-error-port) args)
  (newline (current-error-port)))

(define (log-bytevector bv)
  (let* ((len (bytevector-length bv))
         (max-display 100)
         (display-len (min len max-display)))
    (log-message "Bytevector length: ~a" len)
    (log-message "First ~a bytes: ~a" 
                 display-len
                 (bytevector->hex-string bv 0 display-len))
    (when (> len max-display)
      (log-message "... (truncated)"))))

(define (log-hash-table ht name)
  (log-message "~a:" name)
  (hash-for-each 
   (lambda (key value)
     (log-message "  ~a: ~a" key value))
   ht))

;; Main function
(define (resolve-references code symbol-table label-positions relocation-table code-start-offset got-offset got-relocation-table)
  (log-message "Entering resolve-references function")
  (log-message "GOT relocation table: ~a" got-relocation-table)

  (define (lookup-symbol symbol)
    (let ((sym-str (symbol->string symbol)))
      (if (string-suffix? "@GOTPCREL" sym-str)
          (let* ((base-symbol (strip-got-suffix symbol))
                 (got-entry (assoc base-symbol got-relocation-table)))
            (if got-entry
                (begin
                  (log-message "Found GOT entry for ~a: 0x~x" base-symbol (cdr got-entry))
                  (+ got-offset (cdr got-entry))) ; Return GOT entry address
                (begin
                  (log-message "Warning: Symbol ~a not found in GOT relocation table" symbol)
                  #f))) ; Return #f if not found in GOT table
          (let ((addr (safe-ref symbol-table symbol)))
            (log-message "Regular symbol ~a address: ~a" symbol addr)
            addr))))

  (let ((resolved-code (bytevector-copy code))
        (code-base-address code-start-offset))
    (log-message "Starting to apply relocations")
    (for-each
     (lambda (reloc)
       (let ((symbol-address (lookup-symbol (caddr reloc))))
         (log-message "Applying relocation for ~a at offset 0x~x, symbol address: ~a" 
                      (caddr reloc) (car reloc) (if (number? symbol-address) (format #f "0x~x" symbol-address) symbol-address))
         (when (number? symbol-address) ; Only apply relocation if symbol address is found and is a number
           (apply-relocation resolved-code
                             (car reloc)    ; offset
                             (cadr reloc)   ; type
                             (caddr reloc)  ; symbol
                             symbol-address
                             code-base-address
                             code-start-offset
                             got-offset))))
     relocation-table)
    (log-message "Finished applying relocations")
    (log-message "Final resolved code (first 100 bytes):")
    (log-message "~a" (bytevector->hex-string resolved-code 0 (min 100 (bytevector-length resolved-code))))
    resolved-code))

(define (apply-relocation code offset type symbol symbol-address code-base-address code-start-offset got-offset)
  (log-message "Applying relocation:")
  (log-message "  Offset: 0x~x" offset)
  (log-message "  Type: ~a" type)
  (log-message "  Symbol: ~a" symbol)
  (log-message "  Symbol address: ~a" (if (number? symbol-address) (format #f "0x~x" symbol-address) symbol-address))
  (log-message "  Code base address: 0x~x" code-base-address)
  (log-message "  Code start offset: 0x~x" code-start-offset)
  (log-message "  GOT offset: 0x~x" got-offset)
  (log-message "  Code length: ~a" (bytevector-length code))
  (let* ((code-offset (- offset code-start-offset))
         (instruction-end offset)
         (next-instruction-address (+ offset 4)))
    (log-message "  Absolute offset: 0x~x" offset)
    (log-message "  Calculated code offset: 0x~x" code-offset)
    (log-message "  Instruction end: 0x~x" instruction-end)
    (log-message "  Next instruction address: 0x~x" next-instruction-address)
    (if (number? symbol-address)
        (let* ((relative-address (- symbol-address next-instruction-address 3))
               (displacement (bitwise-and relative-address #xffffffff)))
          (log-message "  Calculated relative address: 0x~x" relative-address)
          (log-message "  Calculated displacement: 0x~x" displacement)
          (if (and (>= code-offset 0) (< (+ code-offset 7) (bytevector-length code)))
              (begin
                (log-message "  Original instruction bytes: ~a" 
                             (bytevector->hex-string code code-offset 8))
                ;; Preserve the first 3 bytes (opcode and ModR/M)
                (bytevector-u32-set! code (+ code-offset 3) displacement (endianness little))
                (log-message "  Updated instruction bytes: ~a" 
                             (bytevector->hex-string code code-offset 8)))
              (log-message "  Error: Code offset out of range for bytevector operations")))
        (log-message "  Error: Invalid symbol address for ~a" symbol))))

(define (link-code code symbol-addresses label-positions relocation-table data-addr)
  (log-message "Relocation Table LINKER: ~a" relocation-table)
  (log-message "link-code: Inputs:")
  (log-message "code:")
  (log-bytevector code)
  (log-message "symbol-addresses:")
  (define got-relocation-table (process-relocation-table relocation-table))
  (log-message "GOT RELOCATION TABLE LINKER: ~a" got-relocation-table)
  (if (hash-table? symbol-addresses)
      (log-hash-table symbol-addresses "Symbol addresses")
      (for-each (lambda (addr) 
                  (log-message "  ~a: ~a" (car addr) (cdr addr)))
                symbol-addresses))
  (log-message "label-positions:")
  (log-hash-table label-positions "Label positions")
  (log-message "relocation-table:")
  (for-each
   (lambda (reloc)
     (log-message "  Offset: ~a, Type: ~a, Symbol: ~a" (car reloc) (cadr reloc) (caddr reloc)))
   relocation-table)

  (let ((symbol-table (if (hash-table? symbol-addresses)
                          symbol-addresses
                          (alist->hash-table symbol-addresses))))
    (resolve-references code symbol-table label-positions relocation-table #x1000 data-addr got-relocation-table)))
