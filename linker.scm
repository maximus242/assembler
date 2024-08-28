(define-module (linker)
               #:use-module (rnrs bytevectors)
               #:use-module (rnrs io ports)
               #:use-module (ice-9 format)
               #:use-module (ice-9 hash-table)
               #:use-module (rnrs arithmetic bitwise)
               #:use-module (srfi srfi-11)  ; for let-values
               #:export (link-code resolve-references))

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
                   ((list? table) (assoc-ref table stripped-key))
                   (else #f))))
    (log-message "safe-ref: original key: ~a, stripped key: ~a, result: ~a" 
                 key stripped-key result)
    result))

(define (apply-relocation code offset type symbol symbol-table code-base-address code-start-offset got-offset)
  (log-message "Applying relocation:")
  (log-message "  Offset: 0x~x" offset)
  (log-message "  Type: ~a" type)
  (log-message "  Symbol: ~a" symbol)
  (log-message "  Code base address: 0x~x" code-base-address)
  (log-message "  Code start offset: 0x~x" code-start-offset)
  (log-message "  GOT offset: 0x~x" got-offset)
  (log-message "  Code length: ~a" (bytevector-length code))

  (let* ((symbol-info (safe-ref symbol-table symbol))
         (symbol-address (if (pair? symbol-info) 
                           (car symbol-info) 
                           (if (number? symbol-info) symbol-info #f)))
         (code-offset (- offset code-start-offset))
         (instruction-end offset)
         (next-instruction-address (+ offset 4)))

    (log-message "  Symbol info: ~a" symbol-info)
    (log-message "  Symbol address: 0x~x" symbol-address)
    (log-message "  Absolute offset: 0x~x" offset)
    (log-message "  Calculated code offset: 0x~x" code-offset)
    (log-message "  Instruction end: 0x~x" instruction-end)
    (log-message "  Next instruction address: 0x~x" next-instruction-address)

    (if symbol-address

      (let* ((got-entry-offset (- symbol-address got-offset))
             (relative-address (- (+ got-entry-offset got-offset) next-instruction-address)) ;; Updated calculation
             (displacement (bitwise-and relative-address #xffffffff)))
        (log-message "  GOT entry offset: 0x~x" got-entry-offset)
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
      (log-message "  Error: Symbol not found in symbol table: ~a" symbol))))

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

;; Constants
(define instruction-sizes
  `((mov-immediate . 7)
    (vmovaps . 8)
    (call-jmp . 5)
    (lea . 7)))

;; Instruction handling functions
(define (handle-mov-immediate code resolved-code offset symbol-table code-base-address)
  (log-message "handle-mov-immediate: offset: ~a" offset)
  ;; Implementation needed here
  (values resolved-code (+ offset (cdr (assoc 'mov-immediate instruction-sizes)))))

(define (handle-vmovaps code resolved-code offset symbol-table code-base-address)
  (log-message "handle-vmovaps: offset: ~a" offset)
  ;; Implementation needed here
  (values resolved-code (+ offset (cdr (assoc 'vmovaps instruction-sizes)))))

(define (handle-call-jmp code resolved-code offset label-positions code-base-address)
  (log-message "handle-call-jmp: offset: ~a" offset)
  ;; Implementation needed here
  (values resolved-code (+ offset (cdr (assoc 'call-jmp instruction-sizes)))))

(define (handle-lea code resolved-code offset symbol-table label-positions code-base-address)
  (log-message "handle-lea: offset: ~a" offset)
  (let* ((imm-offset (+ offset 3))
         (imm (bytevector-s32-ref code imm-offset (endianness little))))
    (log-message "  LEA immediate value: ~a" imm)
    (let* ((symbol-address (safe-ref symbol-table imm))
           (instruction-end (+ offset 7))
           (next-instruction-address (+ instruction-end code-base-address))
           (relative-address (and symbol-address 
                                  (- symbol-address next-instruction-address))))
      (log-message "  Symbol address: ~a" symbol-address)
      (log-message "  Next instruction address: ~a" next-instruction-address)
      (log-message "  Relative address: ~a" relative-address)
      (if (and relative-address
               (>= relative-address (- (expt 2 31))) 
               (< relative-address (expt 2 31)))
        (begin
          (bytevector-s32-set! resolved-code imm-offset relative-address (endianness little))
          (log-message "  Updated LEA instruction at offset ~a" offset))
        (log-message "  No update for LEA instruction at offset ~a" offset))
      (values resolved-code (+ offset (cdr (assoc 'lea instruction-sizes)))))))

(define (handle-instruction code resolved-code offset symbol-table label-positions code-base-address)
  (log-message "handle-instruction: offset: ~a" offset)
  (let ((instruction (bytevector-u8-ref code offset)))
    (log-message "  Instruction byte: ~2,'0x" instruction)
    (cond
      ((mov-immediate? code offset)
       (handle-mov-immediate code resolved-code offset symbol-table code-base-address))
      ((vmovaps? code offset)
       (handle-vmovaps code resolved-code offset symbol-table code-base-address))
      ((call-jmp? code offset)
       (handle-call-jmp code resolved-code offset label-positions code-base-address))
      ((lea? code offset)
       (handle-lea code resolved-code offset symbol-table label-positions code-base-address))
      (else 
        (log-message "  Unhandled instruction at offset ~a" offset)
        (values resolved-code (+ offset 1))))))

(define (mov-immediate? code offset)
  (and (= (bytevector-u8-ref code offset) #x48)
       (< (+ offset 6) (bytevector-length code))
       (= (bytevector-u8-ref code (+ offset 1)) #xC7)))

(define (vmovaps? code offset)
  (and (= (bytevector-u8-ref code offset) #xC5)
       (< (+ offset 7) (bytevector-length code))
       (= (bytevector-u8-ref code (+ offset 1)) #xFC)
       (= (bytevector-u8-ref code (+ offset 2)) #x28)))

(define (call-jmp? code offset)
  (and (= (bytevector-u8-ref code offset) #xE9)
       (< (+ offset 4) (bytevector-length code))))

(define (lea? code offset)
  (and (or (= (bytevector-u8-ref code offset) #x48)
           (= (bytevector-u8-ref code offset) #x4C))
       (< (+ offset 6) (bytevector-length code))
       (= (bytevector-u8-ref code (+ offset 1)) #x8D)))

;; Main function
(define (resolve-references code symbol-table label-positions relocation-table code-start-offset got-offset)
  (log-message "Entering resolve-references function")
  (log-message "Code length: ~a bytes" (bytevector-length code))
  (log-message "Symbol table contents:")
  (hash-for-each
    (lambda (key value)
      (log-message "  ~a: ~a" key value))
    symbol-table)
  (log-message "Label positions:")
  (hash-for-each
    (lambda (key value)
      (log-message "  ~a: 0x~x" key value))
    label-positions)
  (log-message "Relocation table:")
  (for-each
    (lambda (reloc)
      (log-message "  Offset: 0x~x, Type: ~a, Symbol: ~a" 
                   (car reloc) (cadr reloc) (caddr reloc)))
    relocation-table)
  (log-message "Code start offset: 0x~x" code-start-offset)

  (let ((resolved-code (bytevector-copy code))
        (code-base-address code-start-offset))  ; Set code-base-address to code-start-offset
    (log-message "Starting to apply relocations")
    (for-each
      (lambda (reloc)
        (apply-relocation resolved-code
                          (car reloc)    ; offset
                          (cadr reloc)   ; type
                          (caddr reloc)  ; symbol
                          symbol-table
                          code-base-address
                          code-start-offset
                          got-offset))
      relocation-table)

    (log-message "Finished applying relocations")
    (log-message "Final resolved code (first 100 bytes):")
    (log-message "~a" (bytevector->hex-string resolved-code 0 (min 100 (bytevector-length resolved-code))))
    resolved-code))

(define (link-code code symbol-addresses label-positions relocation-table)
  (log-message "link-code: Inputs:")
  (log-message "code:")
  (log-bytevector code)
  (log-message "symbol-addresses:")
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
    (resolve-references code symbol-table label-positions relocation-table #x1000 #x3100)))
