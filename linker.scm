(define-module (linker)
  #:use-module (elf-header)
  #:use-module (program-headers)
  #:use-module (section-headers)
  #:use-module (dynamic-section)
  #:use-module (symbol-table)
  #:use-module (string-table)
  #:use-module (utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 format)
  #:use-module (shared-object-creator)
  #:export (link-code))

;; Constants
(define base-address-difference #x1000) ; 0x401000 - 0x400000

;; Helper functions
(define (alist-ref key alist)
  (let ((pair (assoc key alist)))
    (and pair (cdr pair))))

(define (adjust-address address)
  (+ address base-address-difference))

(define (take lst n)
  (if (or (null? lst) (= n 0))
    '()
    (cons (car lst) (take (cdr lst) (- n 1)))))

(define (write-bytevector bv port)
  (put-bytevector port bv))

(define (bytevector-slice bv start end)
  (let ((result (make-bytevector (- end start))))
    (bytevector-copy! bv start result 0 (- end start))
    result))

(define (bytevector-index bv byte start)
  (let loop ((i start))
    (cond
      ((= i (bytevector-length bv)) #f)
      ((= (bytevector-u8-ref bv i) byte) i)
      (else (loop (+ i 1))))))

(define (display-bytevector bv)
  (let ((len (bytevector-length bv)))
    (do ((i 0 (+ i 1)))
      ((= i len))
      (format #t "~2,'0x " (bytevector-u8-ref bv i))
      (when (= (modulo (+ i 1) 16) 0)
        (newline)))))

;; Instruction handling functions
(define (handle-mov-immediate code resolved-code offset symbol-table label-positions reg-to-symbol-map code-base-address)
  (let* ((reg (logand (bytevector-u8-ref code (+ offset 2)) #x07))
         (imm-offset (+ offset 3)))
    (if (< (+ imm-offset 4) (bytevector-length code))
      (let ((imm (bytevector-u32-ref code imm-offset (endianness little))))
        (format #t "  MOV imm32: reg=~a, imm=~x~%" reg imm)
        (when (= imm 0) ; Possible symbolic reference or label
          (let* ((symbol-name (alist-ref reg reg-to-symbol-map))
                 (symbol-address (and symbol-name (or (alist-ref symbol-name symbol-table)
                                                      (hash-ref label-positions symbol-name)))))
            (format #t "  Resolving symbol/label: ~a -> ~a~%" 
                    (or symbol-name "#f") 
                    (if symbol-address 
                      (format #f "~x" symbol-address)
                      "#f"))
            (when symbol-address
              (bytevector-u32-set! resolved-code imm-offset 
                                   (if (hash-ref label-positions symbol-name)
                                     (+ symbol-address code-base-address)
                                     (adjust-address symbol-address))
                                   (endianness little)))))
        (format #t "  After MOV resolution: ~x~%" 
                (bytevector-u32-ref resolved-code imm-offset (endianness little)))
        (+ offset 7))
      (begin
        (format #t "Warning: Insufficient bytes for MOV immediate at offset ~a~%" offset)
        (+ offset 1)))))

(define (handle-vmovaps code resolved-code offset symbol-table code-base-address)
  (let* ((imm-offset (+ offset 4))
         (imm (bytevector-u32-ref code imm-offset (endianness little)))
         (symbol-address (alist-ref 'multiplier symbol-table)))
    (format #t "  VMOVAPS: original displacement=~x~%" imm)
    (when symbol-address
      (let* ((instruction-end (+ offset 8))
             (next-instruction-address (+ instruction-end code-base-address))
             (target-offset (- (adjust-address symbol-address) next-instruction-address)))
        (format #t "    Resolving multiplier -> ~x~%" symbol-address)
        (format #t "    Instruction end: ~x~%" instruction-end)
        (format #t "    Next instruction address: ~x~%" next-instruction-address)
        (format #t "    Target offset: ~x~%" target-offset)
        (bytevector-u32-set! resolved-code imm-offset target-offset (endianness little))))
    (format #t "  After VMOVAPS resolution: ~x~%" 
            (bytevector-u32-ref resolved-code imm-offset (endianness little)))
    (+ offset 8)))

(define (handle-call-jmp code resolved-code offset symbol-table label-positions reg-to-symbol-map code-base-address)
  (let* ((imm-offset (+ offset 1))
         (imm (bytevector-s32-ref code imm-offset (endianness little))))
    (format #t "  CALL/JMP: original offset=~x~%" imm)
    (when (= imm 0) ; Possible label reference
      (let* ((label-name (alist-ref offset reg-to-symbol-map))
             (label-position (and label-name (hash-ref label-positions label-name))))
        (when label-position
          (let* ((instruction-end (+ offset 5))
                 (next-instruction-address (+ instruction-end code-base-address))
                 (target-offset (- (+ label-position code-base-address) next-instruction-address)))
            (format #t "    Resolving label ~a -> ~x~%" label-name label-position)
            (format #t "    Instruction end: ~x~%" instruction-end)
            (format #t "    Next instruction address: ~x~%" next-instruction-address)
            (format #t "    Target offset: ~x~%" target-offset)
            (bytevector-s32-set! resolved-code imm-offset target-offset (endianness little))))))
    (format #t "  After CALL/JMP resolution: ~x~%" 
            (bytevector-s32-ref resolved-code imm-offset (endianness little)))
    (+ offset 5)))

;; Main functions
(define (resolve-references code symbol-table label-positions reg-to-symbol-map)
  (let* ((code-length (bytevector-length code))
         (resolved-code (bytevector-copy code))
         (code-base-address 0)
         (program-base-address 0))
    (format #t "Starting to resolve references. Code length: ~a~%" code-length)
    (format #t "Symbol table: ~a~%" symbol-table)
    (format #t "Label positions: ~a~%" label-positions)
    (let loop ((offset 0))
      (if (< offset code-length)
        (let ((instruction (bytevector-u8-ref code offset)))
          (format #t "Offset: ~a, Instruction: ~x~%" offset instruction)
          (cond
            ((and (= instruction #x48)
                  (< (+ offset 6) code-length)
                  (= (bytevector-u8-ref code (+ offset 1)) #xC7))
             (loop (handle-mov-immediate code resolved-code offset symbol-table label-positions reg-to-symbol-map code-base-address)))
            ((and (= instruction #xC5)
                  (< (+ offset 7) code-length)
                  (= (bytevector-u8-ref code (+ offset 1)) #xFC)
                  (= (bytevector-u8-ref code (+ offset 2)) #x28)
                  (= (bytevector-u8-ref code (+ offset 3)) #x05))
             (loop (handle-vmovaps code resolved-code offset symbol-table code-base-address)))
            ((and (or (= instruction #xE8) ; CALL
                      (= instruction #xE9)) ; JMP
                  (< (+ offset 4) code-length))
             (loop (handle-call-jmp code resolved-code offset symbol-table label-positions reg-to-symbol-map code-base-address)))
            (else 
              (loop (+ offset 1)))))
        (begin
          (format #t "Finished resolving references.~%")
          resolved-code)))))

(define (link-code code symbol-addresses label-positions)
  (let ((linked-code (bytevector-copy code))
        (code-offset 0))
    (for-each
      (lambda (inst)
        (when (eq? (car inst) 'lea)
          (let* ((label (caddr (cadr inst)))
                 (target-address (cdr (assoc label symbol-addresses)))
                 (instruction-end (+ code-offset 7))
                 (next-instruction-address (+ code-offset 7))
                 (displacement (- target-address next-instruction-address)))
            (bytevector-u32-set! linked-code (+ code-offset 3) displacement (native-endianness)))))
      (disassemble code))
    linked-code))
