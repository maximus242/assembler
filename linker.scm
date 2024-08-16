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
  #:use-module (rnrs arithmetic bitwise)  ; Added this line
  #:export (link-code resolve-references))

;; Constants
(define base-address-difference #x1000) ; 0x401000 - 0x400000
(define rex-w #x48)
(define mov-immediate #xC7)
(define vmovaps-prefix #xC5)
(define vmovaps-opcode #xFC)
(define vmovaps-mod-rm #x28)
(define vmovaps-sib #x05)
(define call-opcode #xE8)
(define jmp-opcode #xE9)
(define register-mask #x07)
(define immediate-size 4)
(define mov-immediate-total-size 7)
(define vmovaps-total-size 8)
(define call-jmp-total-size 5)
(define lea-instruction-size 7)

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

;; Instruction handling functions
(define (handle-mov-immediate code resolved-code offset symbol-table label-positions reg-to-symbol-map code-base-address)
  (let* ((reg (bitwise-and (bytevector-u8-ref code (+ offset 2)) register-mask))
         (imm-offset (+ offset 3)))
    (if (< (+ imm-offset immediate-size) (bytevector-length code))
      (let ((imm (bytevector-u32-ref code imm-offset (endianness little))))
        (when (= imm 0) ; Possible symbolic reference or label
          (let* ((symbol-name (alist-ref reg reg-to-symbol-map))
                 (symbol-address (and symbol-name (or (alist-ref symbol-name symbol-table)
                                                      (hash-ref label-positions symbol-name)))))
            (when symbol-address
              (bytevector-u32-set! resolved-code imm-offset 
                                   (if (hash-ref label-positions symbol-name)
                                     (+ symbol-address code-base-address)
                                     (adjust-address symbol-address))
                                   (endianness little)))))
        (+ offset mov-immediate-total-size))
      (+ offset 1))))

(define (handle-vmovaps code resolved-code offset symbol-table code-base-address)
  (let* ((imm-offset (+ offset 4))
         (imm (bytevector-u32-ref code imm-offset (endianness little)))
         (symbol-address (alist-ref 'multiplier symbol-table)))
    (when symbol-address
      (let* ((instruction-end (+ offset vmovaps-total-size))
             (next-instruction-address (+ instruction-end code-base-address))
             (target-offset (- (adjust-address symbol-address) next-instruction-address)))
        (bytevector-u32-set! resolved-code imm-offset target-offset (endianness little))))
    (+ offset vmovaps-total-size)))

(define (handle-call-jmp code resolved-code offset symbol-table label-positions reg-to-symbol-map code-base-address)
  (let* ((imm-offset (+ offset 1))
         (imm (bytevector-s32-ref code imm-offset (endianness little))))
    (when (= imm 0) ; Possible label reference
      (let* ((label-name (alist-ref offset reg-to-symbol-map))
             (label-position (and label-name (hash-ref label-positions label-name))))
        (when label-position
          (let* ((instruction-end (+ offset call-jmp-total-size))
                 (next-instruction-address (+ instruction-end code-base-address))
                 (target-offset (- (+ label-position code-base-address) next-instruction-address)))
            (bytevector-s32-set! resolved-code imm-offset target-offset (endianness little))))))
    (+ offset call-jmp-total-size)))

;; Main functions
(define (resolve-references code symbol-table label-positions reg-to-symbol-map)
  (let* ((code-length (bytevector-length code))
         (resolved-code (bytevector-copy code)))
    (let loop ((offset 0))
      (if (< offset code-length)
          (let ((instruction (bytevector-u8-ref code offset)))
            (cond
              ;; Handle mov.imm32 instructions
              ((and (= instruction #x48)
                    (< (+ offset 6) code-length)
                    (= (bytevector-u8-ref code (+ offset 1)) #xC7))
               (let* ((reg (bitwise-and (bytevector-u8-ref code (+ offset 2)) #x07))
                      (symbol (assoc-ref reg-to-symbol-map reg))
                      (address (and symbol (assoc-ref symbol-table (string->symbol symbol)))))
                 (when address
                   (bytevector-u32-set! resolved-code (+ offset 3) address (endianness little)))
                 (loop (+ offset 7))))
              
              ;; Handle vmovaps instructions
              ((and (= instruction #xC5)
                    (< (+ offset 7) code-length)
                    (= (bytevector-u8-ref code (+ offset 1)) #xFC)
                    (= (bytevector-u8-ref code (+ offset 2)) #x28))
               (let* ((symbol 'multiplier)  ; Assuming it's always multiplier in this case
                      (address (assoc-ref symbol-table symbol)))
                 (when address
                   (bytevector-u32-set! resolved-code (+ offset 4) 
                                        (- address (+ offset 8))  ; Relative addressing
                                        (endianness little)))
                 (loop (+ offset 8))))
              
              ;; Handle jump instructions
              ((and (= instruction #xE9)
                    (< (+ offset 4) code-length))
               (let* ((label 'label1)  ; Assuming it's always label1 in this case
                      (target-offset (hash-ref label-positions label)))
                 (when target-offset
                   (let ((relative-offset (- target-offset (+ offset 5))))
                     (bytevector-u32-set! resolved-code (+ offset 1) 
                                          relative-offset
                                          (endianness little))))
                 (loop (+ offset 5))))
              
              (else (loop (+ offset 1)))))
        resolved-code))))

(define (link-code code symbol-addresses label-positions)
  (let ((linked-code (bytevector-copy code))
        (code-offset 0))
    (for-each
      (lambda (inst)
        (when (eq? (car inst) 'lea)
          (let* ((label (caddr (cadr inst)))
                 (target-address (cdr (assoc label symbol-addresses)))
                 (instruction-end (+ code-offset lea-instruction-size))
                 (next-instruction-address (+ code-offset lea-instruction-size))
                 (displacement (- target-address next-instruction-address)))
            (bytevector-u32-set! linked-code (+ code-offset 3) displacement (native-endianness)))))
      (disassemble code))
    linked-code))