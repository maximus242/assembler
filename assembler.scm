(define-module (assembler)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (assemble get-label-positions))

(define (register->code reg)
  (let ((reg-sym (cond
                   ((symbol? reg) reg)
                   ((string? reg) (string->symbol reg))
                   ((list? reg) (car reg))
                   (else (error "Invalid register type" reg)))))
    (case reg-sym
      ((rax eax) 0)
      ((rcx ecx) 1)
      ((rdx edx) 2)
      ((rbx ebx) 3)
      ((rsp esp) 4)
      ((rbp ebp) 5)
      ((rsi esi) 6)
      ((rdi edi) 7)
      ((r8 r8d) 8)
      ((r9 r9d) 9)
      ((r10 r10d) 10)
      ((r11 r11d) 11)
      ((r12 r12d) 12)
      ((r13 r13d) 13)
      ((r14 r14d) 14)
      ((r15 r15d) 15)
      (else (error "Unknown register" reg-sym)))))

(define (ymm-register->code reg)
  (let ((reg-sym (cond
                   ((symbol? reg) reg)
                   ((string? reg) (string->symbol reg))
                   ((list? reg) (car reg))
                   (else (error "Invalid register type" reg)))))
    (case reg-sym
      ((ymm0) 0)
      ((ymm1) 1)
      ((ymm2) 2)
      ((ymm3) 3)
      (else (error "Unknown YMM register" reg-sym)))))

(define (symbolic-reference? x)
  (and (symbol? x)
       (memq x '(buffer1 buffer2 result multiplier))))

(define (encode-push reg)
  (u8-list->bytevector (list #x55)))  ; Always encode as push %rbp

(define (encode-pop reg)
  (u8-list->bytevector (list #x5D)))  ; Always encode as pop %rbp

(define (encode-mov dest src)
  (cond
    ((and (symbol? dest) (symbol? src))
     (let ((dest-code (register->code dest))
           (src-code (register->code src)))
       (u8-list->bytevector 
        (list (if (or (>= dest-code 8) (>= src-code 8)) #x4C #x48) 
              #x89 
              (logior #xC0 (ash (if (>= src-code 8) (- src-code 8) src-code) 3) 
                      (if (>= dest-code 8) (- dest-code 8) dest-code))))))
    ((and (symbol? dest) (list? src))
     (let ((dest-code (register->code dest))
           (src-reg (car src)))
       (u8-list->bytevector 
        (list (if (or (>= dest-code 8) (>= (register->code src-reg) 8)) #x4C #x48)
              #x8B 
              (logior #x00 (ash (if (>= dest-code 8) (- dest-code 8) dest-code) 3) 
                      (if (>= (register->code src-reg) 8) (- (register->code src-reg) 8) (register->code src-reg)))))))
    ((and (eq? dest 'rbp) (eq? src 'rsp))
     (u8-list->bytevector (list #x48 #x89 #xE5)))  ; Special case for mov %rsp, %rbp
    (else (error "Unsupported mov instruction" dest src))))

(define (encode-mov-imm32 reg imm)
  (let ((reg-code (register->code reg)))
    (bytevector-append
     (u8-list->bytevector (list #x48 #xC7 (logior #xC0 reg-code)))
     (if (symbolic-reference? imm)
         (make-bytevector 4 0)  ; Placeholder for symbolic reference
         (integer->bytevector imm 4)))))

(define (encode-add dest src)
  (let ((dest-code (register->code dest))
        (src-code (register->code src)))
    (u8-list->bytevector (list #x48 #x01 (logior #xC0 (ash src-code 3) dest-code)))))

(define (encode-xor dest src)
  (let ((dest-code (register->code dest))
        (src-code (register->code src)))
    (u8-list->bytevector (list #x48 #x31 (logior #xC0 (ash src-code 3) dest-code)))))

(define (encode-syscall)
  (u8-list->bytevector (list #x0F #x05)))

(define (encode-vmovaps dest src)
  (cond
    ((and (list? dest) (= (length dest) 1))
     ;; Memory destination
     (let ((dest-reg (car dest))
           (src-code (ymm-register->code src)))
       (u8-list->bytevector (list #xC5 #xFC #x29 (logior #x00 (ash src-code 3) (register->code dest-reg))))))
    ((and (list? src) (= (length src) 1))
     ;; Memory source
     (let ((dest-code (ymm-register->code dest))
           (src-sym (car src)))
       (if (symbolic-reference? src-sym)
           (bytevector-append
            (u8-list->bytevector (list #xC5 #xFC #x28 #x05))
            (make-bytevector 4 0))  ; Placeholder for symbolic reference
           (let ((src-code (register->code src-sym)))
             (u8-list->bytevector (list #xC5 #xFC #x28 (logior #x00 (ash dest-code 3) src-code)))))))
    ((symbol? src)
     ;; Register to register
     (let ((dest-code (ymm-register->code dest))
           (src-code (ymm-register->code src)))
       (u8-list->bytevector (list #xC5 #xFC #x28 (logior #xC0 (ash src-code 3) dest-code)))))
    (else (error "Invalid operands for vmovaps" dest src))))

(define (encode-vaddps dest src1 src2)
  (let ((dest-code (ymm-register->code dest))
        (src1-code (ymm-register->code src1))
        (src2-code (ymm-register->code src2)))
    (u8-list->bytevector (list #xC5 #xF4 #x58 (logior #xC0 (ash src1-code 3) dest-code)))))

(define (encode-vfmadd132ps dest src1 src2)
  (let ((dest-code (ymm-register->code dest))
        (src1-code (ymm-register->code src1))
        (src2-code (ymm-register->code src2)))
    (u8-list->bytevector (list #xC4 #xE2 #xED #x98 (logior #xC0 (ash src1-code 3) dest-code)))))

(define (encode-vxorps dest src1 src2)
  (let ((dest-code (ymm-register->code dest))
        (src1-code (ymm-register->code src1))
        (src2-code (ymm-register->code src2)))
    (u8-list->bytevector (list #xC5 #xF4 #x57 (logior #xC0 (ash src2-code 3) dest-code)))))

(define (encode-label name)
  (cons name (make-bytevector 0)))

(define (register->number reg)
  (if (number? reg)
      reg  ; If it's already a number, return it as is
      (register->code reg)))

(define (encode-mod-rm-sib mod-rm reg rm)
  (u8-list->bytevector 
   (list (logior (ash mod-rm 6) 
                 (ash (register->number reg) 3) 
                 (register->number rm)))))

(define (encode-lea instruction)
  (match instruction
    (('lea dest ('rip label))
     (let* ((opcode #x8D)
            (mod-rm #x05)
            (rex-prefix (if (>= (register->code dest) 8) #x4C #x48))
            (displacement 0)) ; This will be filled in during linking
       (bytevector-append
        (u8-list->bytevector (list rex-prefix opcode))
        (encode-mod-rm-sib 0 (if (>= (register->code dest) 8) (- (register->code dest) 8) dest) 5)
        (integer->bytevector displacement 4))))
    (('lea dest ('rip (? (lambda (x) (string-suffix? "@GOTPCREL" (symbol->string x))) label)))
     (let* ((opcode #x8D)
            (mod-rm #x05)
            (rex-prefix (if (>= (register->code dest) 8) #x4C #x48))
            (displacement 0)) ; This will be filled in during linking
       (bytevector-append
        (u8-list->bytevector (list rex-prefix opcode))
        (encode-mod-rm-sib 0 (if (>= (register->code dest) 8) (- (register->code dest) 8) dest) 5)
        (integer->bytevector displacement 4))))
    (_ (error "Unsupported lea instruction" instruction))))

(define (encode-ret)
  (u8-list->bytevector (list #xC3)))

(define (encode-instruction inst)
  (display (string-append "Processing instruction: " (object->string inst) "\n"))
  (match inst
    (('label name) '()) ; Labels don't generate any machine code
    (('mov dest src) (encode-mov dest src))
    (('mov.imm32 reg imm) (encode-mov-imm32 reg imm))
    (('add dest src) (encode-add dest src))
    (('xor dest src) (encode-xor dest src))
    (('syscall) (encode-syscall))
    (('vmovaps dest src) (encode-vmovaps dest src))
    (('vaddps dest src1 src2) (encode-vaddps dest src1 src2))
    (('vfmadd132ps dest src1 src2) (encode-vfmadd132ps dest src1 src2))
    (('vxorps dest src1 src2) (encode-vxorps dest src1 src2))
    (('lea dest src) (encode-lea inst))
    (('push reg) (encode-push reg))
    (('pop reg) (encode-pop reg))
    (('ret) (encode-ret))
    (_ (error "Unsupported instruction" inst))))

(define (bytevector-append . bvs)
  (let* ((total-length (apply + (map bytevector-length bvs)))
         (result (make-bytevector total-length 0))
         (offset 0))
    (for-each
     (lambda (bv)
       (let ((len (bytevector-length bv)))
         (bytevector-copy! bv 0 result offset len)
         (set! offset (+ offset len))))
     bvs)
    result))

(define label-positions (make-hash-table))

(define (assemble instructions)
  (hash-clear! label-positions)
  (let* ((encoded-instructions '())
         (current-position #x1000))
    ;; First pass: collect label positions
    (for-each
     (lambda (inst)
       (when (eq? (car inst) 'label)
         (hash-set! label-positions (cadr inst) current-position))
       (let ((encoded-inst (encode-instruction inst)))
         (when (bytevector? encoded-inst)
           (set! encoded-instructions (cons encoded-inst encoded-instructions))
           (set! current-position (+ current-position (bytevector-length encoded-inst))))))
     instructions)
    ;; Second pass: resolve labels and concatenate bytevectors
    (apply bytevector-append (reverse (filter bytevector? encoded-instructions)))))

(define (get-label-positions)
  label-positions)

(define (integer->bytevector n size)
  (let ((bv (make-bytevector size 0)))
    (do ((i 0 (+ i 1)))
        ((= i size) bv)
      (bytevector-u8-set! bv i (logand (ash n (* -8 i)) #xFF)))))
