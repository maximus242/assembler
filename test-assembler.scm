(use-modules (assembler))
(use-modules (ice-9 pretty-print))
(use-modules (rnrs bytevectors))

(define (bytevector->hex-string bv)
  (string-join (map (lambda (byte) (format #f "~2,'0x" byte))
                    (bytevector->u8-list bv))
               " "))

(define (test-assemble name instructions expected)
  (let ((result (assemble instructions)))
    (if (equal? result expected)
        (format #t "PASS: ~a~%" name)
        (begin
          (format #t "FAIL: ~a~%" name)
          (format #t "  Expected: ~a~%" (bytevector->hex-string expected))
          (format #t "  Actual:   ~a~%" (bytevector->hex-string result))))))

;; Test mov instruction
(test-assemble "mov rax, rcx"
               '((mov rax rcx))
               #vu8(#x48 #x89 #xC8))

;; Test mov.imm32 instruction
(test-assemble "mov.imm32 rax, 42"
               '((mov.imm32 rax 42))
               #vu8(#x48 #xC7 #xC0 #x2A #x00 #x00 #x00))

;; Test add instruction
(test-assemble "add rax, rcx"
               '((add rax rcx))
               #vu8(#x48 #x01 #xC8))

;; Test syscall instruction
(test-assemble "syscall"
               '((syscall))
               #vu8(#x0F #x05))

;; Test multiple instructions
(test-assemble "Multiple instructions"
               '((mov.imm32 rax 60)
                 (mov.imm32 rdi 0)
                 (syscall))
               #vu8(#x48 #xC7 #xC0 #x3C #x00 #x00 #x00
                    #x48 #xC7 #xC7 #x00 #x00 #x00 #x00
                    #x0F #x05))

;; Test valid program with new instructions
(test-assemble "Valid program with new instructions"
  '((mov.imm32 rdi buffer1)
    (mov.imm32 rsi buffer2)
    (mov.imm32 rdx result)
    (vmovaps ymm0 (rdi))
    (vmovaps ymm1 (rsi))
    (vaddps ymm2 ymm0 ymm1)
    (vmovaps ymm3 (multiplier))
    (vfmadd132ps ymm2 ymm2 ymm3)
    (vmovaps (rdx) ymm2)
    (vxorps ymm2 ymm2 ymm2)
    (vmovaps (rdx) ymm2)
    (mov.imm32 eax 60)
    (xor edi edi)
    (syscall))
  (u8-list->bytevector
   (append
    '(#x48 #xC7 #xC7)       ; mov.imm32 rdi, buffer1
    (make-list 4 0)         ; Placeholder for buffer1 address
    '(#x48 #xC7 #xC6)       ; mov.imm32 rsi, buffer2
    (make-list 4 0)         ; Placeholder for buffer2 address
    '(#x48 #xC7 #xC2)       ; mov.imm32 rdx, result
    (make-list 4 0)         ; Placeholder for result address
    '(#xC5 #xFC #x28 #x07   ; vmovaps ymm0, [rdi]
      #xC5 #xFC #x28 #x0E   ; vmovaps ymm1, [rsi]
      #xC5 #xF4 #x58 #xD1   ; vaddps ymm2, ymm0, ymm1
      #xC5 #xFC #x28 #x05)  ; vmovaps ymm3, [multiplier]
    (make-list 4 0)         ; Placeholder for multiplier address
    '(#xC4 #xE2 #x7D #x98 #xD3 ; vfmadd132ps ymm2, ymm2, ymm3
      #xC5 #xFC #x29 #x12      ; vmovaps [rdx], ymm2
      #xC5 #xF4 #x57 #xD2      ; vxorps ymm2, ymm2, ymm2
      #xC5 #xFC #x29 #x12      ; vmovaps [rdx], ymm2
      #x48 #xC7 #xC0 #x3C #x00 #x00 #x00 ; mov.imm32 eax, 60
      #x48 #x31 #xFF            ; xor edi, edi
      #x0F #x05))))             ; syscall

;; Test label and instructions
(test-assemble "Label and instructions"
               '((label start)
                 (mov.imm32 rax 1)
                 (label loop)
                 (add rax rax)
                 (syscall))
               #vu8(#x48 #xC7 #xC0 #x01 #x00 #x00 #x00
                    #x48 #x01 #xC0
                    #x0F #x05))

;; Test label positions
(let* ((instructions '((label start)
                       (mov.imm32 rax 1)
                       (label loop)
                       (add rax rax)
                       (syscall)))
       (result (assemble instructions))
       (label-pos (get-label-positions)))
  (if (and (= (hash-ref label-pos 'start) 0)
           (= (hash-ref label-pos 'loop) 7))
      (format #t "PASS: Label positions~%")
      (format #t "FAIL: Label positions~%  Expected: start=0, loop=7~%  Actual: start=~a, loop=~a~%"
              (hash-ref label-pos 'start)
              (hash-ref label-pos 'loop))))

(newline)
(display "All tests completed.\n")
