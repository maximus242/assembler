(use-modules (linker)
             (assembler)
             (rnrs bytevectors))

;; Define data
(define buffer1
  (u8-list->bytevector
   '(0 0 128 63 0 0 0 64 0 0 64 64 0 0 128 64
     0 0 160 64 0 0 192 64 0 0 224 64 0 0 0 65)))

(define buffer2
  (u8-list->bytevector
   '(0 0 0 63 0 0 64 63 0 0 128 63 0 0 160 63
     0 0 192 63 0 0 224 63 0 0 0 64 0 0 16 64)))

(define multiplier
  (u8-list->bytevector
   (apply append (make-list 8 '(0 0 0 64)))))

(define result (make-bytevector 32 0))

;; Update symbol addresses with proper alignment
(define symbol-addresses
  '((buffer1 . #x404000)
    (buffer2 . #x404040)
    (result . #x404080)
    (multiplier . #x4040C0)))

;; Updated code without vmulps
(define example-code
  '((label test_function)
    (mov.imm32 rdi buffer1)
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
    (mov.imm32 eax 60)       ; 60 is the syscall number for exit
    (xor edi edi)            ; 0 exit status
    (syscall)))

(define assembled-code (assemble example-code))
(define label-positions (get-label-positions))
(define linked-code (link-code assembled-code symbol-addresses label-positions))

;; Create the executable
(create-executable 
 linked-code 
 "simple_executable"
 `((buffer1 . ,buffer1)
   (buffer2 . ,buffer2)
   (result . ,result)
   (multiplier . ,multiplier))
 symbol-addresses
 label-positions)  ; Add this line

;; Add logging
(display "Logging information:\n")
(display (string-append "buffer1 address: " (number->string (cdr (assoc 'buffer1 symbol-addresses))) "\n"))
(display (string-append "buffer2 address: " (number->string (cdr (assoc 'buffer2 symbol-addresses))) "\n"))
(display (string-append "result address: " (number->string (cdr (assoc 'result symbol-addresses))) "\n"))
(display (string-append "multiplier address: " (number->string (cdr (assoc 'multiplier symbol-addresses))) "\n"))
(display "Executable created: simple_executable\n")
