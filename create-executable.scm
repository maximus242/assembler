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

;; Define symbol addresses
(define symbol-addresses
  '((buffer1 . #x400000)
    (buffer2 . #x400020)
    (result . #x400040)
    (multiplier . #x400060)))

;; Your existing code remains the same
(define example-code
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
    (syscall)))

(define assembled-code (assemble example-code))
(define linked-code (link-code assembled-code symbol-addresses))

;; Create the executable
(create-executable 
 linked-code 
 "my_executable"
 `((buffer1 . ,buffer1)
   (buffer2 . ,buffer2)
   (result . ,result)
   (multiplier . ,multiplier))
 symbol-addresses)

(display "Executable created: my_executable\n")
