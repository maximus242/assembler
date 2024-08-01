(use-modules (assembler))
(use-modules (linker))
(use-modules (rnrs bytevectors))

;; Define data
(define buffer1
  (u8-list->bytevector
   '(0 0 128 63   ; 1.0
     0 0 0 64     ; 2.0
     0 0 64 64    ; 3.0
     0 0 128 64   ; 4.0
     0 0 160 64   ; 5.0
     0 0 192 64   ; 6.0
     0 0 224 64   ; 7.0
     0 0 0 65)))  ; 8.0

(define buffer2
  (u8-list->bytevector
   '(0 0 0 63     ; 0.5
     0 0 64 63    ; 0.75
     0 0 128 63   ; 1.0
     0 0 160 63   ; 1.25
     0 0 192 63   ; 1.5
     0 0 224 63   ; 1.75
     0 0 0 64     ; 2.0
     0 0 16 64))) ; 2.25

(define multiplier
  (u8-list->bytevector
   (apply append (make-list 8 '(0 0 0 64))))) ; 2.0 repeated 8 times

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

;; Define symbol addresses
(define symbol-addresses
  '((buffer1 . #x1000)
    (buffer2 . #x2000)
    (result . #x3000)
    (multiplier . #x4000)))

(define linked-code (link-code assembled-code symbol-addresses))

;; Create a result buffer
(define result (make-bytevector 32 0))

(create-executable linked-code 
                   "my_executable"
                   `((buffer1 . ,buffer1)
                     (buffer2 . ,buffer2)
                     (result . ,result)
                     (multiplier . ,multiplier))
                   symbol-addresses)  ; Add this line to pass symbol-addresses

(display "Executable created: my_executable\n")
