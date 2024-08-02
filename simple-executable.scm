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
(define symbol-addresses
  '((buffer1 . #x402000)
    (buffer2 . #x402020)
    (result . #x402040)
    (multiplier . #x402060)))

;; Simplest possible code: just exit the program
(define example-code
  '((mov.imm32 rdi buffer1)
    (mov.imm32 rsi buffer2)
    (mov.imm32 rdx result)
    (vmovaps ymm0 (rdi))
    (mov.imm32 eax 60)  ; 60 is the syscall number for exit
    (xor edi edi)       ; 0 exit status
    (syscall)))

(define assembled-code (assemble example-code))
(define linked-code (link-code assembled-code symbol-addresses))

;; Create the executable
(create-executable 
 linked-code 
 "simple_executable"
 `((buffer1 . ,buffer1)
   (buffer2 . ,buffer2)
   (result . ,result)
   (multiplier . ,multiplier))
 symbol-addresses)

;; Add logging
(display "Logging information:\n")
(display (string-append "buffer1 address: " (number->string (cdr (assoc 'buffer1 symbol-addresses))) "\n"))
(display (string-append "buffer2 address: " (number->string (cdr (assoc 'buffer2 symbol-addresses))) "\n"))
(display (string-append "result address: " (number->string (cdr (assoc 'result symbol-addresses))) "\n"))
(display (string-append "multiplier address: " (number->string (cdr (assoc 'multiplier symbol-addresses))) "\n"))

(display "Executable created: simple_executable\n")