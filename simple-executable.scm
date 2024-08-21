(use-modules (linker)
             (shared-object-creator)
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
(bytevector-length multiplier)  ; Should return 32
(format #t "Buffer 1 Value:  ~a \n" (bytevector-length buffer1))

(define result (make-bytevector 32 0))

;; Define symbol addresses (these will be relative to the start of .data section)
(define symbol-addresses
  '((buffer1    . #x2000)  ; 8192 in decimal
    (buffer2    . #x2020)  ; 8224 in decimal
    (result     . #x2040)  ; 8256 in decimal
    (multiplier . #x2060)))  ; 8288 in decimal

(define example-code
  '((label perform_operations)
    (push rbp)
    (mov rbp rsp)
    
    ; Load addresses using GOTPCREL for PIC
    (lea rdi (rip buffer1@GOTPCREL))
    (lea rsi (rip buffer2@GOTPCREL))
    (lea rdx (rip result@GOTPCREL))
    (lea r8 (rip multiplier@GOTPCREL))
    
    ; Dereference GOT entries
    (mov rdi (rdi))
    (mov rsi (rsi))
    (mov rdx (rdx))
    (mov r8 (r8))
    
    ; SIMD operations
    (vmovaps ymm0 (rdi))
    (vmovaps ymm1 (rsi))
    (vaddps ymm2 ymm1 ymm1)
    (vmovaps ymm3 (r8))
    (vfmadd132ps ymm2 ymm3 ymm0)
    (vmovaps (rdx) ymm2)
    
    ; XOR operation
    (vxorps ymm2 ymm1 ymm2)
    (vmovaps (rdx) ymm2)
    
    ; End of function
    (xor eax eax)
    (pop rbp)
    (ret)
    ))

(define assembled-code (assemble example-code))
(define label-positions (get-label-positions))

;; Create the shared object file
(create-shared-object
 assembled-code
 `((buffer1 . ,buffer1)
   (buffer2 . ,buffer2)
   (result . ,result)
   (multiplier . ,multiplier))
 "output.so"
 symbol-addresses
 label-positions)

;; Add logging
(display "Shared object file created: output.so\n")
(display "Label positions:\n")
(hash-for-each (lambda (label pos)
                 (format #t "  ~a: 0x~x\n" label pos))
               label-positions)
