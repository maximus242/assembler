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

(define result (make-bytevector 32 0))

;; Define symbol addresses (these will be relative to the start of .data section)
(define symbol-addresses
  '((buffer1 . 8192)        ; Updated to a valid address in the .data section
    (buffer2 . 8200)        ; Keeping in line with previous offset, now valid
    (result . 8208)         ; Keeping in line with previous offset, now valid
    (multiplier . 8216)))   ; Keeping in line with previous offset, now valid


;; Updated code using RIP-relative addressing
(define example-code
  '((label main)
    (push rbp)
    (mov rbp rsp)
    (lea rdi (rip buffer1))
    (lea rsi (rip buffer2))
    (lea rdx (rip result))
    (vmovaps ymm0 (rdi))
    (vmovaps ymm1 (rsi))
    (vaddps ymm2 ymm0 ymm1)
    (lea r8 (rip multiplier))
    (vmovaps ymm3 (r8))
    (vfmadd132ps ymm2 ymm2 ymm3)
    (vmovaps (rdx) ymm2)
    (vxorps ymm2 ymm2 ymm2)
    (vmovaps (rdx) ymm2)
    (mov.imm32 eax 0)
    (ret)))

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
