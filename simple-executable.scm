(use-modules (linker)
             (shared-object-creator)
             (assembler)
             (rnrs bytevectors)
             (ice-9 receive))  ; For receiving multiple values

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

;; Log buffer sizes
(format #t "Buffer 1 length: ~a\n" (bytevector-length buffer1))
(format #t "Buffer 2 length: ~a\n" (bytevector-length buffer2))
(format #t "Multiplier length: ~a\n" (bytevector-length multiplier))
(format #t "Result buffer length: ~a\n" (bytevector-length result))

;; Define symbol addresses (these will be relative to the start of .data section)
(define symbol-addresses
  '((buffer1    . #x3180)
    (buffer2    . #x31a0)
    (result     . #x31a0)
    (multiplier . #x31e0)))

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
    (ret)))

;; Assemble the code and get the relocation table
(receive (assembled-code relocation-table)
    (assemble example-code)
  
  (define label-positions (get-label-positions))
  
  ;; Log assembled code and relocation table
  (format #t "Assembled code length: ~a bytes\n" (bytevector-length assembled-code))
  (format #t "Relocation table entries: ~a\n" (length relocation-table))
  
  ;; Log relocation table
  (format #t "Relocation table:\n")
  (for-each
   (lambda (entry)
     (format #t "  Offset: 0x~x, Type: ~a, Symbol: ~a\n"
             (car entry) (cadr entry) (caddr entry)))
   relocation-table)
  
  ;; Create the shared object file
  (create-shared-object
   assembled-code
   `((buffer1 . ,buffer1)
    (buffer2 . ,buffer2)
    (result . ,result)
    (multiplier . ,multiplier))
   "output.so"
   symbol-addresses
   label-positions
   relocation-table)  ; Pass the relocation table to create-shared-object
  
  ;; Add logging
  (display "Shared object file created: output.so\n")
  (display "Label positions:\n")
  (hash-for-each 
   (lambda (label pos)
     (format #t "  ~a: 0x~x\n" label pos))
   label-positions))
