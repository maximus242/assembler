(use-modules (linker)
             (shared-object-creator)
             (assembler)
             (rnrs bytevectors)
             (ice-9 receive))  ; For receiving multiple values

;; Define data
(define buffer1
  (u8-list->bytevector
    '(0 0 128 63    ; 1.0
      0 0 0 64      ; 2.0
      0 0 64 64     ; 3.0
      0 0 128 64    ; 4.0
      0 0 160 64    ; 5.0
      0 0 192 64    ; 6.0
      0 0 224 64    ; 7.0
      0 0 0 65)))   ; 8.0

(define buffer2
  (u8-list->bytevector
    '(0 0 0 65      ; 8.0
      0 0 224 64    ; 7.0
      0 0 192 64    ; 6.0
      0 0 160 64    ; 5.0
      0 0 128 64    ; 4.0
      0 0 64 64     ; 3.0
      0 0 0 64      ; 2.0
      0 0 128 63))) ; 1.0
(define multiplier
  (u8-list->bytevector
    (apply append (make-list 8 '(0 0 0 64)))))
(define result (make-bytevector 32 0))

(define __dso_handle (make-bytevector 32 0))


;; Log buffer sizes
(format #t "Buffer 1 length: ~a\n" (bytevector-length buffer1))
(format #t "Buffer 2 length: ~a\n" (bytevector-length buffer2))
(format #t "Multiplier length: ~a\n" (bytevector-length multiplier))
(format #t "Result buffer length: ~a\n" (bytevector-length result))

;; Define symbol addresses (these will be relative to the start of .data section)
(define symbol-addresses
  '((buffer1    . #x3020)  ; Start of .data section
    (buffer2    . #x3040)  ; 32 bytes after buffer1
    (result     . #x3060)  ; 32 bytes after buffer2
    (multiplier . #x3080)  ; 32 bytes after result
    ))

(define example-code
  '((label perform_operations)
    (push rbp)
    (mov rbp rsp)

    ; Load addresses using GOTPCREL for PIC
    (lea rdi (rip buffer1@GOTPCREL))
    (lea rsi (rip buffer2@GOTPCREL))
    (lea rdx (rip result@GOTPCREL))
    (lea rax (rip multiplier@GOTPCREL))

    ; Dereference GOT entries
    (mov rdi (rdi))
    (mov rsi (rsi))
    (mov rdx (rdx))
    (mov rax (rax))

    ; Addition
    (vmovaps (rdi) ymm0)
    (vmovaps (rsi) ymm1)
    (vaddps ymm1 ymm0 ymm0)
    (vmovaps ymm0 (rdx))

    ; Multiplication
    (vxorps ymm1 ymm1 ymm1)
    (vmovaps (rax) ymm2)
    (vfmadd132ps ymm0 ymm1 ymm2)
    (vmovaps ymm0 (rdx))

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
           "liboutput.so"
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
