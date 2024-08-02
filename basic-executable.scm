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

;; Define symbol addresses (these will be relative to the start of .data section)
(define symbol-addresses
  '((buffer1 . 0)
    (buffer2 . 32)
    (result . 64)
    (multiplier . 96)))

;; Updated code using RIP-relative addressing
(define example-code
  '((label _start)
    (lea rdi (rip buffer1))
    (mov.imm32 eax 60)       ; 60 is the syscall number for exit
    (xor edi edi)            ; 0 exit status
    (syscall)))

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
