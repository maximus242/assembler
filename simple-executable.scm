(use-modules (linker)
             (assembler)
             (rnrs bytevectors))

;; No data buffers needed for this simple example

(define symbol-addresses '())  ; No symbols needed

;; Simplest possible code: just exit the program
(define example-code
  '((mov.imm32 eax 60)  ; 60 is the syscall number for exit
    (xor edi edi)       ; 0 exit status
    (syscall)))

(define assembled-code (assemble example-code))
(define linked-code (link-code assembled-code symbol-addresses))

;; Create the executable
(create-executable 
 linked-code 
 "minimal_executable"
 '()  ; No data to include
 symbol-addresses)

(display "Executable created: minimal_executable\n")
