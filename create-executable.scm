(use-modules ((linker) #:prefix linker:))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 regex))

(define (hex-string->bytevector hex-string)
  (u8-list->bytevector
    (map (lambda (hex-byte)
           (string->number hex-byte 16))
         (filter (lambda (s) (not (string-null? s)))
                 (string-split (regexp-substitute/global
                                 #f ";.*$" hex-string 'pre)
                               #\space)))))

(define (bytevector->hex-string bv)
  (string-join (map (lambda (byte)
                      (format #f "~2,'0x" byte))
                    (bytevector->u8-list bv))
               " "))

;; Example input code (you can modify this as needed)
(define assembled-code 
  #vu8(
    #x48 #x31 #xFF                      ; xor rdi, rdi (set exit code to 0)
    #x48 #xC7 #xC0 #x3C #x00 #x00 #x00  ; mov rax, 60 (sys_exit)
    #x0F #x05                           ; syscall
  ))

(display "Input code size: ")
(display (bytevector-length assembled-code))
(newline)

(display "Input code: ")
(display (bytevector->hex-string assembled-code))
(newline)

;; Link the code
(define linked-code (linker:link assembled-code))

(display "Linked code size: ")
(display (bytevector-length linked-code))
(newline)

(display "Linked code: ")
(display (bytevector->hex-string linked-code))
(newline)

;; Create the executable
(linker:create-executable linked-code "my_executable")

(display "Executable 'my_executable' created successfully.\n")