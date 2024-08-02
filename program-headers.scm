(define-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:export (create-program-headers))

(define (create-program-header type offset vaddr paddr filesz memsz flags align)
  (let ((header (make-bytevector 56 0)))
    (bytevector-u32-set! header 0 type (endianness little))
    (bytevector-u32-set! header 4 flags (endianness little))
    (bytevector-u64-set! header 8 offset (endianness little))
    (bytevector-u64-set! header 16 vaddr (endianness little))
    (bytevector-u64-set! header 24 paddr (endianness little))
    (bytevector-u64-set! header 32 filesz (endianness little))
    (bytevector-u64-set! header 40 memsz (endianness little))
    (bytevector-u64-set! header 48 align (endianness little))
    header))

(define (create-program-headers code-size data-size)
  (let* ((text-segment (create-program-header 1 #x1000 #x1000 #x1000 code-size code-size 5 #x1000))
         (data-segment (create-program-header 1 (+ #x1000 (align-to code-size 16)) 
                                              (+ #x1000 (align-to code-size 16)) 
                                              (+ #x1000 (align-to code-size 16)) 
                                              data-size data-size 6 #x1000)))
    (bytevector-append text-segment data-segment)))
