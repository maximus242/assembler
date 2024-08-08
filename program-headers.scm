(define-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:export (create-program-headers))

(define (create-program-headers code-size data-size total-dynamic-size dynamic-offset)
  (let* ((num-headers 2)
         (header-size 56)
         (headers (make-bytevector (* num-headers header-size) 0))
         (text-addr #x1000)
         (data-addr (align-to (+ text-addr code-size) #x1000))
         (total-data-size (+ data-size total-dynamic-size)))
    
    ;; LOAD segment for .text
    (bytevector-u32-set! headers 0 1 (endianness little))  ; p_type (PT_LOAD)
    (bytevector-u32-set! headers 4 5 (endianness little))  ; p_flags (PF_R | PF_X)
    (bytevector-u64-set! headers 8 text-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 16 text-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 24 text-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 32 code-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 40 code-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 48 #x1000 (endianness little))  ; p_align

    ;; LOAD segment for .data and .dynamic
    (bytevector-u32-set! headers 56 1 (endianness little))  ; p_type (PT_LOAD)
    (bytevector-u32-set! headers 60 6 (endianness little))  ; p_flags (PF_R | PF_W)
    (bytevector-u64-set! headers 64 data-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 72 data-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 80 data-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 88 total-data-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 96 total-data-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 104 #x1000 (endianness little))  ; p_align

    headers))