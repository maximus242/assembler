(define-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:export (create-program-headers))

(define (create-program-headers code-size data-size total-dynamic-size dynamic-offset)
  (let* ((num-headers 3)
         (header-size 56)
         (headers (make-bytevector (* num-headers header-size) 0))
         (text-addr #x1000)
         (data-addr (align-to (+ text-addr code-size) #x1000))
         (dynamic-addr dynamic-offset))
    
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
    (bytevector-u64-set! headers 88 (+ data-size (- dynamic-addr data-addr) total-dynamic-size) (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 96 (+ data-size (- dynamic-addr data-addr) total-dynamic-size) (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 104 #x1000 (endianness little))  ; p_align

    ;; DYNAMIC segment
    (bytevector-u32-set! headers 112 2 (endianness little))  ; p_type (PT_DYNAMIC)
    (bytevector-u32-set! headers 116 6 (endianness little))  ; p_flags (PF_R | PF_W)
    (bytevector-u64-set! headers 120 dynamic-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 128 dynamic-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 136 dynamic-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 144 total-dynamic-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 152 total-dynamic-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 160 8 (endianness little))  ; p_align

    headers))
