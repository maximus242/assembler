(define-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:export (create-program-headers))

(define (create-program-headers code-size data-size total-dynamic-size dynamic-offset)
  (let* ((num-headers 5)
         (header-size 56)
         (headers (make-bytevector (* num-headers header-size) 0))
         (phdr-size (* num-headers header-size))
         (text-addr #x1000)
         (data-addr (align-to (+ text-addr code-size) #x1000))
         (total-size (+ dynamic-offset total-dynamic-size))
         (relro-size (- dynamic-offset data-addr)))
    
    ;; PT_PHDR segment
    (bytevector-u32-set! headers 0 6 (endianness little))  ; p_type (PT_PHDR)
    (bytevector-u32-set! headers 4 4 (endianness little))  ; p_flags (PF_R)
    (bytevector-u64-set! headers 8 (elf-header-size) (endianness little))  ; p_offset
    (bytevector-u64-set! headers 16 (+ text-addr (elf-header-size)) (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 24 (+ text-addr (elf-header-size)) (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 32 phdr-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 40 phdr-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 48 8 (endianness little))  ; p_align

    ;; PT_LOAD segment for .text (includes ELF header and program headers)
    (bytevector-u32-set! headers 56 1 (endianness little))  ; p_type (PT_LOAD)
    (bytevector-u32-set! headers 60 5 (endianness little))  ; p_flags (PF_R | PF_X)
    (bytevector-u64-set! headers 64 0 (endianness little))  ; p_offset
    (bytevector-u64-set! headers 72 text-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 80 text-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 88 (+ (elf-header-size) phdr-size code-size) (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 96 (+ (elf-header-size) phdr-size code-size) (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 104 #x1000 (endianness little))  ; p_align

    ;; PT_LOAD segment for .data, .dynamic, and other sections
    (bytevector-u32-set! headers 112 1 (endianness little))  ; p_type (PT_LOAD)
    (bytevector-u32-set! headers 116 6 (endianness little))  ; p_flags (PF_R | PF_W)
    (bytevector-u64-set! headers 120 data-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 128 data-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 136 data-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 144 (- total-size data-addr) (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 152 (- total-size data-addr) (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 160 #x1000 (endianness little))  ; p_align

    ;; PT_DYNAMIC segment
    (bytevector-u32-set! headers 168 2 (endianness little))  ; p_type (PT_DYNAMIC)
    (bytevector-u32-set! headers 172 6 (endianness little))  ; p_flags (PF_R | PF_W)
    (bytevector-u64-set! headers 176 dynamic-offset (endianness little))  ; p_offset
    (bytevector-u64-set! headers 184 (+ data-addr (- dynamic-offset data-addr)) (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 192 (+ data-addr (- dynamic-offset data-addr)) (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 200 total-dynamic-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 208 total-dynamic-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 216 8 (endianness little))  ; p_align

    ;; PT_GNU_RELRO segment
    (bytevector-u32-set! headers 224 #x6474e552 (endianness little))  ; p_type (PT_GNU_RELRO)
    (bytevector-u32-set! headers 228 4 (endianness little))  ; p_flags (PF_R)
    (bytevector-u64-set! headers 232 data-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 240 data-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 248 data-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 256 relro-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 264 relro-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 272 1 (endianness little))  ; p_align

    headers))

(define (elf-header-size) 64)
