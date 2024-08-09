(define-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:export (create-program-headers))

(define (create-program-headers code-size data-size total-dynamic-size dynamic-offset dynamic-size)
  (let* ((num-headers 5)
         (header-size 56)
         (headers (make-bytevector (* num-headers header-size) 0))
         (phdr-size (* num-headers header-size))
         (text-addr #x1000)
         (data-addr (align-to (+ text-addr code-size) #x1000))
         (total-size (+ dynamic-offset total-dynamic-size))
         (dynamic-addr (align-to (+ data-addr data-size) #x1000))
         (relro-size (- dynamic-offset data-addr)))

    ;; Logging the computed addresses and sizes
    (format #t "Computed addresses and sizes:\n")
    (format #t "  text-addr=0x~x\n" text-addr)
    (format #t "  data-addr=0x~x\n" data-addr)
    (format #t "  dynamic-addr=0x~x\n" dynamic-addr)
    (format #t "  total-size=0x~x\n" total-size)
    (format #t "  relro-size=0x~x\n" relro-size)

    ;; PT_PHDR segment
    (format #t "Setting PT_PHDR segment:\n")
    (bytevector-u32-set! headers 0 6 (endianness little))  ; p_type (PT_PHDR)
    (bytevector-u32-set! headers 4 4 (endianness little))  ; p_flags (PF_R)
    (bytevector-u64-set! headers 8 (elf-header-size) (endianness little))  ; p_offset
    (bytevector-u64-set! headers 16 (+ text-addr (elf-header-size)) (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 24 (+ text-addr (elf-header-size)) (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 32 phdr-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 40 phdr-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 48 8 (endianness little))  ; p_align
    (log-header "PT_PHDR" 0 headers)

    ;; PT_LOAD segment for .text (includes ELF header and program headers)
    (format #t "Setting PT_LOAD segment for .text:\n")
    (bytevector-u32-set! headers 56 1 (endianness little))  ; p_type (PT_LOAD)
    (bytevector-u32-set! headers 60 5 (endianness little))  ; p_flags (PF_R | PF_X)
    (bytevector-u64-set! headers 64 0 (endianness little))  ; p_offset
    (bytevector-u64-set! headers 72 text-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 80 text-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 88 (+ (elf-header-size) phdr-size code-size) (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 96 (+ (elf-header-size) phdr-size code-size) (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 104 #x1000 (endianness little))  ; p_align
    (log-header "PT_LOAD (.text)" 56 headers)

    ;; PT_LOAD segment for .data, .dynamic, and other sections
    (format #t "Setting PT_LOAD segment for .data, .dynamic, and other sections:\n")
    (bytevector-u32-set! headers 112 1 (endianness little))  ; p_type (PT_LOAD)
    (bytevector-u32-set! headers 116 6 (endianness little))  ; p_flags (PF_R | PF_W)
    (bytevector-u64-set! headers 120 data-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 128 data-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 136 data-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 144 (- total-size data-addr) (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 152 (- total-size data-addr) (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 160 #x1000 (endianness little))  ; p_align
    (log-header "PT_LOAD (.data, .dynamic, etc.)" 112 headers)

    ;; PT_DYNAMIC segment
    (format #t "Setting PT_DYNAMIC segment:\n")
    (bytevector-u32-set! headers 168 2 (endianness little))  ; p_type (PT_DYNAMIC)
    (bytevector-u32-set! headers 172 6 (endianness little))  ; p_flags (PF_R | PF_W)
    (bytevector-u64-set! headers 176 dynamic-offset (endianness little))  ; p_offset
    (bytevector-u64-set! headers 184 dynamic-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 192 dynamic-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 200 dynamic-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 208 dynamic-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 216 8 (endianness little))  ; p_align
    (log-header "PT_DYNAMIC" 168 headers)

    ;; PT_GNU_RELRO segment
    (format #t "Setting PT_GNU_RELRO segment:\n")
    (bytevector-u32-set! headers 224 #x6474e552 (endianness little))  ; p_type (PT_GNU_RELRO)
    (bytevector-u32-set! headers 228 4 (endianness little))  ; p_flags (PF_R)
    (bytevector-u64-set! headers 232 data-addr (endianness little))  ; p_offset
    (bytevector-u64-set! headers 240 data-addr (endianness little))  ; p_vaddr
    (bytevector-u64-set! headers 248 data-addr (endianness little))  ; p_paddr
    (bytevector-u64-set! headers 256 relro-size (endianness little))  ; p_filesz
    (bytevector-u64-set! headers 264 relro-size (endianness little))  ; p_memsz
    (bytevector-u64-set! headers 272 1 (endianness little))  ; p_align
    (log-header "PT_GNU_RELRO" 224 headers)

    headers))

(define (elf-header-size) 64)

;; Helper function to log header details
(define (log-header name offset headers)
  (let ((p_type (bytevector-u32-ref headers offset (endianness little)))
        (p_flags (bytevector-u32-ref headers (+ offset 4) (endianness little)))
        (p_offset (bytevector-u64-ref headers (+ offset 8) (endianness little)))
        (p_vaddr (bytevector-u64-ref headers (+ offset 16) (endianness little)))
        (p_paddr (bytevector-u64-ref headers (+ offset 24) (endianness little)))
        (p_filesz (bytevector-u64-ref headers (+ offset 32) (endianness little)))
        (p_memsz (bytevector-u64-ref headers (+ offset 40) (endianness little)))
        (p_align (bytevector-u64-ref headers (+ offset 48) (endianness little))))
    (format #t "Logging ~a:\n  p_type=0x~x, p_flags=0x~x, p_offset=0x~x, p_vaddr=0x~x, p_paddr=0x~x, p_filesz=0x~x, p_memsz=0x~x, p_align=0x~x\n"
            name p_type p_flags p_offset p_vaddr p_paddr p_filesz p_memsz p_align)))
