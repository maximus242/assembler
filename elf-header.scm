(define-module (elf-header)
  #:use-module (rnrs bytevectors)
  #:export (create-elf-header))

(define (create-elf-header entry-point program-headers-offset program-headers-size 
                           section-headers-offset num-program-headers num-sections
                           total-size shstrtab-index)
  (let ((header (make-bytevector 64 0)))
    ;; ELF identification
    (bytevector-u32-set! header 0 #x464c457f (endianness little))  ; ELF magic number
    (bytevector-u8-set! header 4 2)  ; 64-bit format
    (bytevector-u8-set! header 5 1)  ; Little endian
    (bytevector-u8-set! header 6 1)  ; Current version of ELF
    (bytevector-u8-set! header 7 3)  ; Linux ABI (changed from System V)
    (bytevector-u8-set! header 8 0)  ; ABI version

    ;; ELF header fields
    (bytevector-u16-set! header 16 3 (endianness little))  ; Type: Shared object file
    (bytevector-u16-set! header 18 #x3e (endianness little))  ; Machine: AMD x86-64
    (bytevector-u32-set! header 20 1 (endianness little))  ; Version: Current
    (bytevector-u64-set! header 24 (max entry-point #x1000) (endianness little))  ; Entry point address (minimum 0x1000)
    (bytevector-u64-set! header 32 program-headers-offset (endianness little))  ; Program header offset
    (bytevector-u64-set! header 40 section-headers-offset (endianness little))  ; Section header offset
    (bytevector-u32-set! header 48 0 (endianness little))  ; Flags (architecture-specific, 0 for x86-64)
    (bytevector-u16-set! header 52 64 (endianness little))  ; Size of this header
    (bytevector-u16-set! header 54 56 (endianness little))  ; Size of program headers
    (bytevector-u16-set! header 56 (max num-program-headers 3) (endianness little))  ; Number of program headers (minimum 3)
    (bytevector-u16-set! header 58 64 (endianness little))  ; Size of section headers
    (bytevector-u16-set! header 60 (max num-sections 5) (endianness little))  ; Number of section headers (minimum 5)
    (bytevector-u16-set! header 62 shstrtab-index (endianness little))  ; Section header string table index
    header))
