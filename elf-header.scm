(define-module (elf-header)
  #:use-module (rnrs bytevectors)
  #:export (create-elf-header))

(define (create-elf-header entry-point section-header-offset num-sections)
  (let ((header (make-bytevector 64 0)))
    (bytevector-u32-set! header 0 #x464c457f (endianness little))  ; ELF magic number
    (bytevector-u8-set! header 4 2)  ; 64-bit format
    (bytevector-u8-set! header 5 1)  ; Little endian
    (bytevector-u8-set! header 6 1)  ; Current version of ELF
    (bytevector-u8-set! header 7 0)  ; System V ABI
    (bytevector-u8-set! header 8 0)  ; ABI version
    (bytevector-u16-set! header 16 3 (endianness little))  ; Type: Shared object file
    (bytevector-u16-set! header 18 #x3e (endianness little))  ; Machine: AMD x86-64
    (bytevector-u32-set! header 20 1 (endianness little))  ; Version: Current
    (bytevector-u64-set! header 24 entry-point (endianness little))  ; Entry point address
    (bytevector-u64-set! header 32 64 (endianness little))  ; Program header offset
    (bytevector-u64-set! header 40 section-header-offset (endianness little))  ; Section header offset
    (bytevector-u32-set! header 48 0 (endianness little))  ; Flags
    (bytevector-u16-set! header 52 64 (endianness little))  ; Size of this header
    (bytevector-u16-set! header 54 56 (endianness little))  ; Size of program headers
    (bytevector-u16-set! header 56 2 (endianness little))   ; Number of program headers
    (bytevector-u16-set! header 58 64 (endianness little))   ; Size of section headers
    (bytevector-u16-set! header 60 num-sections (endianness little))   ; Number of section headers
    (bytevector-u16-set! header 62 5 (endianness little))   ; Section header string table index (point to .shstrtab)
    header))


