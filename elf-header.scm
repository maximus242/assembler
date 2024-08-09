(define-module (elf-header)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (create-elf-header))

;; Constants
(define ELF-MAGIC #x464c457f)
(define ELF-CLASS-64 2)
(define ELF-DATA-LSB 1)
(define ELF-VERSION-CURRENT 1)
(define ELF-OSABI-LINUX 3)
(define ELF-TYPE-SHARED 3)
(define ELF-MACHINE-X86-64 #x3e)
(define ELF-HEADER-SIZE 64)
(define PROGRAM-HEADER-SIZE 56)

;; ELF Header structure
(define-record-type <elf-header>
  (make-elf-header entry-point ph-offset ph-size sh-offset ph-count sh-count total-size shstrtab-index)
  elf-header?
  (entry-point elf-header-entry-point)
  (ph-offset elf-header-ph-offset)
  (ph-size elf-header-ph-size)
  (sh-offset elf-header-sh-offset)
  (ph-count elf-header-ph-count)
  (sh-count elf-header-sh-count)
  (total-size elf-header-total-size)
  (shstrtab-index elf-header-shstrtab-index))

(define (create-elf-header entry-point program-headers-offset program-headers-size 
                           section-headers-offset num-program-headers num-sections
                           total-size shstrtab-index)
  (let ((header (make-elf-header 
                  (max entry-point #x1000)
                  program-headers-offset
                  program-headers-size
                  section-headers-offset
                  (max num-program-headers 3)
                  (max num-sections 5)
                  total-size
                  shstrtab-index)))
    (elf-header->bytevector header)))

(define (elf-header->bytevector header)
  (let ((bv (make-bytevector ELF-HEADER-SIZE 0)))
    (bytevector-u32-set! bv 0 ELF-MAGIC (endianness little))
    (bytevector-u8-set! bv 4 ELF-CLASS-64)
    (bytevector-u8-set! bv 5 ELF-DATA-LSB)
    (bytevector-u8-set! bv 6 ELF-VERSION-CURRENT)
    (bytevector-u8-set! bv 7 ELF-OSABI-LINUX)
    (bytevector-u8-set! bv 8 0)  ; ABI version
    (bytevector-u16-set! bv 16 ELF-TYPE-SHARED (endianness little))
    (bytevector-u16-set! bv 18 ELF-MACHINE-X86-64 (endianness little))
    (bytevector-u32-set! bv 20 ELF-VERSION-CURRENT (endianness little))
    (bytevector-u64-set! bv 24 (elf-header-entry-point header) (endianness little))
    (bytevector-u64-set! bv 32 (elf-header-ph-offset header) (endianness little))
    (bytevector-u64-set! bv 40 (elf-header-sh-offset header) (endianness little))
    (bytevector-u32-set! bv 48 0 (endianness little))  ; Flags
    (bytevector-u16-set! bv 52 ELF-HEADER-SIZE (endianness little))
    (bytevector-u16-set! bv 54 PROGRAM-HEADER-SIZE (endianness little))
    (bytevector-u16-set! bv 56 (elf-header-ph-count header) (endianness little))
    (bytevector-u16-set! bv 58 64 (endianness little))  ; Section header size
    (bytevector-u16-set! bv 60 (elf-header-sh-count header) (endianness little))
    (bytevector-u16-set! bv 62 (elf-header-shstrtab-index header) (endianness little))
    bv))
