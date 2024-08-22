(define-module (elf-header)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (config)
  #:export (create-elf-header))

;; ELF Header structure
(define-record-type <elf-header>
  (make-elf-header entry-point ph-offset ph-size sh-offset ph-count sh-count 
                   total-size shstrtab-index hash-offset hash-size)
  elf-header?
  (entry-point elf-header-entry-point)
  (ph-offset elf-header-ph-offset)
  (ph-size elf-header-ph-size)
  (sh-offset elf-header-sh-offset)
  (ph-count elf-header-ph-count)
  (sh-count elf-header-sh-count)
  (total-size elf-header-total-size)
  (shstrtab-index elf-header-shstrtab-index)
  (hash-offset elf-header-hash-offset)
  (hash-size elf-header-hash-size))

(define (create-elf-header entry-point program-headers-offset program-headers-size 
                           section-headers-offset num-program-headers num-sections
                           total-size shstrtab-index hash-offset hash-size)

  (format #t "Creating ELF header with ~a sections.\n" num-sections)

  (let ((header (make-elf-header 
                  (max entry-point #x1000)
                  program-headers-offset
                  program-headers-size
                  section-headers-offset
                  num-program-headers
                  num-sections
                  total-size
                  shstrtab-index
                  hash-offset
                  hash-size)))
    (elf-header->bytevector header)))

(define (elf-header->bytevector header)
  (let ((bv (make-bytevector elf-header-size 0)))
    (bytevector-u32-set! bv 0 elf-magic (endianness little))
    (bytevector-u8-set! bv 4 elf-class-64)
    (bytevector-u8-set! bv 5 elf-data-lsb)
    (bytevector-u8-set! bv 6 elf-version-current)
    (bytevector-u8-set! bv 7 elf-osabi-linux)
    (bytevector-u8-set! bv 8 0)  ; ABI version
    (bytevector-u16-set! bv 16 elf-type-shared (endianness little))
    (bytevector-u16-set! bv 18 elf-machine-x86-64 (endianness little))
    (bytevector-u32-set! bv 20 elf-version-current (endianness little))
    (bytevector-u64-set! bv 24 (elf-header-entry-point header) (endianness little))
    (bytevector-u64-set! bv 32 (elf-header-ph-offset header) (endianness little))
    (bytevector-u64-set! bv 40 (elf-header-sh-offset header) (endianness little))
    (bytevector-u32-set! bv 48 0 (endianness little))  ; Flags
    (bytevector-u16-set! bv 52 elf-header-size (endianness little))
    (bytevector-u16-set! bv 54 program-header-size (endianness little))
    (bytevector-u16-set! bv 56 (elf-header-ph-count header) (endianness little))
    (bytevector-u16-set! bv 58 section-header-size (endianness little))
    (bytevector-u16-set! bv 60 (elf-header-sh-count header) (endianness little))
    (bytevector-u16-set! bv 62 (elf-header-shstrtab-index header) (endianness little))
    bv))

;; Helper function to get hash section information
(define (get-hash-section-info header)
  (values (elf-header-hash-offset header)
          (elf-header-hash-size header)))
