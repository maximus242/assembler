(define-module (program-headers)
  #:use-module (rnrs bytevectors)
  #:use-module (utils)
  #:use-module (srfi srfi-9)
  #:export (create-program-headers))

;; Constants
(define PT_LOAD 1)
(define PT_DYNAMIC 2)
(define PT_PHDR 6)
(define PT_GNU_RELRO #x6474e552)

(define PF_X #b001)
(define PF_W #b010)
(define PF_R #b100)

;; Program header record
(define-record-type <program-header>
  (make-program-header type flags offset vaddr paddr filesz memsz align)
  program-header?
  (type ph-type)
  (flags ph-flags)
  (offset ph-offset)
  (vaddr ph-vaddr)
  (paddr ph-paddr)
  (filesz ph-filesz)
  (memsz ph-memsz)
  (align ph-align))

(define (create-program-headers code-size data-size total-dynamic-size dynamic-offset dynamic-size)
  (let* ((elf-header-size 64)
         (num-headers 5)
         (header-size 56)
         (phdr-size (* num-headers header-size))
         (text-addr #x1000)
         (data-addr (align-to (+ text-addr code-size) #x1000))
         (total-size (+ dynamic-offset total-dynamic-size))
         (dynamic-addr (align-to (+ data-addr data-size) #x1000))
         (relro-size (- dynamic-offset data-addr)))

    (log-addresses-and-sizes text-addr data-addr dynamic-addr total-size relro-size)

    (let ((headers
           (list
            ;; PT_PHDR
            (make-program-header PT_PHDR PF_R elf-header-size
                                 (+ text-addr elf-header-size) (+ text-addr elf-header-size)
                                 phdr-size phdr-size 8)
            ;; PT_LOAD for .text
            (make-program-header PT_LOAD (logior PF_R PF_X) 0
                                 text-addr text-addr
                                 (+ elf-header-size phdr-size code-size)
                                 (+ elf-header-size phdr-size code-size)
                                 #x1000)
            ;; PT_LOAD for .data, .dynamic, etc.
            (make-program-header PT_LOAD (logior PF_R PF_W) data-addr
                                 data-addr data-addr
                                 (- total-size data-addr) (- total-size data-addr)
                                 #x1000)
            ;; PT_DYNAMIC
            (make-program-header PT_DYNAMIC (logior PF_R PF_W) dynamic-offset
                                 dynamic-addr dynamic-addr
                                 dynamic-size dynamic-size 8)
            ;; PT_GNU_RELRO
            (make-program-header PT_GNU_RELRO PF_R data-addr
                                 data-addr data-addr
                                 relro-size relro-size 1))))

      (for-each log-program-header headers)
      (program-headers->bytevector headers))))

(define (log-addresses-and-sizes text-addr data-addr dynamic-addr total-size relro-size)
  (format #t "Computed addresses and sizes:
  text-addr=0x~x
  data-addr=0x~x
  dynamic-addr=0x~x
  total-size=0x~x
  relro-size=0x~x\n"
          text-addr data-addr dynamic-addr total-size relro-size))

(define (log-program-header ph)
  (format #t "Program Header:
  type=0x~x, flags=0x~x, offset=0x~x,
  vaddr=0x~x, paddr=0x~x,
  filesz=0x~x, memsz=0x~x, align=0x~x\n"
          (ph-type ph) (ph-flags ph) (ph-offset ph)
          (ph-vaddr ph) (ph-paddr ph)
          (ph-filesz ph) (ph-memsz ph) (ph-align ph)))

(define (program-headers->bytevector headers)
  (let* ((header-size 56)
         (bv (make-bytevector (* (length headers) header-size) 0)))
    (let loop ((headers headers) (offset 0))
      (if (null? headers)
          bv
          (let ((ph (car headers)))
            (bytevector-u32-set! bv offset (ph-type ph) (endianness little))
            (bytevector-u32-set! bv (+ offset 4) (ph-flags ph) (endianness little))
            (bytevector-u64-set! bv (+ offset 8) (ph-offset ph) (endianness little))
            (bytevector-u64-set! bv (+ offset 16) (ph-vaddr ph) (endianness little))
            (bytevector-u64-set! bv (+ offset 24) (ph-paddr ph) (endianness little))
            (bytevector-u64-set! bv (+ offset 32) (ph-filesz ph) (endianness little))
            (bytevector-u64-set! bv (+ offset 40) (ph-memsz ph) (endianness little))
            (bytevector-u64-set! bv (+ offset 48) (ph-align ph) (endianness little))
            (loop (cdr headers) (+ offset header-size)))))))
