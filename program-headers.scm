(define-module (program-headers)
               #:use-module (rnrs bytevectors)
               #:use-module (utils)
               #:use-module (srfi srfi-9)
               #:use-module (config)
               #:export (create-program-headers))

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

(define (create-program-headers 
          elf-header-size program-header-size num-program-headers
          text-addr code-size rodata-size bss-size data-size
          dynamic-addr dynamic-offset dynamic-size
          got-offset got-size plt-offset plt-size
          total-size alignment)

  (let* ((phdr-size (* num-program-headers program-header-size))
         (text-segment-size (+ code-size rodata-size))
         (text-segment-end (+ text-addr text-segment-size))
         (data-segment-start text-segment-end)
         (data-segment-file-size (- total-size text-segment-end))
         (data-segment-mem-size (+ data-segment-file-size bss-size))
         (relro-size (- got-offset data-segment-start)))

    (log-addresses-and-sizes 
      text-addr data-segment-start dynamic-addr total-size
      relro-size got-offset got-size plt-offset plt-size
      text-segment-size data-segment-file-size data-segment-mem-size)

    (let ((headers
            (list
              ;; PT_PHDR
              (make-program-header pt-phdr pf-r elf-header-size
                                   (+ text-addr elf-header-size) (+ text-addr elf-header-size)
                                   phdr-size phdr-size alignment)
              ;; PT_LOAD for .text and .rodata (read-only, executable)
              (make-program-header pt-load (logior pf-r pf-x) 0
                                   text-addr text-addr
                                   (+ elf-header-size phdr-size text-segment-size)
                                   (+ elf-header-size phdr-size text-segment-size)
                                   alignment)
              ;; PT_LOAD for .data, .bss, .dynamic, .got, .plt (read-write)
              (make-program-header pt-load (logior pf-r pf-w) 
                                   data-segment-start
                                   data-segment-start data-segment-start
                                   data-segment-file-size data-segment-mem-size
                                   alignment)
              ;; PT_DYNAMIC
              (make-program-header pt-dynamic (logior pf-r pf-w) dynamic-offset
                                   dynamic-addr dynamic-addr
                                   dynamic-size dynamic-size alignment)
              ;; PT_GNU_RELRO
              (make-program-header pt-gnu-relro pf-r 
                                   data-segment-start
                                   data-segment-start data-segment-start
                                   relro-size relro-size
                                   1))))

      (for-each log-program-header headers)
      (program-headers->bytevector headers))))

(define (log-addresses-and-sizes 
          text-addr data-addr dynamic-addr total-size
          relro-size got-offset got-size plt-offset plt-size
          text-segment-size data-segment-file-size data-segment-mem-size)
  (format #t "Computed addresses and sizes:
          text-addr=0x~x
          data-addr=0x~x
          dynamic-addr=0x~x
          total-size=0x~x
          relro-size=0x~x
          got-offset=0x~x
          got-size=0x~x
          plt-offset=0x~x
          plt-size=0x~x
          text-segment-size=0x~x
          data-segment-file-size=0x~x
          data-segment-mem-size=0x~x\n"
          text-addr data-addr dynamic-addr total-size
          relro-size got-offset got-size plt-offset plt-size
          text-segment-size data-segment-file-size data-segment-mem-size))

(define (log-program-header ph)
  (format #t "Program Header:
          type=0x~x, flags=0x~x, offset=0x~x,
          vaddr=0x~x, paddr=0x~x,
          filesz=0x~x, memsz=0x~x, align=0x~x\n"
          (ph-type ph) (ph-flags ph) (ph-offset ph)
          (ph-vaddr ph) (ph-paddr ph)
          (ph-filesz ph) (ph-memsz ph) (ph-align ph)))

(define (program-headers->bytevector headers)
  (let* ((bv (make-bytevector (* (length headers) program-header-size) 0)))
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
          (loop (cdr headers) (+ offset program-header-size)))))))
