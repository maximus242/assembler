(define-module (program-headers)
               #:use-module (rnrs bytevectors)
               #:use-module (utils)
               #:use-module (srfi srfi-9)
               #:use-module (config)
               #:use-module (elf-layout-calculator)
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

;; Calculates the size of the text segment
(define (calculate-text-segment-size code-size rodata-size plt-size)
  (+ code-size rodata-size plt-size))

;; Calculates the end address of the text segment
(define (calculate-text-segment-end text-addr text-segment-size)
  (+ text-addr text-segment-size))

;; Calculates the start address of the data segment based on the end of the text segment
(define (calculate-data-segment-start text-segment-end alignment)
  (align-up text-segment-end alignment))

;; Calculate the file size for the second PT_LOAD segment
(define (calculate-data-segment-file-size total-size data-segment-start)
  ;; Since .bss is at the end and has no file size, the file size is calculated normally
  (- total-size data-segment-start))

;; Calculate the memory size for the second PT_LOAD segment
(define (calculate-data-segment-mem-size data-segment-file-size bss-size)
  ;; Include the size of the .bss section in the memory size
  (+ data-segment-file-size bss-size))

;; Calculates the size of the RELRO segment
(define (calculate-relro-size data-segment-start dynamic-addr)
  (- dynamic-addr data-segment-start))

(define (calculate-first-load-size text-addr code-size rodata-size)
  (+ code-size rodata-size))

(define (create-program-headers 
          elf-header-size program-header-size num-program-headers
          text-addr code-size rodata-size bss-size data-size
          dynamic-addr dynamic-offset dynamic-size
          got-offset got-size plt-offset plt-size
          total-size alignment)

  (format #t "\n--- Input Parameters ---\n")
  (format #t "elf-header-size: 0x~x\n" elf-header-size)
  (format #t "program-header-size: 0x~x\n" program-header-size)
  (format #t "num-program-headers: ~a\n" num-program-headers)
  (format #t "text-addr: 0x~x\n" text-addr)
  (format #t "code-size: 0x~x\n" code-size)
  (format #t "rodata-size: 0x~x\n" rodata-size)
  (format #t "bss-size: 0x~x\n" bss-size)
  (format #t "data-size: 0x~x\n" data-size)
  (format #t "dynamic-addr: 0x~x\n" dynamic-addr)
  (format #t "dynamic-offset: 0x~x\n" dynamic-offset)
  (format #t "dynamic-size: 0x~x\n" dynamic-size)
  (format #t "got-offset: 0x~x\n" got-offset)
  (format #t "got-size: 0x~x\n" got-size)
  (format #t "plt-offset: 0x~x\n" plt-offset)
  (format #t "plt-size: 0x~x\n" plt-size)
  (format #t "total-size: 0x~x\n" total-size)
  (format #t "alignment: 0x~x\n" alignment)

  ;; Calculate dependent values
  (let* ((text-segment-size (calculate-text-segment-size code-size rodata-size plt-size))
         (text-segment-end (calculate-text-segment-end text-addr text-segment-size))
         (data-segment-start (calculate-data-segment-start text-segment-end alignment))
         (data-segment-file-size (calculate-data-segment-file-size total-size data-segment-start))
         (data-segment-mem-size (calculate-data-segment-mem-size data-segment-file-size bss-size))
         (relro-size (calculate-relro-size data-segment-start dynamic-addr))
         (phdr-size (calculate-phdr-size num-program-headers program-header-size))
         ;; Place .bss at the end after all other sections, ensuring alignment
         (bss-addr (align-up total-size alignment))
         (phdr-offset (align-up elf-header-size alignment))
         (first-load-size (max (+ text-addr text-segment-size)
                               (+ phdr-offset phdr-size))))

    (format #t "\n--- Calculated Values ---\n")
    (format #t "text-segment-size: 0x~x\n" text-segment-size)
    (format #t "text-segment-end: 0x~x\n" text-segment-end)
    (format #t "data-segment-start: 0x~x\n" data-segment-start)
    (format #t "data-segment-file-size: 0x~x\n" data-segment-file-size)
    (format #t "data-segment-mem-size: 0x~x\n" data-segment-mem-size)
    (format #t "relro-size: 0x~x\n" relro-size)
    (format #t "phdr-size: 0x~x\n" phdr-size)
    (format #t "bss-addr: 0x~x\n" bss-addr)

    (format #t "\nBSS Placement Information:\n")
    (format #t "data_segment_start: 0x~x\n" data-segment-start)
    (format #t "data_size: 0x~x\n" data-size)
    (format #t "bss_addr: 0x~x\n" bss-addr)
    (format #t "bss-size: 0x~x\n" bss-size)

    (log-addresses-and-sizes 
      text-addr data-segment-start dynamic-addr total-size
      relro-size got-offset got-size plt-offset plt-size
      text-segment-size data-segment-file-size data-segment-mem-size
      bss-size bss-addr) 

    (let ((headers
            (list
              (make-program-header pt-phdr pf-r elf-header-size
                                   (+ text-addr elf-header-size) (+ text-addr elf-header-size)
                                   phdr-size phdr-size alignment)
              ;; First LOAD segment (RX) - includes .text and .rodata
              (make-program-header pt-load (logior pf-r pf-x) 0
                                   text-addr text-addr
                                   first-load-size first-load-size
                                   alignment)
              ;; Second LOAD segment (RW) - for .data
              (make-program-header pt-load (logior pf-r pf-w)
                                   data-segment-start
                                   data-segment-start data-segment-start
                                   (- dynamic-offset data-segment-start)
                                   (+ (- dynamic-offset data-segment-start) bss-size)
                                   alignment)
              ;; Third LOAD segment (RWX) - starting from .dynamic, includes .plt and ends with .bss
              (make-program-header pt-load (logior pf-r pf-w pf-x) dynamic-offset
                                   dynamic-addr dynamic-addr
                                   dynamic-size dynamic-size alignment)
              ;; PT_DYNAMIC
              (make-program-header pt-dynamic (logior pf-r pf-w) dynamic-offset
                                   dynamic-addr dynamic-addr
                                   dynamic-size dynamic-size alignment)

              ;; GNU_RELRO
              (make-program-header pt-gnu-relro pf-r 
                                   data-segment-start
                                   data-segment-start data-segment-start
                                   relro-size relro-size
                                   1)

              (make-program-header
                pt-load            ;; Type for LOAD segment
                pf-r               ;; Writable flag for program header
                #x0                ;; offset: File offset, should be 0 for NOBITS
                #x5000             ;; vaddr: Hardcoded start address for .bss (virtual address)
                #x5000             ;; paddr: Hardcoded physical address (same as virtual in this case)
                #x0                ;; filesz: Should be 0 because .bss is NOBITS
                #x0             ;; memsz: Hardcoded size for .bss in memory
                #x1000             ;; align: Usually 0x1000 for page alignment
                )

              )))

      (format #t "\n--- Generated Program Headers ---\n")
      (for-each (lambda (ph index)
                  (format #t "\nProgram Header ~a:\n" (+ index 1))
                  (log-program-header ph))
                headers
                (iota (length headers)))

      (program-headers->bytevector headers))))

;; Helper function to align addresses
(define (align-up address alignment)
  (let ((remainder (modulo address alignment)))
    (if (zero? remainder)
      address
      (+ address (- alignment remainder)))))

(define (log-addresses-and-sizes 
          text-addr data-addr dynamic-addr total-size
          relro-size got-offset got-size plt-offset plt-size
          text-segment-size data-segment-file-size data-segment-mem-size
          bss-size bss-addr)  ; Added bss-size and bss-addr
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
          data-segment-mem-size=0x~x
          bss-size=0x~x
          bss-addr=0x~x\n"
          text-addr data-addr dynamic-addr total-size
          relro-size got-offset got-size plt-offset plt-size
          text-segment-size data-segment-file-size data-segment-mem-size
          bss-size bss-addr))

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
