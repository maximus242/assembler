(define-module (section-headers)
               #:use-module (rnrs bytevectors)
               #:use-module (utils)
               #:use-module (string-table)
               #:use-module (srfi srfi-9)
               #:use-module (config)
               #:export (create-section-headers))

;; Define a record type for section headers
;; This allows us to create structured data for each section header
(define-record-type <section-header>
  (make-section-header name type flags addr offset size link info align entsize)
  section-header?
  (name sh-name)     ; Index into the section header string table
  (type sh-type)     ; Type of section (e.g., SHT_PROGBITS, SHT_NOBITS)
  (flags sh-flags)   ; Section attributes
  (addr sh-addr)     ; Virtual address of the section in memory
  (offset sh-offset) ; Offset of the section in the file
  (size sh-size)     ; Size of the section
  (link sh-link)     ; Section header table index link
  (info sh-info)     ; Extra information about the section
  (align sh-align)   ; Section alignment
  (entsize sh-entsize)) ; Size of each entry, for sections with fixed-size entries

;; Main function to create section headers
(define (create-section-headers
          text-addr 
          code-size 
          data-size 
          symtab-size 
          strtab-size 
          shstrtab-size 
          dynsym-size 
          dynstr-size 
          rela-size 
          total-dynamic-size 
          dynamic-size
          rela-offset 
          got-size 
          data-addr 
          dynamic-addr 
          dynsym-addr 
          dynstr-addr
          rela-addr 
          got-addr 
          plt-addr 
          symtab-offset 
          strtab-offset 
          shstrtab-addr)

  ;; Calculate the .plt section position
  (define rodata-offset (+ text-addr code-size))
  (define rodata-size 0)
  (define plt-offset (align-to (+ rodata-offset rodata-size) 16)) ; Place .plt after .rodata
  (define plt-size   #x40)                          ; Increased size for .plt (64 bytes)
  (define plt-addr   plt-offset)                    ; Use the same value for address and offset

  ;; Define the variables for .rela.plt section
  (define rela-plt-addr (+ plt-addr plt-size))      ; Place .rela.plt after .plt
  (define rela-plt-offset rela-plt-addr)            ; Use the same value for address and offset
  (define rela-plt-size (* 3 24))                   ; Assume 3 entries, each 24 bytes

  ;; Define variables for the .plt.got section
  (define plt-got-size #x20)                        ; Size for .plt.got (32 bytes)
  (define plt-got-addr (+ rela-plt-addr rela-plt-size)) ; Place .plt.got after .rela.plt
  (define plt-got-offset plt-got-addr)              ; Use the same value for address and offset

  ;; Calculate the .got.plt section position
  (define got-plt-addr (+ got-addr got-size))
  (define got-plt-offset got-plt-addr)
  (define got-plt-size #x18)

  ;; Create a list of all section headers
  (let ((headers
          (list
            ;; Null section (always first)
            (make-section-header
              0          ; name: No name (index 0 in string table)
              sht-null   ; type: Null section type
              0          ; flags: No flags
              0          ; addr: No address
              0          ; offset: No file offset
              0          ; size: No size
              0          ; link: No link
              0          ; info: No additional info
              0          ; align: No alignment
              0)         ; entsize: No entry size

            ;; .text section (code)
            (make-section-header
              1                                  ; name: Index of ".text" in string table
              sht-progbits                       ; type: Program bits (executable code)
              (logior shf-alloc shf-execinstr)   ; flags: Allocate memory and executable
              text-addr                          ; addr: Virtual address of .text section
              text-addr                          ; offset: File offset (same as addr for simplicity)
              code-size                          ; size: Size of the code
              0                                  ; link: No link
              0                                  ; info: No additional info
              16                                 ; align: Align to 16 bytes
              0)                                 ; entsize: No fixed entry size

            ;; .data section (initialized data)
            (make-section-header
              7                                  ; name: Index of ".data" in string table
              sht-progbits                       ; type: Program bits (initialized data)
              (logior shf-write shf-alloc)       ; flags: Writable and allocate memory
              data-addr                          ; addr: Virtual address of .data section
              data-addr                          ; offset: File offset (same as addr for simplicity)
              data-size                          ; size: Size of initialized data
              0                                  ; link: No link
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              0)                                 ; entsize: No fixed entry size

            ;; .bss section (uninitialized data)
            (make-section-header
              13                                 ; name: Index of ".bss" in string table
              sht-nobits                         ; type: No bits (uninitialized data)
              (logior shf-write shf-alloc)       ; flags: Writable and allocate memory
              #x5000; addr: Virtual address for .bss (updated)
              #x5000; offset: File offset (should be 0 for NOBITS, but keeping consistent)
              0                                  ; size: No size in file (uninitialized)
              0                                  ; link: No link
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              0)                                 ; entsize: No fixed entry size

            ;; .rodata section (read-only data)
            (make-section-header
              18                                 ; name: Index of ".rodata" in string table
              sht-progbits                       ; type: Program bits (read-only data)
              shf-alloc                          ; flags: Allocate memory (read-only)
              rodata-offset            ; addr: Virtual address after .text section
              rodata-offset            ; offset: File offset after .text section
              rodata-size                                  ; size: Size of read-only data (0 in this case)
              0                                  ; link: No link
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              0)                                 ; entsize: No fixed entry size

            ;; .dynamic section (dynamic linking information)
            (make-section-header
              63                                 ; name: Index of ".dynamic" in string table
              sht-dynamic                        ; type: Dynamic linking information
              (logior shf-write shf-alloc)       ; flags: Writable and allocate memory
              dynamic-addr                       ; addr: Virtual address of .dynamic section
              dynamic-addr                       ; offset: File offset of .dynamic section
              dynamic-size                       ; size: Size of .dynamic section
              7                                  ; link: Index of .dynstr section
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              dynamic-entry-size)                ; entsize: Size of each dynamic entry

            ;; .dynsym section (dynamic linking symbol table)
            (make-section-header
              80                                 ; name: Index of ".dynsym" in string table
              sht-dynsym                         ; type: Dynamic symbol table
              shf-alloc                          ; flags: Allocate memory
              dynsym-addr                        ; addr: Virtual address of .dynsym section
              dynsym-addr                        ; offset: File offset of .dynsym section
              dynsym-size                        ; size: Size of .dynsym section
              7                                  ; link: Index of .dynstr section
              1                                  ; info: Index of first non-local symbol
              8                                  ; align: Align to 8 bytes
              24)                                ; entsize: Size of each symbol entry (usually 24 bytes)

            ;; .dynstr section (dynamic linking string table)
            (make-section-header
              72                                 ; name: Index of ".dynstr" in string table
              sht-strtab                         ; type: String table
              shf-alloc                          ; flags: Allocate memory
              dynstr-addr                        ; addr: Virtual address of .dynstr section
              dynstr-addr                        ; offset: File offset of .dynstr section
              dynstr-size                        ; size: Size of .dynstr section
              0                                  ; link: No link
              0                                  ; info: No additional info
              1                                  ; align: Align to 1 byte (no alignment)
              0)                                 ; entsize: No fixed entry size for string tables

            ;; .rela.dyn section (relocation entries for non-PLT objects)
            (make-section-header
              88                                 ; name: Index of ".rela.dyn" in string table
              4                                  ; type: Relocation entries with addends
              shf-alloc                          ; flags: Allocate memory
              rela-addr                          ; addr: Virtual address of .rela.dyn section
              rela-addr                          ; offset: File offset of .rela.dyn section
              rela-size                          ; size: Size of .rela.dyn section
              6                                  ; link: Index
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              24)                                ; entsize: Size of each relocation entry (usually 24 bytes)


            ;; .got section (global offset table)
            (make-section-header
              98                                 ; name: Index of ".got" in string table
              sht-progbits                       ; type: Program bits
              (logior shf-write shf-alloc)       ; flags: Writable and allocate memory
              got-addr                           ; addr: Virtual address of .got section
              got-addr                           ; offset: File offset of .got section
              got-size                           ; size: Size of .got section
              0                                  ; link: No link
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              got-entry-size)                    ; entsize: Size of each GOT entry

            ;; .plt section (procedure linkage table)
            (make-section-header
              103                                ; name: Index of ".plt" in string table
              sht-progbits                       ; type: Program bits
              (logior shf-alloc shf-execinstr)   ; flags: Executable instruction only (removed shf-alloc)
              plt-addr ; addr: Virtual address of .plt section
              plt-offset ; offset: File offset of .plt section
              plt-size ; size: Size of .plt section (hardcoded to 32 bytes)
              0                                  ; link: No link
              0                                  ; info: No additional info
              16                                 ; align: Align to 16 bytes
              16)                                ; entsize: Size of each PLT entry (usually 16 bytes)

            ;; .symtab section (symbol table)
            (make-section-header
              26                                 ; name: Index of ".symtab" in string table
              sht-symtab                         ; type: Symbol table
              0                                  ; flags: No flags (not loaded at runtime)
              0                                  ; addr: No virtual address (not loaded)
              symtab-offset                      ; offset: File offset of .symtab section
              symtab-size                        ; size: Size of .symtab section
              12                                 ; link: Index of .strtab section
              1                                  ; info: Index of first non-local symbol
              8                                  ; align: Align to 8 bytes
              24)                                ; entsize: Size of each symbol entry (usually 24 bytes)

            ;; .strtab section (string table)
            (make-section-header
              34                                 ; name: Index of ".strtab" in string table
              sht-strtab                         ; type: String table
              0                                  ; flags: No flags (not loaded at runtime)
              0                                  ; addr: No virtual address (not loaded)
              strtab-offset                      ; offset: File offset of .strtab section
              strtab-size                        ; size: Size of .strtab section
              0                                  ; link: No link
              0                                  ; info: No additional info
              1                                  ; align: Align to 1 byte (no alignment)
              0)                                 ; entsize: No fixed entry size for string tables

            ;; .shstrtab section (section header string table)
            (make-section-header
              42                                 ; name: Index of ".shstrtab" in string table
              sht-strtab                         ; type: String table
              0                                  ; flags: No flags (not loaded at runtime)
              0                                  ; addr: No virtual address (not loaded)
              shstrtab-addr                      ; offset: File offset of .shstrtab section
              shstrtab-size                      ; size: Size of .shstrtab section
              0                                  ; link: No link
              0                                  ; info: No additional info
              1                                  ; align: Align to 1 byte (no alignment)
              0)                                 ; entsize: No fixed entry size for string tables

            ;; .rela.plt section
            (make-section-header
              #x7e                                ; name: Index of ".rela.plt" in string table
              sht-rela                            ; type: Relocation entries with addends
              shf-alloc                           ; flags: Allocate memory
              rela-plt-addr                       ; addr: Virtual address of .rela.plt section
              rela-plt-offset                     ; offset: File offset of .rela.plt section
              rela-plt-size                       ; size: Size of .rela.plt section
              6                                   ; link: Index of .dynsym section
              0                                   ; info: No additional info
              8                                   ; align: Align to 8 bytes
              24)                                 ; entsize: Size of each relocation entry (usually 24 bytes)

            ;; .plt.got section (GOT entry used by the PLT)
            (make-section-header
              108                                ; name: Index of ".plt.got" in string table
              sht-progbits                        ; type: Program bits
              (logior shf-write shf-alloc shf-execinstr) ; flags: Writable, Allocatable, Executable
              plt-got-addr                        ; addr: Virtual address of .plt.got section
              plt-got-addr                        ; offset: File offset of .plt.got section
              plt-got-size                        ; size: Size of .plt.got section
              0                                   ; link: No link
              0                                   ; info: No additional info
              8                                   ; align: Align to 8 bytes
              8)                                  ; entsize: Size of each .plt.got entry

            ;; .got.plt section (GOT entries for the PLT)
            (make-section-header
              117                                ; name: Index of ".got.plt" in string table
              sht-progbits                       ; type: Program bits
              (logior shf-write shf-alloc)       ; flags: Writable and allocate memory
              got-plt-addr                       ; addr: Virtual address of .got.plt section
              got-plt-addr                       ; offset: File offset of .got.plt section
              got-plt-size                       ; size: Size of .got.plt section
              0                                  ; link: No link
              0                                  ; info: No additional info
              8                                  ; align: Align to 8 bytes
              8)                                 ; entsize: Size of each .got.plt entry
            )))

    ;; Log the final section headers for debugging
    (log-section-headers headers)

    ;; Convert the section headers to a bytevector
    (section-headers->bytevector headers)))

;; Helper function to log all input parameters
(define (log-addresses-and-sizes text-addr data-addr dynamic-addr dynsym-addr 
                                 dynstr-addr rela-addr got-addr plt-addr 
                                 shstrtab-addr shstrtab-size got-size
                                 code-size data-size symtab-size strtab-size
                                 dynsym-size dynstr-size rela-size total-dynamic-size 
                                 dynamic-size rela-offset)
  (format #t "Computed addresses and sizes:\n")
  (for-each (lambda (name value)
              (format #t "  ~a=0x~x\n" name value))
            '(text-addr data-addr dynamic-addr dynsym-addr dynstr-addr 
                        rela-addr got-addr plt-addr shstrtab-addr shstrtab-size got-size
                        code-size data-size symtab-size strtab-size dynsym-size dynstr-size
                        rela-size total-dynamic-size dynamic-size rela-offset)
            (list text-addr data-addr dynamic-addr dynsym-addr dynstr-addr 
                  rela-addr got-addr plt-addr shstrtab-addr shstrtab-size got-size
                  code-size data-size symtab-size strtab-size dynsym-size dynstr-size
                  rela-size total-dynamic-size dynamic-size rela-offset)))

;; Helper function to log the final section headers
(define (log-section-headers headers)
  (format #t "Final section headers:\n")
  (for-each
    (lambda (index header)
      (format #t "Section [~a]: name=0x~x, type=0x~x, flags=0x~x, addr=0x~x, offset=0x~x, size=0x~x, link=0x~x, info=0x~x, align=0x~x, entsize=0x~x\n"
              index
              (sh-name header) (sh-type header) (sh-flags header)
              (sh-addr header) (sh-offset header) (sh-size header)
              (sh-link header) (sh-info header) (sh-align header) (sh-entsize header)))
    (iota (length headers))
    headers))

(define (section-headers->bytevector headers)
  (let* ((bv (make-bytevector (* (length headers) section-header-size) 0)))
    (for-each
      (lambda (index header)
        (let ((base (* index section-header-size)))
          (bytevector-u32-set! bv (+ base 0) (sh-name header) (endianness little))
          (bytevector-u32-set! bv (+ base 4) (sh-type header) (endianness little))
          (bytevector-u64-set! bv (+ base 8) (sh-flags header) (endianness little))
          (bytevector-u64-set! bv (+ base 16) (sh-addr header) (endianness little))
          (bytevector-u64-set! bv (+ base 24) (sh-offset header) (endianness little))
          (bytevector-u64-set! bv (+ base 32) (sh-size header) (endianness little))
          (bytevector-u32-set! bv (+ base 40) (sh-link header) (endianness little))
          (bytevector-u32-set! bv (+ base 44) (sh-info header) (endianness little))
          (bytevector-u64-set! bv (+ base 48) (sh-align header) (endianness little))
          (bytevector-u64-set! bv (+ base 56) (sh-entsize header) (endianness little))))
      (iota (length headers))
      headers)
    bv))
