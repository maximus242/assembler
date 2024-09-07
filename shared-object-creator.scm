(define-module (shared-object-creator)
               #:use-module (config)
               #:use-module (elf-header)
               #:use-module (program-headers)
               #:use-module (section-headers)
               #:use-module (dynamic-section)
               #:use-module (symbol-table)
               #:use-module (string-table)
               #:use-module (rnrs bytevectors)
               #:use-module (rnrs io ports)
               #:use-module (ice-9 format)
               #:use-module (relocation-table)
               #:use-module (elf-layout-calculator)
               #:use-module (elf-dynamic-calculator)
               #:use-module (plt-section)
               #:use-module (rela-plt-section)
               #:use-module (got-plt-section)
               #:use-module (plt-got-section)
               #:use-module (linker)
               #:use-module (validate-relocations)
               #:use-module (data-section)
               #:use-module (note-gnu-build-id-section)
               #:export (create-shared-object
                          custom-assert
                          verify-dynamic-section
                          check-section-overlaps
                          verify-segment-contents))

(define (create-reg-to-symbol-map symbol-addresses)
  (let ((map (make-hash-table)))
    (let loop ((symbols (if (hash-table? symbol-addresses)
                          (hash-map->list cons symbol-addresses)
                          symbol-addresses))
               (reg 0))
      (if (or (null? symbols) (>= reg 16))
        map
        (let* ((symbol-pair (car symbols))
               (symbol-key (car symbol-pair))
               (symbol-value (cdr symbol-pair)))
          (hash-set! map reg (cons (symbol->string symbol-key)
                                   (if (pair? symbol-value)
                                     (car symbol-value)
                                     symbol-value)))
          (loop (cdr symbols) (+ reg 1)))))))

(define (create-got-section got-size)
  (make-bytevector got-size 0))

(define (custom-assert condition message)
  (unless condition
    (error message)))

(define (create-init-section)
  (let ((init-section (make-bytevector 16 0)))
    ;; Simple .init section (x86_64 assembly)
    (bytevector-u8-set! init-section 0 #x55)  ; push rbp
    (bytevector-u8-set! init-section 1 #x48)
    (bytevector-u8-set! init-section 2 #x89)
    (bytevector-u8-set! init-section 3 #xe5)  ; mov rbp, rsp
    (bytevector-u8-set! init-section 4 #x5d)  ; pop rbp
    (bytevector-u8-set! init-section 5 #xc3)  ; ret
    init-section))

(define (print-relocation-table table)
  (let ((size (bytevector-length table)))
    (do ((i 0 (+ i 8)))
      ((>= i size))
      (let ((value (bytevector-u64-ref table i (endianness little))))
        (format #t "~8,'0x: ~16,'0x~%" i value)))))

(define (get-tag-name tag)
  (case tag
    ((1) "DT_NEEDED")
    ((5) "DT_STRTAB")
    ((6) "DT_SYMTAB")
    ((7) "DT_RELA")
    ((8) "DT_RELASZ")
    ((9) "DT_RELAENT")
    ((10) "DT_STRSZ")
    ((11) "DT_SYMENT")
    ((0) "DT_NULL")
    (else (format #f "Unknown tag: ~a" tag))))

(define (verify-dynamic-section dynamic-section dynstr-offset dynsym-offset strtab-size dynsym-size rela-offset rela-size)
  (let loop ((offset 0))
    (when (< offset (bytevector-length dynamic-section))
      (let* ((tag (bytevector-u64-ref dynamic-section offset (endianness little)))
             (value (bytevector-u64-ref dynamic-section (+ offset 8) (endianness little))))
        (case tag
          ((5) (custom-assert (= value dynstr-offset) "DT_STRTAB value mismatch"))
          ((6) (custom-assert (= value dynsym-offset) "DT_SYMTAB value mismatch"))
          ((7) (custom-assert (= value rela-offset) "DT_RELA value mismatch"))
          ((8) (custom-assert (= value rela-size) "DT_RELASZ value mismatch"))
          ((9) (custom-assert (= value 24) "DT_RELAENT value mismatch"))
          ((10) (custom-assert (= value strtab-size) "DT_STRSZ value mismatch"))
          ((11) (custom-assert (= value 24) "DT_SYMENT value mismatch")))
        (if (not (= tag 0))
          (loop (+ offset 16)))))))

(define (check-section-overlaps sections)
  (let loop ((remaining sections))
    (when (> (length remaining) 1)
      (let* ((current (car remaining))
             (others (cdr remaining)))
        (for-each
          (lambda (other)
            (custom-assert (or (<= (+ (car current) (cadr current)) (car other))
                               (>= (car current) (+ (car other) (cadr other))))
                           (format #f "Section overlap: ~a and ~a" (caddr current) (caddr other))))
          others)
        (loop (cdr remaining))))))

(define (verify-segment-contents data-segment-start data-segment-end dynstr-offset dynsym-offset rela-offset relocation-table-size)
  (custom-assert (<= data-segment-start dynstr-offset data-segment-end) ".dynstr not in LOAD segment")
  (custom-assert (<= data-segment-start dynsym-offset data-segment-end) ".dynsym not in LOAD segment")
  (custom-assert (<= data-segment-start rela-offset data-segment-end) ".rela.dyn not in LOAD segment")
  (custom-assert (<= (+ rela-offset relocation-table-size) data-segment-end) ".rela.dyn exceeds LOAD segment"))

(define (create-gnu-version-r-section)
  (let ((bv (make-bytevector 32 0)))  ; Adjusted size for full entry, including auxiliary data
    ;; Entry Header
    (bytevector-u16-set! bv 0 1 (endianness little))  ; Version (16-bit)
    (bytevector-u16-set! bv 2 1 (endianness little))  ; Count (16-bit)
    (bytevector-u32-set! bv 4 #x00000498 (endianness little))  ; File offset (32-bit)
    (bytevector-u32-set! bv 8 #x00000000 (endianness little))  ; Hash (32-bit)
    (bytevector-u32-set! bv 12 #x00000498 (endianness little)) ; Auxiliary offset (32-bit)
    (bytevector-u32-set! bv 16 #x00000000 (endianness little)) ; Next offset (32-bit)

    ;; Auxiliary Data (following entry header)
    (bytevector-u32-set! bv 20 #x00000498 (endianness little)) ; Offset to the name string in .dynstr (32-bit)
    (bytevector-u16-set! bv 24 2 (endianness little))          ; Version (16-bit)
    (bytevector-u16-set! bv 26 0 (endianness little))          ; Flags (16-bit)

    ;; The rest of the auxiliary data would typically include other information, 
    ;; such as dependencies on other versions if any (but is omitted here for simplicity).

    bv))
;;
;;(define (create-gnu-version-d-section)
;;  (let ((bv (make-bytevector 28 0)))
;;    (bytevector-u16-set! bv 0 1 (endianness little))  ; Version (1)
;;    (bytevector-u16-set! bv 2 1 (endianness little))  ; Flags (1 for BASE)
;;    (bytevector-u16-set! bv 4 1 (endianness little))  ; Version index (1)
;;    (bytevector-u16-set! bv 6 1 (endianness little))  ; Cnt (1)
;;    (bytevector-u32-set! bv 8 #xb027790a (endianness big))  ; Hash
;;    (bytevector-u32-set! bv 12 20 (endianness little))  ; Offset to verdaux
;;    (bytevector-u32-set! bv 16 0 (endianness little))  ; Offset to next (0)
;;
;;    ; Verdaux entry
;;    (bytevector-u32-set! bv 20 1 (endianness little))  ; Name offset in .dynstr
;;    (bytevector-u32-set! bv 24 0 (endianness little))  ; Offset to next aux (0)
;;    bv))
;;
;;(define (create-gnu-version-section dynsym-count)
;;  (let ((version-section (make-bytevector (* 2 dynsym-count) 0)))
;;    (do ((i 0 (+ i 1)))
;;      ((= i dynsym-count) version-section)
;;      (bytevector-u16-set! version-section (* i 2) 
;;                           (if (= i 0) 0 1)  ; 0 for local, 1 for global
;;                           (endianness little)))))
;;
(define (create-shared-object code data-sections output-file symbol-addresses label-positions assembled-relocation-table)
  (let* ((layout (calculate-elf-layout code data-sections symbol-addresses label-positions))
         (program-headers-offset (assoc-ref layout 'program-headers-offset))
         (code-size (assoc-ref layout 'code-size))
         (data-size (assoc-ref layout 'data-size))
         (symtab-size (assoc-ref layout 'symtab-size))
         (strtab-size (assoc-ref layout 'strtab-size))
         (shstrtab-size (assoc-ref layout 'shstrtab-size))
         (dynamic-symbol-table-size (assoc-ref layout 'dynamic-symbol-table-size))
         (relocation-table-size (assoc-ref layout 'relocation-table-size))
         (got-size (assoc-ref layout 'got-size))
         (data-offset (assoc-ref layout 'data-offset))
         (rodata-size (assoc-ref layout 'rodata-size))
         (plt-size (assoc-ref layout 'plt-size))
         (init-offset (assoc-ref layout 'init-offset))
         (init-size (assoc-ref layout 'init-size))
         (section-headers-offset (assoc-ref layout 'section-headers-offset))
         (shstrtab-addr (assoc-ref layout 'shstrtab-addr))
         (dynamic-addr #x3000)
         (bss-size (assoc-ref layout 'bss-size))
         (symtab-offset (assoc-ref layout 'symtab-offset))
         (strtab-offset (assoc-ref layout 'strtab-offset))
         (symtab-hash (assoc-ref layout 'symtab-hash))
         (dynsym-indices (assoc-ref layout 'dynsym-indices))
         (symtab-bv (assoc-ref layout 'symtab-bv))
         (strtab (assoc-ref layout 'strtab))
         (dynsymtab-bv (assoc-ref layout 'dynsymtab-bv))
         (dynstrtab (assoc-ref layout 'dynstrtab))
         (dynsym-size (assoc-ref layout 'dynsym-size))
         (dynstr-size (assoc-ref layout 'dynstr-size))
         (init-section (create-init-section))
         (rodata-offset #x2000)
         (note-gnu-build-id-address #x1c8)
         (note-gnu-build-id-size #x24)
         (hash-offset (align-to (+ note-gnu-build-id-address note-gnu-build-id-size) word-size))
         (dynamic-offset (align-to dynamic-addr word-size))
         (dynamic-size (assoc-ref layout 'dynamic-size))
         ;; (version-section (assoc-ref layout 'version-section))
         (hash-table (create-hash-section dynsymtab-bv dynstrtab))
         (hash-size (bytevector-length hash-table))
         (dynsym-offset (align-to (+ hash-offset hash-size) word-size))
         (dynstr-offset (align-to (+ dynsym-offset dynsym-size) word-size))
         (rela-offset (align-to (+ dynstr-offset dynstr-size) word-size))
         (relocation-table (create-relocation-table symtab-hash))
         (relocation-table-size (bytevector-length relocation-table))
         (code-offset (assoc-ref layout 'code-offset))
         (got-offset (align-to (+ dynamic-offset dynamic-size) word-size))
         (got-plt-offset (align-to (+ got-offset got-size) word-size))
         (got-plt-size (* (+ (length (hash-map->list cons label-positions)) 3) 8))  ; 3 reserved entries + function entries
         (got-size (assoc-ref layout 'got-size))
         (init-offset (+ text-addr code-size))
         (init-size 16)
         (fini-size 16)
         (fini-offset (align-to (+ init-offset init-size) 16))
         (plt-offset (align-to (+ fini-offset fini-size) 16))
         (plt-section (create-plt-section label-positions))
         (plt-size (bytevector-length plt-section))
         (plt-got-offset (align-to (+ plt-offset plt-size) word-size))
         (plt-got-size (* (length (hash-map->list cons label-positions)) 8))
         (data-addr (align-to (+ got-plt-offset got-plt-size) word-size))
         (rela-plt-offset (align-to (+ rela-offset relocation-table-size) word-size))
         (rela-addr (+ dynamic-addr (- rela-offset dynamic-offset)))

         (rela-plt-section (create-rela-plt-section 
                             (hash-map->list cons label-positions)
                             got-plt-offset
                             dynsym-indices))
         (rela-plt-size (bytevector-length rela-plt-section))
         (gnu-version-offset (align-to (+ hash-offset hash-size) 4))
         (gnu-version-r-size 32)  ; Since we're creating an empty .gnu.version_r section
         (gnu-version-size (* 2 num-dynamic-entries))
         (gnu-version-r-offset (align-to (+ gnu-version-offset gnu-version-size) word-size))
         (gnu-version-d-size 32)
         (gnu-version-size (* 2 num-dynamic-entries))
         (gnu-version-d-offset (align-to (+ gnu-version-offset gnu-version-size) word-size))

         (data-section (create-data-section data-sections))

         (got-plt-section (create-got-plt-section 
                            (hash-map->list cons label-positions)
                            dynamic-addr
                            plt-offset))

         (plt-got-section (create-plt-got-section 
                            (hash-map->list cons label-positions)
                            got-plt-offset))

         (total-dynamic-size (- (+ got-plt-offset got-plt-size) dynamic-offset))
         (data-segment-size (+ data-size total-dynamic-size))
         (shstrtab (create-section-header-string-table))
         (dynamic-section (create-dynamic-section
                            dynstr-offset
                            dynsym-offset
                            dynstr-size
                            dynsym-size
                            rela-offset
                            relocation-table-size
                            got-offset
                            hash-offset
                            gnu-version-offset
                            gnu-version-r-offset
                            gnu-version-r-size
                            plt-offset
                            plt-size
                            rela-plt-offset
                            rela-plt-size
                            got-plt-offset))
         (section-headers (create-section-headers
                            text-addr
                            code-size
                            data-size
                            symtab-size
                            strtab-size
                            shstrtab-size
                            dynsym-size
                            dynstr-size
                            relocation-table-size
                            total-dynamic-size
                            dynamic-size
                            rela-offset
                            got-size
                            data-addr
                            dynamic-addr
                            dynsym-offset
                            dynstr-offset
                            rela-addr
                            (+ dynamic-addr (- got-offset dynamic-offset))
                            (+ dynamic-addr (- plt-offset dynamic-offset))
                            symtab-offset
                            strtab-offset
                            shstrtab-addr
                            plt-size
                            (+ dynamic-addr (- plt-got-offset dynamic-offset))
                            plt-got-size
                            rela-plt-offset
                            rela-plt-size
                            (+ dynamic-addr (- got-plt-offset dynamic-offset))
                            got-plt-size
                            rodata-offset
                            (+ dynamic-addr (- gnu-version-offset dynamic-offset))
                            (+ dynamic-addr (- gnu-version-r-offset dynamic-offset))
                            (* 2 (/ dynsym-size 24))  ; gnu-version-size
                            gnu-version-r-size
                            gnu-version-d-offset
                            gnu-version-d-size
                            hash-offset
                            hash-size
                            code-offset
                            init-offset
                            init-size
                            note-gnu-build-id-address
                            note-gnu-build-id-size
                            #x2000  ; Hardcoded .eh_frame address
                            #x100   ; Hardcoded .eh_frame size
                            ))
         (program-headers (create-program-headers 
                            elf-header-size
                            program-header-size
                            num-program-headers
                            text-addr
                            code-size
                            rodata-size
                            bss-size
                            data-segment-size
                            dynamic-addr
                            dynamic-offset
                            dynamic-size
                            total-dynamic-size
                            got-offset
                            got-size
                            plt-offset
                            plt-size
                            (+ data-addr data-segment-size)
                            alignment))
         (program-headers-size (bytevector-length program-headers))
         (num-program-headers (/ program-headers-size program-header-size))
         (section-headers-size (* num-sections section-header-size))
         (total-size (+ section-headers-offset section-headers-size))
         (text-section-offset (calculate-text-section-offset elf-header-size program-headers-size))
         (entry-point (calculate-entry-point text-addr text-section-offset))
         (build-id "ff868e73a156d910e45c2590bb78b1224345b530")
         (note-gnu-build-id-section (create-note-gnu-build-id-section build-id))
         (elf-header (create-elf-header
                       entry-point
                       program-headers-offset
                       program-headers-size
                       section-headers-offset
                       num-program-headers
                       num-sections
                       total-size
                       shstrtab-index
                       hash-offset
                       hash-size)))

    (format #t "Creating shared object...~%")
    (format #t "Total size: ~a~%" total-size)
    (format #t "Code size: ~a~%" code-size)
    (format #t "Code offset: ~a~%" (assoc-ref layout 'code-offset))

    (let ((elf-file (make-bytevector total-size 0)))
      ;; Copy ELF header (assumed to start at the beginning of the file)
      (bytevector-copy! elf-header 0 elf-file 0 (bytevector-length elf-header))

      ;; Copy program headers
      (bytevector-copy! program-headers 0 elf-file program-headers-offset program-headers-size)

      ;; Copy the .note.gnu.build-id section into the ELF file
      (bytevector-copy! note-gnu-build-id-section 0 elf-file note-gnu-build-id-address note-gnu-build-id-size)

      ;; Copy hash table (.hash)
      (bytevector-copy! hash-table 0 elf-file hash-offset hash-size)

      ;; Resolve and copy code section
      (let* ((resolved-code (link-code code symtab-hash label-positions assembled-relocation-table data-addr))
             (code-offset (assoc-ref layout 'code-offset)))
        (bytevector-copy! resolved-code 0 elf-file code-offset (bytevector-length resolved-code)))

      ;; Copy dynamic section
      (bytevector-copy! dynamic-section 0 elf-file dynamic-offset dynamic-size)

      ;; Copy dynamic symbol table (.dynsym)
      (bytevector-copy! dynsymtab-bv 0 elf-file dynsym-offset dynsym-size)

      ;; Copy dynamic string table (.dynstr)
      (bytevector-copy! dynstrtab 0 elf-file dynstr-offset dynstr-size)

      ;; Copy initialization section (.init)
      (bytevector-copy! init-section 0 elf-file init-offset init-size)

      ;; Copy relocation table (.rela.dyn)
      (bytevector-copy! relocation-table 0 elf-file rela-offset relocation-table-size)

      ;; Copy data section (.data)
      (bytevector-copy! data-section 0 elf-file data-addr (bytevector-length data-section))

      ;; Copy symbol table (.symtab)
      (bytevector-copy! symtab-bv 0 elf-file symtab-offset symtab-size)

      ;; Copy string table (.strtab)
      (bytevector-copy! strtab 0 elf-file strtab-offset strtab-size)

      ;;;; Create and copy .gnu.version section
      ;;(let* ((dynsym-count (/ (bytevector-length symtab-bv) 24))
      ;;       (gnu-version-output (create-gnu-version-section dynsym-count))
      ;;       (gnu-version-size (* 2 dynsym-count)))
      ;;  (bytevector-copy! gnu-version-output 0 elf-file gnu-version-offset gnu-version-size))

      ;;;; Create and copy .gnu.version_d section
      ;;(let ((gnu-version-d (create-gnu-version-d-section)))
      ;;  (bytevector-copy! gnu-version-d 0 elf-file gnu-version-d-offset (bytevector-length gnu-version-d)))

      ;; Copy Global Offset Table (.got)
      (let ((got-section (create-got-section got-size)))
        (bytevector-copy! got-section 0 elf-file got-offset got-size))

      ;; Copy Procedure Linkage Table (.plt)
      (bytevector-copy! plt-section 0 elf-file plt-offset plt-size)

      ;; Copy .plt.got section
      (bytevector-copy! plt-got-section 0 elf-file plt-got-offset plt-got-size)

      ;; Copy relocation entries for PLT (.rela.plt)
      (bytevector-copy! rela-plt-section 0 elf-file rela-plt-offset rela-plt-size)

      ;; Copy Global Offset Table for PLT (.got.plt)
      (bytevector-copy! got-plt-section 0 elf-file got-plt-offset got-plt-size)

      ;; Copy section header string table (.shstrtab)
      (bytevector-copy! shstrtab 0 elf-file (- section-headers-offset shstrtab-size) shstrtab-size)

      ;; Copy section headers
      (bytevector-copy! section-headers 0 elf-file section-headers-offset section-headers-size)

      ;; Write the ELF file
      (call-with-output-file output-file
                             (lambda (port)
                               (put-bytevector port elf-file)))

      total-size)))
