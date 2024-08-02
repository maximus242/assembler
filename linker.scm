(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs io ports) ; For put-bytevector
  #:export (link-code create-shared-object))

; Add this definition near the top of the file, after the module definition
(define (bytevector-append . bvs)
  (let* ((total-length (apply + (map bytevector-length bvs)))
         (result (make-bytevector total-length)))
    (let loop ((offset 0)
               (bvs bvs))
      (if (null? bvs)
          result
          (let ((bv (car bvs)))
            (bytevector-copy! bv 0 result offset (bytevector-length bv))
            (loop (+ offset (bytevector-length bv)) (cdr bvs)))))))

(define (create-program-header type offset vaddr paddr filesz memsz flags align)
  (let ((header (make-bytevector 56 0)))
    (bytevector-u32-set! header 0 type (endianness little))
    (bytevector-u32-set! header 4 flags (endianness little))
    (bytevector-u64-set! header 8 offset (endianness little))
    (bytevector-u64-set! header 16 vaddr (endianness little))
    (bytevector-u64-set! header 24 paddr (endianness little))
    (bytevector-u64-set! header 32 filesz (endianness little))
    (bytevector-u64-set! header 40 memsz (endianness little))
    (bytevector-u64-set! header 48 align (endianness little))
    header))

(define (create-program-headers code-size data-size)
  (let* ((text-segment (create-program-header 1 #x1000 #x1000 #x1000 code-size code-size 5 #x1000))
         (data-segment (create-program-header 1 (+ #x1000 code-size) (+ #x1000 code-size) (+ #x1000 code-size) data-size data-size 6 #x1000)))
    (bytevector-append text-segment data-segment)))

(define (make-symbol-table)
  (make-hash-table))

(define (add-symbol! table name address)
  (hash-set! table name address))

(define (get-symbol table name)
  (hash-ref table name))

(define (alist-ref key alist)
  (let ((pair (assoc key alist)))
    (and pair (cdr pair))))

(define base-address-difference #x1000) ; 0x401000 - 0x400000

(define (adjust-address address)
  (+ address base-address-difference))

(define (resolve-references code symbol-table label-positions reg-to-symbol-map)
  (let* ((code-length (bytevector-length code))
         (resolved-code (bytevector-copy code))
         (code-base-address 0)
         (program-base-address 0))
    (format #t "Starting to resolve references. Code length: ~a~%" code-length)
    (format #t "Symbol table: ~a~%" symbol-table)
    (format #t "Label positions: ~a~%" label-positions)
    (let loop ((offset 0))
      (if (< offset code-length)
          (let ((instruction (bytevector-u8-ref code offset)))
            (format #t "Offset: ~a, Instruction: ~x~%" offset instruction)
            (cond
              ; Handle MOV immediate (could be a symbol or label reference)
              ((and (= instruction #x48)
                    (< (+ offset 6) code-length)
                    (= (bytevector-u8-ref code (+ offset 1)) #xC7))
               (let* ((reg (logand (bytevector-u8-ref code (+ offset 2)) #x07))
                      (imm-offset (+ offset 3)))
                 (if (< (+ imm-offset 4) code-length)
                     (let ((imm (bytevector-u32-ref code imm-offset (endianness little))))
                       (format #t "  MOV imm32: reg=~a, imm=~x~%" reg imm)
                       (when (= imm 0) ; Possible symbolic reference or label
                         (let* ((symbol-name (alist-ref reg reg-to-symbol-map))
                                (symbol-address (and symbol-name (or (alist-ref symbol-name symbol-table)
                                                                     (hash-ref label-positions symbol-name)))))
                           (format #t "  Resolving symbol/label: ~a -> ~a~%" 
                                   (or symbol-name "#f") 
                                   (if symbol-address 
                                       (format #f "~x" symbol-address)
                                       "#f"))
                           (when symbol-address
                             (bytevector-u32-set! resolved-code imm-offset 
                                                  (if (hash-ref label-positions symbol-name)
                                                      (+ symbol-address code-base-address)
                                                      (adjust-address symbol-address))
                                                  (endianness little)))))
                       (format #t "  After MOV resolution: ~x~%" 
                               (bytevector-u32-ref resolved-code imm-offset (endianness little)))
                       (loop (+ offset 7)))
                     (format #t "Warning: Insufficient bytes for MOV immediate at offset ~a~%" offset))))
              
              ; Handle VMOVAPS
              ((and (= instruction #xC5)
                    (< (+ offset 7) code-length)
                    (= (bytevector-u8-ref code (+ offset 1)) #xFC)
                    (= (bytevector-u8-ref code (+ offset 2)) #x28)
                    (= (bytevector-u8-ref code (+ offset 3)) #x05))
               (let* ((imm-offset (+ offset 4))
                      (imm (bytevector-u32-ref code imm-offset (endianness little))))
                 (format #t "  VMOVAPS: original displacement=~x~%" imm)
                 (let ((symbol-address (alist-ref 'multiplier symbol-table)))
                   (when symbol-address
                     (let* ((instruction-end (+ offset 8))
                            (next-instruction-address (+ instruction-end code-base-address))
                            (target-offset (- (adjust-address symbol-address) next-instruction-address)))
                       (format #t "    Resolving multiplier -> ~x~%" symbol-address)
                       (format #t "    Instruction end: ~x~%" instruction-end)
                       (format #t "    Next instruction address: ~x~%" next-instruction-address)
                       (format #t "    Target offset: ~x~%" target-offset)
                       (bytevector-u32-set! resolved-code imm-offset target-offset (endianness little)))))
                 (format #t "  After VMOVAPS resolution: ~x~%" 
                         (bytevector-u32-ref resolved-code imm-offset (endianness little)))
                 (loop (+ offset 8))))
              
              ; Handle potential label references in other instructions (e.g., jumps, calls)
              ((and (or (= instruction #xE8) ; CALL
                        (= instruction #xE9)) ; JMP
                    (< (+ offset 4) code-length))
               (let* ((imm-offset (+ offset 1))
                      (imm (bytevector-s32-ref code imm-offset (endianness little))))
                 (format #t "  CALL/JMP: original offset=~x~%" imm)
                 (when (= imm 0) ; Possible label reference
                   (let* ((label-name (alist-ref offset reg-to-symbol-map))
                          (label-position (and label-name (hash-ref label-positions label-name))))
                     (when label-position
                       (let* ((instruction-end (+ offset 5))
                              (next-instruction-address (+ instruction-end code-base-address))
                              (target-offset (- (+ label-position code-base-address) next-instruction-address)))
                         (format #t "    Resolving label ~a -> ~x~%" label-name label-position)
                         (format #t "    Instruction end: ~x~%" instruction-end)
                         (format #t "    Next instruction address: ~x~%" next-instruction-address)
                         (format #t "    Target offset: ~x~%" target-offset)
                         (bytevector-s32-set! resolved-code imm-offset target-offset (endianness little))))))
                 (format #t "  After CALL/JMP resolution: ~x~%" 
                         (bytevector-s32-ref resolved-code imm-offset (endianness little)))
                 (loop (+ offset 5))))
              
              (else (loop (+ offset 1)))))
          (begin
            (format #t "Finished resolving references.~%")
            resolved-code)))))

(define (link-code code symbol-addresses label-positions)
  (let ((linked-code (bytevector-copy code))
        (code-offset 0))
    (for-each
     (lambda (inst)
       (when (eq? (car inst) 'lea)
         (let* ((label (caddr (cadr inst)))
                (target-address (cdr (assoc label symbol-addresses)))
                (instruction-end (+ code-offset 7))
                (next-instruction-address (+ code-offset 7))
                (displacement (- target-address next-instruction-address)))
           (bytevector-u32-set! linked-code (+ code-offset 3) displacement (native-endianness)))))
     (disassemble code))
    linked-code))

; Helper function to implement 'take' functionality
(define (take lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

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

(define (write-bytevector bv port)
  (put-bytevector port bv))

(define (create-section-headers code-size data-size symtab-size strtab-size shstrtab-size)
  (let* ((num-sections 6)  ; .text, .data, .symtab, .strtab, .shstrtab, and a null section
         (section-header-size 64)
         (headers (make-bytevector (* num-sections section-header-size) 0)))
    ;; Null section
    (bytevector-u32-set! headers 0 0 (endianness little))
    
    ;; .text section
    (bytevector-u32-set! headers 64 1 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 68 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 72 6 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_EXECINSTR)
    (bytevector-u64-set! headers 80 #x1000 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 88 #x1000 (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 96 code-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 104 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 108 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 112 16 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 120 0 (endianness little))  ; sh_entsize
    
    ;; .data section
    (bytevector-u32-set! headers 128 7 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 132 1 (endianness little))  ; sh_type (SHT_PROGBITS)
    (bytevector-u64-set! headers 136 3 (endianness little))  ; sh_flags (SHF_ALLOC | SHF_WRITE)
    (bytevector-u64-set! headers 144 (+ #x1000 code-size) (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 152 (+ #x1000 code-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 160 data-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 168 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 172 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 176 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 184 0 (endianness little))  ; sh_entsize
    
    ;; .symtab section
    (bytevector-u32-set! headers 192 13 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 196 2 (endianness little))  ; sh_type (SHT_SYMTAB)
    (bytevector-u64-set! headers 200 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 208 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 216 (+ #x1000 code-size data-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 224 symtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 232 4 (endianness little))  ; sh_link (index of .strtab)
    (bytevector-u32-set! headers 236 5 (endianness little))  ; sh_info (one greater than the symbol table index of the last local symbol)
    (bytevector-u64-set! headers 240 8 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 248 24 (endianness little))  ; sh_entsize (size of a symbol table entry)
    
    ;; .strtab section
    (bytevector-u32-set! headers 256 21 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 260 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 264 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 272 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 280 (+ #x1000 code-size data-size symtab-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 288 strtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 296 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 300 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 304 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 312 0 (endianness little))  ; sh_entsize
    
    ;; .shstrtab section
    (bytevector-u32-set! headers 320 29 (endianness little))  ; sh_name
    (bytevector-u32-set! headers 324 3 (endianness little))  ; sh_type (SHT_STRTAB)
    (bytevector-u64-set! headers 328 0 (endianness little))  ; sh_flags
    (bytevector-u64-set! headers 336 0 (endianness little))  ; sh_addr
    (bytevector-u64-set! headers 344 (+ #x1000 code-size data-size symtab-size strtab-size) (endianness little))  ; sh_offset
    (bytevector-u64-set! headers 352 shstrtab-size (endianness little))  ; sh_size
    (bytevector-u32-set! headers 360 0 (endianness little))  ; sh_link
    (bytevector-u32-set! headers 364 0 (endianness little))  ; sh_info
    (bytevector-u64-set! headers 368 1 (endianness little))  ; sh_addralign
    (bytevector-u64-set! headers 376 0 (endianness little))  ; sh_entsize
    
    headers))

(define (create-section-header-string-table)
  (string->utf8 "\0.text\0.data\0.symtab\0.strtab\0.shstrtab\0"))

(define (create-shared-object code data-sections output-file symbol-addresses label-positions)
  (let* ((entry-point #x1000)
         (code-size (bytevector-length code))
         (data-size (apply + (map (lambda (pair) (bytevector-length (cdr pair))) data-sections)))
         (symtab (create-symbol-table symbol-addresses))
         (symtab-size (bytevector-length symtab))
         (strtab (create-string-table symbol-addresses))
         (strtab-size (bytevector-length strtab))
         (shstrtab (create-section-header-string-table))
         (shstrtab-size (bytevector-length shstrtab))
         (section-headers-offset (align-to (+ #x1000 code-size data-size symtab-size strtab-size shstrtab-size) 8))
         (num-sections 6)  ; .text, .data, .symtab, .strtab, .shstrtab, and a null section
         (section-headers (create-section-headers code-size data-size symtab-size strtab-size shstrtab-size))
         (elf-header (create-elf-header entry-point section-headers-offset num-sections))
         (program-headers (create-program-headers code-size data-size))
         (total-size (+ section-headers-offset (bytevector-length section-headers))))

    (call-with-output-file output-file
      (lambda (port)
        (write-bytevector elf-header port)
        (write-bytevector program-headers port)
        (write-bytevector (make-bytevector (max 0 (- #x1000 (+ (bytevector-length elf-header) (bytevector-length program-headers)))) 0) port)
        (write-bytevector code port)
        (write-bytevector (make-bytevector (align-to code-size 8) 0) port)
        (for-each (lambda (pair) (write-bytevector (cdr pair) port)) data-sections)
        (write-bytevector (make-bytevector (align-to data-size 8) 0) port)
        (write-bytevector symtab port)
        (write-bytevector (make-bytevector (align-to symtab-size 8) 0) port)
        (write-bytevector strtab port)
        (write-bytevector (make-bytevector (align-to strtab-size 8) 0) port)
        (write-bytevector shstrtab port)
        (let ((padding-size (max 0 (- section-headers-offset (+ #x1000 (align-to code-size 8) (align-to data-size 8) (align-to symtab-size 8) (align-to strtab-size 8) shstrtab-size)))))
          (when (> padding-size 0)
            (write-bytevector (make-bytevector padding-size 0) port)))
        (write-bytevector section-headers port)))

    (format #t "Shared object created: ~a~%" output-file)
    (format #t "Total file size: ~a bytes~%" total-size)))

(define (align-to value alignment)
  (* (ceiling (/ value alignment)) alignment))

;; Helper functions to create various parts of the ELF file
(define (create-symbol-table symbol-addresses)
  (let* ((symbol-count (length symbol-addresses))
         (table-size (* symbol-count 24))  ; Each symbol entry is 24 bytes
         (table (make-bytevector table-size 0))
         (string-table-offset 1))  ; Start at 1 to account for null byte at beginning of string table
    (format #t "Creating symbol table with ~a symbols~%" symbol-count)
    (let loop ((symbols symbol-addresses)
               (index 0)
               (str-offset 1))
      (if (null? symbols)
          (begin
            (format #t "Symbol table created. Size: ~a bytes~%" (bytevector-length table))
            table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (address (cdr symbol)))
            (bytevector-u32-set! table (* index 24) str-offset (endianness little))  ; st_name
            (bytevector-u8-set! table (+ (* index 24) 4) 1)  ; st_info (1 = STT_OBJECT)
            (bytevector-u8-set! table (+ (* index 24) 5) 0)  ; st_other
            (bytevector-u16-set! table (+ (* index 24) 6) 1 (endianness little))  ; st_shndx (1 = .text section)
            (bytevector-u64-set! table (+ (* index 24) 8) address (endianness little))  ; st_value
            (bytevector-u64-set! table (+ (* index 24) 16) 0 (endianness little))  ; st_size
            (format #t "Added symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                    name address str-offset)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (string-length name) 1)))))))

(define (create-string-table symbol-addresses)
  (let* ((names (map (lambda (pair) (symbol->string (car pair))) symbol-addresses))
         (total-length (+ 1 (apply + (map (lambda (name) (+ (string-length name) 1)) names))))
         (table (make-bytevector total-length 0)))
    (let loop ((names names)
               (offset 1))
      (if (null? names)
          table
          (let ((name (car names)))
            (bytevector-copy! (string->utf8 name) 0 table offset (string-length name))
            (loop (cdr names) (+ offset (string-length name) 1)))))))

(define (create-relocation-table symbol-addresses)
  (let* ((reloc-count (length symbol-addresses))
         (table-size (* reloc-count 24))  ; Each Elf64_Rela entry is 24 bytes
         (table (make-bytevector table-size 0)))
    (let loop ((symbols symbol-addresses)
               (index 0))
      (if (null? symbols)
          table
          (let* ((symbol (car symbols))
                 (name (car symbol))
                 (address (cdr symbol)))
            (bytevector-u64-set! table (* index 24) address (endianness little))  ; r_offset
            (bytevector-u64-set! table (+ (* index 24) 8)
                                 (logior (ash index 32) 1) (endianness little))  ; r_info (1 = R_X86_64_64)
            (bytevector-u64-set! table (+ (* index 24) 16) 0 (endianness little))  ; r_addend
            (loop (cdr symbols) (+ index 1)))))))

(define (create-dynamic-symbol-table symbol-addresses)
  (let* ((symbol-count (length symbol-addresses))
         (table-size (* symbol-count 24))  ; Each symbol entry is 24 bytes
         (table (make-bytevector table-size 0))
         (string-table-offset 1))  ; Start at 1 to account for null byte at beginning of string table
    (format #t "Creating dynamic symbol table with ~a symbols~%" symbol-count)
    (let loop ((symbols symbol-addresses)
               (index 0)
               (str-offset 1))
      (if (null? symbols)
          (begin
            (format #t "Dynamic symbol table created. Size: ~a bytes~%" (bytevector-length table))
            table)
          (let* ((symbol (car symbols))
                 (name (symbol->string (car symbol)))
                 (address (cdr symbol)))
            (bytevector-u32-set! table (* index 24) str-offset (endianness little))  ; st_name
            (bytevector-u8-set! table (+ (* index 24) 4) 1)  ; st_info (1 = STT_OBJECT)
            (bytevector-u8-set! table (+ (* index 24) 5) 0)  ; st_other
            (bytevector-u16-set! table (+ (* index 24) 6) 0 (endianness little))  ; st_shndx
            (bytevector-u64-set! table (+ (* index 24) 8) address (endianness little))  ; st_value
            (bytevector-u64-set! table (+ (* index 24) 16) 0 (endianness little))  ; st_size
            (format #t "Added dynamic symbol: ~a, address: 0x~x, offset in string table: ~a~%" 
                    name address str-offset)
            (loop (cdr symbols)
                  (+ index 1)
                  (+ str-offset (string-length name) 1)))))))

(define (create-dynamic-section symbol-table-size dynamic-symbol-table-size relocation-table-size)
  (let* ((entry-count 7)
         (table-size (* entry-count 16))
         (table (make-bytevector table-size 0)))
    (bytevector-u64-set! table 0 5 (endianness little))  ; DT_STRTAB
    (bytevector-u64-set! table 8 0 (endianness little))  ; Address of .dynstr (to be filled later)
    (bytevector-u64-set! table 16 6 (endianness little))  ; DT_SYMTAB
    (bytevector-u64-set! table 24 0 (endianness little))  ; Address of .dynsym (to be filled later)
    (bytevector-u64-set! table 32 10 (endianness little))  ; DT_STRSZ
    (bytevector-u64-set! table 40 0 (endianness little))  ; Size of .dynstr (to be filled later)
    (bytevector-u64-set! table 48 11 (endianness little))  ; DT_SYMENT
    (bytevector-u64-set! table 56 24 (endianness little))  ; Size of one symbol table entry
    (bytevector-u64-set! table 64 0 (endianness little))  ; DT_NULL
    (bytevector-u64-set! table 72 0 (endianness little))
    table))

(define (display-symbol-table symbol-table string-table)
  (format #t "Symbol Table Contents:~%")
  (format #t "------------------------~%")
  (let ((symbol-count (/ (bytevector-length symbol-table) 24)))
    (do ((i 0 (+ i 1)))
        ((= i symbol-count))
      (let* ((name-offset (bytevector-u32-ref symbol-table (* i 24) (endianness little)))
             (info (bytevector-u8-ref symbol-table (+ (* i 24) 4)))
             (other (bytevector-u8-ref symbol-table (+ (* i 24) 5)))
             (shndx (bytevector-u16-ref symbol-table (+ (* i 24) 6) (endianness little)))
             (value (bytevector-u64-ref symbol-table (+ (* i 24) 8) (endianness little)))
             (size (bytevector-u64-ref symbol-table (+ (* i 24) 16) (endianness little)))
             (name (utf8->string (bytevector-slice string-table name-offset 
                                                   (bytevector-index string-table 0 name-offset)))))
        (format #t "Symbol ~a:~%" i)
        (format #t "  Name: ~a~%" name)
        (format #t "  Value: 0x~x~%" value)
        (format #t "  Size: ~a~%" size)
        (format #t "  Info: 0x~x~%" info)
        (format #t "  Other: 0x~x~%" other)
        (format #t "  Section index: ~a~%~%" shndx)))))

;; Helper function to slice a bytevector
(define (bytevector-slice bv start end)
  (let ((result (make-bytevector (- end start))))
    (bytevector-copy! bv start result 0 (- end start))
    result))

;; Helper function to find the index of a byte in a bytevector
(define (bytevector-index bv byte start)
  (let loop ((i start))
    (cond
     ((= i (bytevector-length bv)) #f)
     ((= (bytevector-u8-ref bv i) byte) i)
     (else (loop (+ i 1))))))
