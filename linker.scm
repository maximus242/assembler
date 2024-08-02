(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (link-code create-shared-library))

(define (create-program-header offset vaddr size)
  (let ((header (make-bytevector 56 0)))
    (bytevector-u32-set! header 0 1 (endianness little))    ; p_type (LOAD)
    (bytevector-u32-set! header 4 5 (endianness little))    ; p_flags (R_X)
    (bytevector-u64-set! header 8 offset (endianness little))   ; p_offset
    (bytevector-u64-set! header 16 vaddr (endianness little))   ; p_vaddr
    (bytevector-u64-set! header 24 vaddr (endianness little))   ; p_paddr
    (bytevector-u64-set! header 32 size (endianness little))    ; p_filesz
    (bytevector-u64-set! header 40 size (endianness little))    ; p_memsz
    (bytevector-u64-set! header 48 4096 (endianness little))  ; p_align
    header))

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

(define (link-code assembled-code symbol-addresses label-positions)
  (let* ((symbols (append (map car symbol-addresses) (hash-map->list cons label-positions)))
         (available-registers '(7 6 2 1 0 3 4 5))  ; Add more if needed
         (reg-to-symbol-map 
          (map cons 
               (take available-registers (min (length symbols) (length available-registers)))
               symbols)))
    (resolve-references assembled-code symbol-addresses label-positions reg-to-symbol-map)))

; Helper function to implement 'take' functionality
(define (take lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (create-elf-header entry-point)
  (let ((header (make-bytevector 64 0)))
    (bytevector-u32-set! header 0 #x464c457f (endianness little))  ; ELF magic number
    (bytevector-u8-set! header 4 2)  ; 64-bit format
    (bytevector-u8-set! header 5 1)  ; Little endian
    (bytevector-u8-set! header 6 1)  ; Current version of ELF
    (bytevector-u8-set! header 7 0)  ; System V ABI
    (bytevector-u16-set! header 16 3 (endianness little))  ; Type: Shared object file
    (bytevector-u16-set! header 18 #x3e (endianness little))  ; Machine: AMD x86-64
    (bytevector-u32-set! header 20 1 (endianness little))  ; Version: Current
    (bytevector-u64-set! header 24 entry-point (endianness little))  ; Entry point address
    (bytevector-u64-set! header 32 64 (endianness little))  ; Program header offset
    (bytevector-u64-set! header 40 0 (endianness little))  ; Section header offset
    (bytevector-u32-set! header 48 0 (endianness little))  ; Flags
    (bytevector-u16-set! header 52 64 (endianness little))  ; Size of this header
    (bytevector-u16-set! header 54 56 (endianness little))  ; Size of program headers
    (bytevector-u16-set! header 56 1 (endianness little))   ; Number of program headers
    (bytevector-u16-set! header 58 0 (endianness little))   ; Size of section headers
    (bytevector-u16-set! header 60 0 (endianness little))   ; Number of section headers
    (bytevector-u16-set! header 62 0 (endianness little))   ; Section header string table index
    header))

(define (create-shared-library code data-sections output-file symbol-addresses label-positions)
  (format #t "Creating shared library. Linked code size: ~a~%" (bytevector-length code))
  (format #t "Data sections: ~a~%" data-sections)
  (format #t "Symbol addresses: ~a~%" symbol-addresses)
  (format #t "Output file: ~a~%" output-file)
  
  (let* ((elf-header-size (bytevector-length (create-elf-header 0)))
         (program-header-size 56)
         (headers-size (+ elf-header-size program-header-size))
         (content-offset (align-to headers-size 4096))
         (vaddr-base #x400000)
         (entry-point (+ vaddr-base content-offset))
         (elf-header (create-elf-header entry-point))
         (code-size (bytevector-length code))
         (data-size (apply + (map (lambda (x) (bytevector-length (cdr x))) data-sections)))
         (total-content-size (+ code-size data-size))
         (program-header (create-program-header 
                          content-offset
                          (+ vaddr-base content-offset)
                          total-content-size))
         (total-size (+ content-offset total-content-size))
         (full-library (make-bytevector total-size 0)))
    
    (format #t "ELF header size: ~a bytes~%" elf-header-size)
    (format #t "Program header size: ~a bytes~%" program-header-size)
    (format #t "Code size: ~a bytes~%" code-size)
    (format #t "Data size: ~a bytes~%" data-size)
    (format #t "Total size: ~a bytes~%" total-size)
    
    ; Debug output for program header
    (format #t "Program header contents:~%")
    (format #t "  p_type: ~a~%" (bytevector-u32-ref program-header 0 (endianness little)))
    (format #t "  p_flags: ~a~%" (bytevector-u32-ref program-header 4 (endianness little)))
    (format #t "  p_offset: ~a~%" (bytevector-u64-ref program-header 8 (endianness little)))
    (format #t "  p_vaddr: ~a~%" (bytevector-u64-ref program-header 16 (endianness little)))
    (format #t "  p_filesz: ~a~%" (bytevector-u64-ref program-header 32 (endianness little)))
    
    ; Copy ELF header
    (bytevector-copy! elf-header 0 full-library 0 elf-header-size)
    
    ; Copy program header
    (bytevector-copy! program-header 0 full-library elf-header-size program-header-size)
    
    ; Copy code
    (bytevector-copy! code 0 full-library content-offset code-size)
    
    ; Copy data sections
    (let loop ((sections data-sections)
               (offset (+ content-offset code-size)))
      (if (null? sections)
          'done
          (let* ((section (car sections))
                 (data (cdr section))
                 (size (bytevector-length data)))
            (bytevector-copy! data 0 full-library offset size)
            (loop (cdr sections) (+ offset size)))))
    
    ; Write the full library to the output file
    (if (string? output-file)
        (begin
          (format #t "Writing shared library to file: ~a~%" output-file)
          (call-with-output-file output-file
            (lambda (port)
              (put-bytevector port full-library)))
          (chmod output-file #o755)) ; Make the file executable
        (format #t "Error: Invalid output file name. Expected a string, got: ~a~%" output-file))

    (let* ((adjusted-symbol-addresses
            (map (lambda (pair)
                   (cons (car pair) (+ (cdr pair) base-address-difference)))
                 symbol-addresses)))
      (format #t "Logging information:~%")
      (for-each (lambda (pair)
                  (format #t "~a address: ~x~%" (car pair) (cdr pair)))
                adjusted-symbol-addresses)
      (format #t "Executable created: ~a~%" output-file))

    full-library)) ; Return the full library bytevector

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
            (bytevector-u16-set! table (+ (* index 24) 6) 0 (endianness little))  ; st_shndx
            (bytevector-u64-set! table (+ (* index 24) 8) address (endianness little))  ; st_value (keep as-is)
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
  (let* ((entry-count 10)  ; Adjust based on actual needed entries
         (table-size (* entry-count 16))
         (table (make-bytevector table-size 0)))
    ;; Fill in dynamic entries (DT_SYMTAB, DT_STRTAB, DT_RELA, etc.)
    ;; ... implementation ...
    table))

(define (align-to value alignment)
  (* (ceiling (/ value alignment)) alignment))

(define (create-section-header-table code-size data-size symbol-table-size string-table-size
                                     relocation-table-size dynamic-symbol-table-size
                                     dynamic-section-size)
  (let* ((header-size 64)
         (num-headers 7)
         (table-size (* header-size num-headers))
         (table (make-bytevector table-size 0))
         (data-offset (+ #x1000 code-size))
         (symtab-offset (+ data-offset data-size))
         (strtab-offset (+ symtab-offset symbol-table-size))
         (rela-offset (+ strtab-offset string-table-size))
         (dynsym-offset (+ rela-offset relocation-table-size))
         (dynamic-offset (+ dynsym-offset dynamic-symbol-table-size)))
    (create-section-header table 0 0 0 0 0 0 0 0 0 0 0)
    (create-section-header table header-size 1 1 6 0 #x1000 code-size 0 0 16 0)
    (create-section-header table (* 2 header-size) 11 2 3 symtab-offset symtab-offset symbol-table-size 3 0 8 24)
    (create-section-header table (* 3 header-size) 19 3 3 strtab-offset strtab-offset string-table-size 0 0 1 0)
    (create-section-header table (* 4 header-size) 4 9 3 rela-offset rela-offset relocation-table-size 3 0 8 24)
    (create-section-header table (* 5 header-size) 11 11 3 dynsym-offset dynsym-offset dynamic-symbol-table-size 3 0 8 24)
    (create-section-header table (* 6 header-size) 6 10 3 dynamic-offset dynamic-offset dynamic-section-size 0 0 8 16)
    table))

(define (create-section-header table offset name type flags addr file-offset size link info addralign entsize)
  (bytevector-u32-set! table (+ offset 0) name (endianness little))
  (bytevector-u32-set! table (+ offset 4) type (endianness little))
  (bytevector-u64-set! table (+ offset 8) flags (endianness little))
  (bytevector-u64-set! table (+ offset 16) addr (endianness little))
  (bytevector-u64-set! table (+ offset 24) file-offset (endianness little))
  (bytevector-u64-set! table (+ offset 32) size (endianness little))
  (bytevector-u32-set! table (+ offset 40) link (endianness little))
  (bytevector-u32-set! table (+ offset 44) info (endianness little))
  (bytevector-u64-set! table (+ offset 48) addralign (endianness little))
  (bytevector-u64-set! table (+ offset 56) entsize (endianness little)))

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
