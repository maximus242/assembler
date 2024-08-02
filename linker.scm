(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (link-code create-executable))

(define (create-program-header offset vaddr size)
  (let ((header (make-bytevector 56 0)))
    (bytevector-u32-set! header 0 1 (endianness little)) ; p_type (LOAD)
    (bytevector-u32-set! header 4 7 (endianness little)) ; p_flags (RWX)
    (bytevector-u64-set! header 8 offset (endianness little)) ; p_offset
    (bytevector-u64-set! header 16 vaddr (endianness little)) ; p_vaddr
    (bytevector-u64-set! header 24 vaddr (endianness little)) ; p_paddr
    (bytevector-u64-set! header 32 size (endianness little)) ; p_filesz
    (bytevector-u64-set! header 40 size (endianness little)) ; p_memsz
    (bytevector-u64-set! header 48 #x1000 (endianness little)) ; p_align
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

(define (resolve-references code symbol-table reg-to-symbol-map)
  (let* ((code-length (bytevector-length code))
         (resolved-code (bytevector-copy code))
         (code-base-address #x401000)
         (program-base-address #x400000))
    (format #t "Starting to resolve references. Code length: ~a~%" code-length)
    (format #t "Symbol table: ~a~%" symbol-table)
    (let loop ((offset 0))
      (if (< offset code-length)
          (let ((instruction (bytevector-u8-ref code offset)))
            (format #t "Offset: ~a, Instruction: ~x~%" offset instruction)
            (cond
              ; Handle MOV immediate
              ((and (= instruction #x48)
                    (< (+ offset 6) code-length)
                    (= (bytevector-u8-ref code (+ offset 1)) #xC7))
               (let* ((reg (logand (bytevector-u8-ref code (+ offset 2)) #x07))
                      (imm-offset (+ offset 3))
                      (imm (bytevector-u32-ref code imm-offset (endianness little))))
                 (format #t "  MOV imm32: reg=~a, imm=~x~%" reg imm)
                 (when (= imm 0) ; Possible symbolic reference
                   (let* ((symbol-name (alist-ref reg reg-to-symbol-map))
                          (symbol-address (and symbol-name (alist-ref symbol-name symbol-table))))
                     (format #t "  Resolving symbol: ~a -> ~a~%" 
                             (or symbol-name "#f") 
                             (if symbol-address 
                                 (format #f "~x" symbol-address)
                                 "#f"))
                     (when symbol-address
                       (bytevector-u32-set! resolved-code imm-offset symbol-address (endianness little)))))
                 (format #t "  After MOV resolution: ~x~%" 
                         (bytevector-u32-ref resolved-code imm-offset (endianness little)))
                 (loop (+ offset 7))))
              
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
                            (target-offset (- symbol-address next-instruction-address)))
                       (format #t "    Resolving multiplier -> ~x~%" symbol-address)
                       (format #t "    Instruction end: ~x~%" instruction-end)
                       (format #t "    Next instruction address: ~x~%" next-instruction-address)
                       (format #t "    Target offset: ~x~%" target-offset)
                       (bytevector-u32-set! resolved-code imm-offset target-offset (endianness little)))))
                 (format #t "  After VMOVAPS resolution: ~x~%" 
                         (bytevector-u32-ref resolved-code imm-offset (endianness little)))
                 (loop (+ offset 8))))
              
              (else (loop (+ offset 1)))))
          (begin
            (format #t "Finished resolving references.~%")
            resolved-code)))))

(define (link-code assembled-code symbol-addresses)
  (let* ((symbols (map car symbol-addresses))
         (available-registers '(7 6 2 1 0 3 4 5))  ; Add more if needed
         (reg-to-symbol-map 
          (map cons 
               (take available-registers (min (length symbols) (length available-registers)))
               symbols)))
    (resolve-references assembled-code symbol-addresses reg-to-symbol-map)))

; Helper function to implement 'take' functionality
(define (take lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (create-executable linked-code output-file data-sections symbol-addresses)
  (format #t "Creating executable. Linked code size: ~a~%" (bytevector-length linked-code))
  (format #t "Data sections: ~a~%" data-sections)
  (format #t "Symbol addresses: ~a~%" symbol-addresses)
  (let* ((code-size (bytevector-length linked-code))
         (data-size (apply + (map (lambda (x) (bytevector-length (cdr x))) data-sections)))
         (highest-address (apply max (map cdr symbol-addresses)))
         (total-size (- (+ highest-address data-size) #x400000))
         (elf-header (make-bytevector 64 0))
         (symbol-table (create-symbol-table symbol-addresses))
         (symbol-table-size (bytevector-length symbol-table))
         (string-table (create-string-table symbol-addresses))
         (string-table-size (bytevector-length string-table))
         (section-header-table (create-section-header-table 
                                 total-size 
                                 symbol-table-size 
                                 string-table-size)))

    ;; Create ELF header
    (bytevector-copy! #vu8(#x7f #x45 #x4c #x46 #x02 #x01 #x01 #x00) 0 elf-header 0 8)
    (bytevector-u16-set! elf-header 16 2 (endianness little))
    (bytevector-u16-set! elf-header 18 #x3e (endianness little))
    (bytevector-u32-set! elf-header 20 1 (endianness little))
    (bytevector-u64-set! elf-header 24 #x401000 (endianness little)) ; e_entry
    (bytevector-u64-set! elf-header 32 64 (endianness little))
    (bytevector-u64-set! elf-header 40 0 (endianness little))
    (bytevector-u32-set! elf-header 48 0 (endianness little))
    (bytevector-u16-set! elf-header 52 64 (endianness little))
    (bytevector-u16-set! elf-header 54 56 (endianness little))
    (bytevector-u16-set! elf-header 56 1 (endianness little))
    (bytevector-u16-set! elf-header 58 0 (endianness little))
    (bytevector-u16-set! elf-header 60 0 (endianness little))
    (bytevector-u16-set! elf-header 62 0 (endianness little))

    ;; Update ELF header with section information
    (bytevector-u16-set! elf-header 58 4 (endianness little)) ; e_shnum (4 sections: null, .text, .symtab, .strtab)
    (bytevector-u64-set! elf-header 40 (- total-size (* 4 64)) (endianness little)) ; e_shoff

    ;; Create full executable
    (let ((full-executable (make-bytevector (+ total-size 
                                               symbol-table-size 
                                               string-table-size 
                                               (* 4 64)) 0)))
      ;; Copy ELF header
      (bytevector-copy! elf-header 0 full-executable 0 64)
      ;; Copy program header
      (bytevector-copy! (create-program-header 0 #x400000 total-size) 0 full-executable 64 56)
      ;; Copy linked code
      (bytevector-copy! linked-code 0 full-executable #x1000 code-size)
      ;; Copy data sections
      (for-each 
       (lambda (section)
         (let* ((section-name (car section))
                (data (cdr section))
                (size (bytevector-length data))
                (address (- (cdr (assoc section-name symbol-addresses)) #x400000)))
           (bytevector-copy! data 0 full-executable address size)))
       data-sections)
      
      ;; Copy symbol table
      (bytevector-copy! symbol-table 0 full-executable total-size symbol-table-size)
      
      ;; Copy string table
      (bytevector-copy! string-table 0 full-executable (+ total-size symbol-table-size) string-table-size)
      
      ;; Copy section header table
      (bytevector-copy! section-header-table 0 full-executable 
                        (+ total-size symbol-table-size string-table-size) 
                        (* 4 64))

      (format #t "Writing executable to file: ~a~%" output-file)
      ;; Write the full executable to file
      (call-with-output-file output-file
        (lambda (port)
          (put-bytevector port full-executable)))
      
      (chmod output-file #o755) ; Make the file executable

      ;; Logging
      (format #t "Executable created: ~a~%" output-file)
      (format #t "Code size: ~a bytes~%" code-size)
      (format #t "Data size: ~a bytes~%" data-size)
      (format #t "Total size: ~a bytes~%" total-size)
      (format #t "Entry point: 0x401000~%")
      (for-each (lambda (section)
                  (format #t "Section ~a at address 0x~x, size ~a bytes~%"
                          (car section)
                          (cdr (assoc (car section) symbol-addresses))
                          (bytevector-length (cdr section))))
                data-sections)
    )

    ;; Display symbol table contents
    (display-symbol-table symbol-table string-table)
  ))

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

(define (create-section-header-table total-size symbol-table-size string-table-size)
  (let* ((header-size 64)
         (num-headers 4)
         (table-size (* header-size num-headers))
         (table (make-bytevector table-size 0)))
    ;; NULL section
    (create-section-header table 0 0 0 0 0 0 0 0 0 0 0)
    ;; .text section
    (create-section-header table header-size 1 1 6 #x400000 #x1000 total-size 0 0 #x1000 0)
    ;; .symtab section
    (create-section-header table (* 2 header-size) 11 2 3 (+ #x400000 total-size) total-size symbol-table-size 3 0 8 24)
    ;; .strtab section
    (create-section-header table (* 3 header-size) 19 3 3 (+ #x400000 total-size symbol-table-size) (+ total-size symbol-table-size) string-table-size 0 0 1 0)
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