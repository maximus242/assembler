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

(define (resolve-references code symbol-table reg-to-symbol-map)
  (let* ((code-length (bytevector-length code))
         (resolved-code (bytevector-copy code)))
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
                   (let* ((symbol-name (hash-ref reg-to-symbol-map reg))
                          (symbol-address (and symbol-name (hash-ref symbol-table symbol-name))))
                     (format #t "  Resolving symbol: ~a -> ~a~%" 
                             (or symbol-name "#f") 
                             (if symbol-address 
                                 (format #f "~x" symbol-address)
                                 "#f"))
                     (when symbol-address
                       (bytevector-u32-set! resolved-code imm-offset symbol-address (endianness little)))))
                 (loop (+ offset 7))))
              
              ; Handle VMOVAPS
              ((and (= instruction #xC5)
                    (< (+ offset 7) code-length)
                    (= (bytevector-u8-ref code (+ offset 1)) #xFC)
                    (= (bytevector-u8-ref code (+ offset 2)) #x28)
                    (= (bytevector-u8-ref code (+ offset 3)) #x05))
               (let* ((imm-offset (+ offset 4))
                      (imm (bytevector-u32-ref code imm-offset (endianness little))))
                 (format #t "  VMOVAPS: displacement=~x~%" imm)
                 (when (= imm 0) ; Possible symbolic reference
                   (let ((symbol-address (hash-ref symbol-table 'multiplier)))
                     (format #t "    Resolving multiplier -> ~a~%" 
                             (if symbol-address 
                                 (format #f "~x" symbol-address)
                                 "#f"))
                     (when symbol-address
                       (bytevector-u32-set! resolved-code imm-offset symbol-address (endianness little)))))
                 (loop (+ offset 8))))
              
              (else (loop (+ offset 1)))))
          resolved-code))))

(define (link-code assembled-code symbol-addresses)
  (let* ((symbol-table (make-hash-table))
         (reg-to-symbol-map (make-hash-table)))
    
    ;; Populate symbol table with provided addresses
    (for-each (lambda (addr-pair)
                (hash-set! symbol-table (car addr-pair) (cdr addr-pair)))
              symbol-addresses)
    
    ;; Set up register to symbol mapping
    (hash-set! reg-to-symbol-map 7 'buffer1)
    (hash-set! reg-to-symbol-map 6 'buffer2)
    (hash-set! reg-to-symbol-map 2 'result)
    
    (resolve-references assembled-code symbol-table reg-to-symbol-map)))

(define (create-executable linked-code output-file data-sections symbol-addresses)
  (let* ((code-size (bytevector-length linked-code))
         (data-size (apply + (map (lambda (x) (bytevector-length (cdr x))) data-sections)))
         (total-size (+ #x2000 code-size data-size)) ; Start at 0x2000 for simplicity
         (elf-header (make-bytevector 64 0)))
    
    ;; Create ELF header
    (bytevector-copy! #vu8(#x7f #x45 #x4c #x46 #x02 #x01 #x01 #x00) 0 elf-header 0 8) ; ELF magic number and class
    (bytevector-u16-set! elf-header 16 2 (endianness little)) ; e_type (ET_EXEC)
    (bytevector-u16-set! elf-header 18 #x3e (endianness little)) ; e_machine (EM_X86_64)
    (bytevector-u32-set! elf-header 20 1 (endianness little)) ; e_version
    (bytevector-u64-set! elf-header 24 #x400000 (endianness little)) ; e_entry
    (bytevector-u64-set! elf-header 32 64 (endianness little)) ; e_phoff
    (bytevector-u64-set! elf-header 40 0 (endianness little)) ; e_shoff
    (bytevector-u32-set! elf-header 48 0 (endianness little)) ; e_flags
    (bytevector-u16-set! elf-header 52 64 (endianness little)) ; e_ehsize
    (bytevector-u16-set! elf-header 54 56 (endianness little)) ; e_phentsize
    (bytevector-u16-set! elf-header 56 1 (endianness little))  ; e_phnum (1 program header)
    (bytevector-u16-set! elf-header 58 0 (endianness little))  ; e_shentsize
    (bytevector-u16-set! elf-header 60 0 (endianness little))  ; e_shnum
    (bytevector-u16-set! elf-header 62 0 (endianness little))  ; e_shstrndx

    ;; Create program header
    (define program-header (create-program-header #x2000 #x400000 (+ code-size data-size)))

    ;; Create full executable
    (let ((full-executable (make-bytevector total-size 0)))
      ;; Copy ELF header
      (bytevector-copy! elf-header 0 full-executable 0 64)
      ;; Copy program header
      (bytevector-copy! program-header 0 full-executable 64 56)
      ;; Copy linked code
      (bytevector-copy! linked-code 0 full-executable #x2000 code-size)
      ;; Copy data sections
      (let ((data-offset (+ #x2000 code-size)))
        (for-each 
         (lambda (section)
           (let* ((data (cdr section))
                  (size (bytevector-length data)))
             (bytevector-copy! data 0 full-executable data-offset size)
             (set! data-offset (+ data-offset size))))
         data-sections))
      
      ;; Write the full executable to file
      (call-with-output-file output-file
        (lambda (port)
          (put-bytevector port full-executable)))
      
      (chmod output-file #o755) ; Make the file executable
    )
  )
)