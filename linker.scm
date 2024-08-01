(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (link create-executable))

(define (make-symbol-table)
  (make-hash-table))

(define (add-symbol! table name address)
  (hash-set! table name address))

(define (get-symbol table name)
  (hash-ref table name))

(define (resolve-references code symbol-table)
  (let* ((code-length (bytevector-length code))
         (resolved-code (bytevector-copy code)))
    (let loop ((offset 0))
      (if (< offset code-length)
          (let ((instruction (bytevector-u8-ref code offset)))
            (format #t "Offset: ~a, Instruction: ~x~%" offset instruction)
            (case instruction
              ((#x48) ; REX.W prefix
               (when (and (< (+ offset 2) code-length)
                          (= (bytevector-u8-ref code (+ offset 1)) #xC7)) ; mov immediate
                 (let* ((reg (logand (bytevector-u8-ref code (+ offset 2)) #x07))
                        (imm-offset (+ offset 3))
                        (imm (bytevector-u32-ref code imm-offset (endianness little))))
                   (format #t "  MOV imm32: reg=~a, imm=~x~%" reg imm)
                   (when (= imm 0) ; Possible symbolic reference
                     (let* ((symbol-name (case reg
                                           ((7) 'buffer1)
                                           ((6) 'buffer2)
                                           ((2) 'result)
                                           (else #f)))
                            (symbol-address (and symbol-name (get-symbol symbol-table symbol-name))))
                       (format #t "  Resolving symbol: ~a -> ~x~%" symbol-name symbol-address)
                       (when symbol-address
                         (bytevector-u32-set! resolved-code imm-offset symbol-address (endianness little)))))))
               (loop (+ offset 7)))
              ((#xC5) ; VEX prefix
               (when (and (< (+ offset 3) code-length)
                          (= (bytevector-u8-ref code (+ offset 1)) #xFC)
                          (= (bytevector-u8-ref code (+ offset 2)) #x28))
                 (let* ((modrm (bytevector-u8-ref code (+ offset 3)))
                        (mod (ash modrm -6))
                        (rm (logand modrm #x07)))
                   (format #t "  VMOVAPS: modrm=~x, mod=~a, rm=~a~%" modrm mod rm)
                   (when (= modrm #x05) ; [rip + disp32]
                     (let* ((imm-offset (+ offset 4))
                            (imm (bytevector-u32-ref code imm-offset (endianness little))))
                       (format #t "    Displacement: ~x~%" imm)
                       (when (= imm 0) ; Possible symbolic reference
                         (let ((symbol-address (get-symbol symbol-table 'multiplier)))
                           (format #t "    Resolving multiplier -> ~x~%" symbol-address)
                           (when symbol-address
                             (bytevector-u32-set! resolved-code imm-offset symbol-address (endianness little)))))))))
               (loop (+ offset 4))) ; Change this to 4 instead of 8
              (else (loop (+ offset 1)))))
          resolved-code))))

(define (link assembled-code)
  (let ((symbol-table (make-symbol-table)))
    ;; Assign addresses to symbols (for this example, we'll use arbitrary addresses)
    (add-symbol! symbol-table 'buffer1 #x1000)
    (add-symbol! symbol-table 'buffer2 #x2000)
    (add-symbol! symbol-table 'result #x3000)
    (add-symbol! symbol-table 'multiplier #x4000)
    
    ;; Resolve references
    (resolve-references assembled-code symbol-table)))

(define (create-executable linked-code output-file)
  (let* ((elf-header (bytevector-copy #vu8(#x7f #x45 #x4c #x46 ; ELF magic number
                                           #x02 #x01 #x01 #x00 ; 64-bit, little endian, current version
                                           #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Padding
                                           #x02 #x00 ; Type (executable)
                                           #x3e #x00 ; Machine (x86-64)
                                           #x01 #x00 #x00 #x00 ; Version
                                           #x78 #x00 #x40 #x00 #x00 #x00 #x00 #x00 ; Entry point (0x400078)
                                           #x40 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Program header offset
                                           #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Section header offset (0, as we're omitting section headers)
                                           #x00 #x00 #x00 #x00 ; Flags
                                           #x40 #x00 ; ELF header size
                                           #x38 #x00 ; Program header entry size
                                           #x01 #x00 ; Number of program headers
                                           #x00 #x00 ; Section header entry size (0, as we're omitting section headers)
                                           #x00 #x00 ; Number of section headers (0, as we're omitting section headers)
                                           #x00 #x00))) ; Section header string table index (0, as we're omitting section headers)
         (program-header (bytevector-copy #vu8(#x01 #x00 #x00 #x00 ; Type (loadable segment)
                                               #x07 #x00 #x00 #x00 ; Flags (read, write, execute)
                                               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Offset in file
                                               #x00 #x00 #x40 #x00 #x00 #x00 #x00 #x00 ; Virtual address
                                               #x00 #x00 #x40 #x00 #x00 #x00 #x00 #x00 ; Physical address
                                               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Size in file (to be filled)
                                               #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Size in memory (to be filled)
                                               #x00 #x10 #x00 #x00 #x00 #x00 #x00 #x00))) ; Alignment
         (headers-size (+ (bytevector-length elf-header)
                          (bytevector-length program-header)))
         (linked-code-size (bytevector-length linked-code))
         (file-size (+ headers-size linked-code-size))
         (full-executable (make-bytevector file-size 0)))
    
    (format #t "Linked code size: ~a bytes~%" linked-code-size)
    (format #t "Total file size: ~a bytes~%" file-size)
    
    ;; Copy ELF header
    (bytevector-copy! elf-header 0 full-executable 0 (bytevector-length elf-header))
    
    ;; Update and copy program header
    (bytevector-u64-set! program-header 32 file-size (endianness little))
    (bytevector-u64-set! program-header 40 file-size (endianness little))
    (bytevector-copy! program-header 0 full-executable 64 (bytevector-length program-header))
    
    ;; Copy linked code
    (bytevector-copy! linked-code 0 full-executable headers-size linked-code-size)
    
    ;; Write the full executable to file
    (call-with-output-file output-file
      (lambda (port)
        (put-bytevector port full-executable)))
    
    (chmod output-file #o755) ; Make the file executable
    (list (bytevector-length elf-header)
          (bytevector-length program-header)
          headers-size
          file-size)))