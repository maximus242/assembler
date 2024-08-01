(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (link-code create-executable))

(define (create-program-header offset vaddr size flags)
  (let ((header (make-bytevector 56 0)))
    (bytevector-u32-set! header 0 1 (endianness little)) ; Type (LOAD)
    (bytevector-u32-set! header 4 flags (endianness little)) ; Flags
    (bytevector-u64-set! header 8 offset (endianness little)) ; Offset
    (bytevector-u64-set! header 16 vaddr (endianness little)) ; Virtual address
    (bytevector-u64-set! header 24 vaddr (endianness little)) ; Physical address
    (bytevector-u64-set! header 32 size (endianness little)) ; File size
    (bytevector-u64-set! header 40 size (endianness little)) ; Memory size
    (bytevector-u64-set! header 48 #x1000 (endianness little)) ; Alignment
    header))

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
                   (let* ((symbol-name (case reg
                                         ((7) 'buffer1)
                                         ((6) 'buffer2)
                                         ((2) 'result)
                                         (else #f)))
                          (symbol-address (and symbol-name (get-symbol symbol-table symbol-name))))
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
                   (let ((symbol-address (get-symbol symbol-table 'multiplier)))
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
  (let* ((code-length (bytevector-length assembled-code))
         (headers-size 120)  ; ELF header (64) + Program header (56)
         (code-base-address #x401000)
         (data-base-address #x402000)
         (symbol-table (make-hash-table)))
    
    ;; Populate symbol table with virtual addresses
    (let loop ((addresses symbol-addresses)
               (current-address data-base-address))
      (if (null? addresses)
          'done
          (let* ((symbol (caar addresses))
                 (size (cdar addresses)))
            (hash-set! symbol-table symbol current-address)
            (loop (cdr addresses) (+ current-address size)))))
    
    (resolve-references assembled-code symbol-table)))

(define (create-executable linked-code output-file data-sections symbol-addresses)
  (let* ((elf-header (bytevector-copy #vu8(#x7f #x45 #x4c #x46 ; ELF magic number
                                           #x02 #x01 #x01 #x00 ; 64-bit, little endian, current version
                                           #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Padding
                                           #x02 #x00 ; Type (executable)
                                           #x3e #x00 ; Machine (x86-64)
                                           #x01 #x00 #x00 #x00 ; Version
                                           #x00 #x10 #x40 #x00 #x00 #x00 #x00 #x00 ; Entry point (0x401000)
                                           #x40 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Program header offset
                                           #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 ; Section header offset (0, as we're omitting section headers)
                                           #x00 #x00 #x00 #x00 ; Flags
                                           #x40 #x00 ; ELF header size
                                           #x38 #x00 ; Program header entry size
                                           #x02 #x00 ; Number of program headers (2: one for code, one for data)
                                           #x00 #x00 ; Section header entry size (0, as we're omitting section headers)
                                           #x00 #x00 ; Number of section headers (0, as we're omitting section headers)
                                           #x00 #x00))) ; Section header string table index (0, as we're omitting section headers)
         (program-header-code (create-program-header #x1000 #x401000 (bytevector-length linked-code) 5)) ; R-X
         (program-header-data (create-program-header #x2000 #x402000 
                                                     (apply + (map (lambda (x) (bytevector-length (cdr x))) data-sections))
                                                     6)) ; RW-
         (headers-size (+ (bytevector-length elf-header)
                          (* 2 (bytevector-length program-header-code))))
         (code-offset #x1000)
         (data-offset #x2000)
         (file-size (+ data-offset (apply + (map (lambda (x) (bytevector-length (cdr x))) data-sections))))
         (full-executable (make-bytevector file-size 0)))
    
    (format #t "Linked code size: ~a bytes~%" (bytevector-length linked-code))
    (format #t "Data size: ~a bytes~%" (- file-size data-offset))
    (format #t "Total file size: ~a bytes~%" file-size)
    
    ;; Copy ELF header
    (bytevector-copy! elf-header 0 full-executable 0 (bytevector-length elf-header))
    
    ;; Copy program headers
    (bytevector-copy! program-header-code 0 full-executable 64 (bytevector-length program-header-code))
    (bytevector-copy! program-header-data 0 full-executable 120 (bytevector-length program-header-data))
    
    ;; Copy linked code
    (bytevector-copy! linked-code 0 full-executable code-offset (bytevector-length linked-code))
    
    ;; Add data sections
    (let loop ((sections data-sections)
               (offset data-offset))
      (if (null? sections)
          'done
          (let* ((data-section (car sections))
                 (data (cdr data-section))
                 (size (bytevector-length data)))
            (bytevector-copy! data 0 full-executable offset size)
            (loop (cdr sections) (+ offset size)))))
    
    ;; Write the full executable to file
    (call-with-output-file output-file
      (lambda (port)
        (put-bytevector port full-executable)))
    
    (chmod output-file #o755) ; Make the file executable
    (list (bytevector-length elf-header)
          (bytevector-length program-header-code)
          headers-size
          file-size)))
