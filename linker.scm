(define-module (linker)
  #:use-module (elf-header)
  #:use-module (program-headers)
  #:use-module (section-headers)
  #:use-module (dynamic-section)
  #:use-module (symbol-table)
  #:use-module (string-table)
  #:use-module (utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 format)
  #:export (link-code create-shared-object))

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
                      (imm (bytevector-u32-ref code imm-offset (endianness little)))
                      (symbol-address (alist-ref 'multiplier symbol-table)))
                 (format #t "  VMOVAPS: original displacement=~x~%" imm)
                 (when symbol-address
                   (let* ((instruction-end (+ offset 8))
                          (next-instruction-address (+ instruction-end code-base-address))
                          (target-offset (- (adjust-address symbol-address) next-instruction-address)))
                     (format #t "    Resolving multiplier -> ~x~%" symbol-address)
                     (format #t "    Instruction end: ~x~%" instruction-end)
                     (format #t "    Next instruction address: ~x~%" next-instruction-address)
                     (format #t "    Target offset: ~x~%" target-offset)
                     (bytevector-u32-set! resolved-code imm-offset target-offset (endianness little))))
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
              
              (else 
                (loop (+ offset 1)))))
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

(define (write-bytevector bv port)
  (put-bytevector port bv))

(define (estimate-dynamic-size)
  (* 16 10))  ; Estimate 10 entries, each 16 bytes long

(define (create-shared-object code data-sections output-file symbol-addresses label-positions)
  (let* ((elf-header-size 64)
         (program-headers-offset elf-header-size)
         (entry-point #x1000)
         (code-size (bytevector-length code))
         (data-size (apply + (map (lambda (pair) (bytevector-length (cdr pair))) data-sections)))
         (symtab (create-symbol-table symbol-addresses))
         (symtab-size (bytevector-length symtab))
         (strtab (create-string-table symbol-addresses))
         (strtab-size (bytevector-length strtab))
         (shstrtab (create-section-header-string-table))
         (shstrtab-size (bytevector-length shstrtab))
         (dynamic-symbol-table (create-dynamic-symbol-table symbol-addresses))
         (dynamic-symbol-table-size (bytevector-length dynamic-symbol-table))
         (relocation-table (create-relocation-table symbol-addresses))
         (relocation-table-size (bytevector-length relocation-table))
         (code-offset #x1000)
         (data-offset (align-to (+ code-offset code-size) #x1000))
         (dynamic-offset (align-to (+ data-offset data-size) #x1000))
         
         ;; Create dynamic section first to get its size
         (dynamic-section (create-dynamic-section 
                            (+ dynamic-offset 128)  ; dynstr offset
                            (+ dynamic-offset 128 strtab-size)  ; dynsym offset
                            strtab-size
                            dynamic-symbol-table-size
                            (+ dynamic-offset 128 strtab-size dynamic-symbol-table-size)  ; rela offset
                            relocation-table-size))
         (dynamic-size (bytevector-length dynamic-section))
         (total-dynamic-size (+ dynamic-size strtab-size dynamic-symbol-table-size relocation-table-size))
         (section-headers-offset (align-to (+ dynamic-offset total-dynamic-size) #x1000))
         (num-sections 14)
         (section-headers (create-section-headers 
                           code-size
                           data-size
                           symtab-size
                           strtab-size
                           shstrtab-size
                           dynamic-symbol-table-size
                           strtab-size
                           relocation-table-size
                           total-dynamic-size))
         (program-headers (create-program-headers 
                           code-size
                           data-size
                           total-dynamic-size))
         (program-headers-size (bytevector-length program-headers))
         (num-program-headers (/ program-headers-size 56))
         (section-headers-size (* num-sections 64))
         (total-size (+ section-headers-offset section-headers-size))
         (elf-header (create-elf-header entry-point 
                                        program-headers-offset
                                        program-headers-size 
                                        section-headers-offset 
                                        num-program-headers 
                                        num-sections
                                        total-size)))

    (format #t "Calculated dynamic section size: ~a bytes~%" dynamic-size)
    (format #t "Actual dynamic section content:~%")
    (display-bytevector dynamic-section)

    (format #t "Total dynamic size: 0x~x~%" total-dynamic-size)

    ;; Create the ELF file
    (let ((elf-file (make-bytevector total-size 0)))
      ;; Write ELF header
      (bytevector-copy! elf-header 0 elf-file 0 (bytevector-length elf-header))
      
      ;; Write program headers
      (bytevector-copy! program-headers 0 elf-file program-headers-offset program-headers-size)
      
      ;; Write .text section
      (bytevector-copy! code 0 elf-file code-offset code-size)
      
      ;; Write .data section
      (for-each (lambda (pair)
                  (bytevector-copy! (cdr pair) 0 elf-file data-offset (bytevector-length (cdr pair)))
                  (set! data-offset (+ data-offset (bytevector-length (cdr pair)))))
                data-sections)
      
      ;; Write .dynamic section
      (bytevector-copy! dynamic-section 0 elf-file dynamic-offset dynamic-size)
      (format #t "Writing dynamic section at offset: 0x~x~%" dynamic-offset)
      
      ;; Write .dynstr section
      (bytevector-copy! strtab 0 elf-file (+ dynamic-offset dynamic-size) strtab-size)
      (format #t "Writing .dynstr section at offset: 0x~x~%" (+ dynamic-offset dynamic-size))
      
      ;; Write .dynsym section
      (bytevector-copy! dynamic-symbol-table 0 elf-file (+ dynamic-offset dynamic-size strtab-size) dynamic-symbol-table-size)
      (format #t "Writing .dynsym section at offset: 0x~x~%" (+ dynamic-offset dynamic-size strtab-size))
      
      ;; Write .rela.dyn section
      (bytevector-copy! relocation-table 0 elf-file (+ dynamic-offset dynamic-size strtab-size dynamic-symbol-table-size) relocation-table-size)
      (format #t "Writing .rela.dyn section at offset: 0x~x~%" (+ dynamic-offset dynamic-size strtab-size dynamic-symbol-table-size))
      
      ;; Write section headers
      (bytevector-copy! section-headers 0 elf-file section-headers-offset section-headers-size)
      
      ;; Write the ELF file to disk
      (call-with-output-file output-file
        (lambda (port)
          (put-bytevector port elf-file)))
      
      (format #t "Shared object created: ~a~%" output-file)
      (format #t "Total file size: ~a bytes~%" total-size))))

(define (create-relocation-table symbol-addresses)
  (let* ((reloc-count (length symbol-addresses))
         (table-size (* reloc-count 24))
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

;; Helper function to display bytevector contents
(define (display-bytevector bv)
  (let ((len (bytevector-length bv)))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (format #t "~2,'0x " (bytevector-u8-ref bv i))
      (when (= (modulo (+ i 1) 16) 0)
        (newline)))))
