(use-modules (srfi srfi-64)
             (rnrs bytevectors)
             (section-headers)
             (utils))

(test-begin "section-headers")

(define (bytevector-slice bv start end)
  (let ((slice (make-bytevector (- end start))))
    (bytevector-copy! bv start slice 0 (- end start))
    slice))

;; Helper function to extract a 32-bit value from a bytevector
(define (get-u32 bv offset)
  (bytevector-u32-ref bv offset (endianness little)))

;; Helper function to extract a 64-bit value from a bytevector
(define (get-u64 bv offset)
  (bytevector-u64-ref bv offset (endianness little)))

;; Test create-section-header function
(test-group "create-section-header"
  (let* ((string-table (create-section-header-string-table))
         (header (create-section-header ".text" 1 #x1000 #x1000 100 0 0 16 0 string-table)))
    (test-equal 64 (bytevector-length header))
    (test-equal 1 (get-u32 header 0))  ; Offset of ".text" in string table
    (test-equal 1 (get-u32 header 4))  ; Type
    (test-equal #x1000 (get-u64 header 8))  ; Address
    (test-equal #x1000 (get-u64 header 16))  ; Offset
    (test-equal 100 (get-u64 header 24))  ; Size
    (test-equal 0 (get-u32 header 32))  ; Link
    (test-equal 0 (get-u32 header 36))  ; Info
    (test-equal 16 (get-u64 header 40))  ; Align
    (test-equal 0 (get-u64 header 48))))  ; Entsize

;; Test create-section-header-string-table function
(test-group "create-section-header-string-table"
  (let ((table (create-section-header-string-table)))
    (test-assert (bytevector? table))
    (test-assert (> (bytevector-length table) 0))
    (test-equal 0 (bytevector-u8-ref table 0))  ; First byte should be null
    (test-equal ".text" (utf8->string (bytevector-slice table 1 6)))
    (test-equal ".dynamic" (utf8->string (bytevector-slice table (- (bytevector-length table) 9) (- (bytevector-length table) 1))))))

;; Test create-section-headers function
(test-group "create-section-headers"
  (let* ((code-size 100)
         (data-size 50)
         (symtab-size 200)
         (strtab-size 100)
         (shstrtab-size 80)
         (dynsym-size 150)
         (rela-size 120)
         (dynamic-size 80)
         (headers (create-section-headers code-size data-size symtab-size strtab-size 
                                          shstrtab-size dynsym-size rela-size dynamic-size)))
    
    (test-equal (* 9 64) (bytevector-length headers))  ; 9 headers, 64 bytes each
    
    ;; Test null header
    (test-equal 0 (get-u32 headers 0))
    (test-equal 0 (get-u32 headers 4))
    
    ;; Test .text header
    (test-equal 1 (get-u32 headers 68))  ; Offset of ".text" in string table
    (test-equal 1 (get-u32 headers 72))  ; Type
    (test-equal #x1000 (get-u64 headers 76))  ; Address
    (test-equal #x1000 (get-u64 headers 84))  ; Offset
    (test-equal code-size (get-u64 headers 92))  ; Size
    
    ;; Test .data header
    (let ((data-offset (+ #x1000 (align-to code-size #x1000))))
      (test-equal 7 (get-u32 headers 132))  ; Offset of ".data" in string table
      (test-equal 1 (get-u32 headers 136))  ; Type
      (test-equal data-offset (get-u64 headers 140))  ; Address
      (test-equal data-offset (get-u64 headers 148))  ; Offset
      (test-equal data-size (get-u64 headers 156)))  ; Size
    
    ;; Test .dynamic header (last header)
    (let ((dynamic-offset (+ #x1000 
                             (align-to code-size #x1000)
                             (align-to data-size #x1000)
                             symtab-size
                             strtab-size
                             shstrtab-size
                             dynsym-size
                             rela-size)))
      (test-equal 57 (get-u32 headers 516))  ; Offset of ".dynamic" in string table
      (test-equal 6 (get-u32 headers 520))  ; Type
      (test-equal dynamic-offset (get-u64 headers 524))  ; Address
      (test-equal dynamic-offset (get-u64 headers 532))  ; Offset
      (test-equal dynamic-size (get-u64 headers 540))  ; Size
      (test-equal 8 (get-u64 headers 556))  ; Align
      (test-equal 16 (get-u64 headers 564)))))  ; Entsize

;; Test alignment issues
(test-group "alignment"
  (let* ((code-size 4095)  ; Not aligned to 4096
         (data-size 4097)  ; Just over 4096
         (headers (create-section-headers code-size data-size 100 100 100 100 100 100)))
    
    (test-equal #x1000 (get-u64 headers 76))  ; .text address
    (test-equal #x1000 (get-u64 headers 84))  ; .text offset
    (test-equal #x2000 (get-u64 headers 140))  ; .data address (should be aligned to next 4K boundary)
    (test-equal #x2000 (get-u64 headers 148))))  ; .data offset

;; Test dynamic section placement
(test-group "dynamic-section"
  (let* ((code-size 1000)
         (data-size 1000)
         (dynamic-size 1000)
         (headers (create-section-headers code-size data-size 100 100 100 100 100 dynamic-size)))
    
    (let ((dynamic-offset (get-u64 headers 524)))
      (test-assert (>= dynamic-offset (+ #x1000 code-size data-size)))
      (test-equal 0 (modulo dynamic-offset 8))  ; Should be 8-byte aligned
      (test-equal dynamic-offset (get-u64 headers 532))  ; Address and offset should be the same
      (test-equal dynamic-size (get-u64 headers 540)))))

(test-end "section-headers")
