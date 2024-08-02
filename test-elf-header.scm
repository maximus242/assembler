(define-module (test-elf-header-issues)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:use-module (linker))

(test-begin "elf-header-issues")

(define (create-test-elf-file)
  ; This function should create an ELF file using your existing code
  ; Return the bytevector representing the entire ELF file
  (let* ((entry-point #x1000)
         (section-header-offset #x2180) ; 8576 in decimal
         (num-sections 6)
         (elf-header (create-elf-header entry-point section-header-offset num-sections))
         (program-headers (create-program-headers))
         (section-headers (create-section-headers 73 128 200 300 100)))
    (bytevector-append elf-header program-headers section-headers)))

(define test-elf-file (create-test-elf-file))

; Test 1: Check ELF header
(test-assert "ELF header magic number"
  (equal? (bytevector-u32-ref test-elf-file 0 (endianness little)) #x464c457f))

(test-equal "Section header offset"
  #x2180
  (bytevector-u64-ref test-elf-file 40 (endianness little)))

; Test 2: Check section header sh_link values
(define (get-sh-link section-index)
  (bytevector-u32-ref test-elf-file 
                      (+ #x2180 (* section-index 64) 48) 
                      (endianness little)))

(test-assert "Section 1 sh_link value in valid range"
  (< (get-sh-link 1) 6))

(test-assert "Section 3 sh_link value in valid range"
  (< (get-sh-link 3) 6))

; Test 3: Check padding between segments and section headers
(define last-segment-end 
  (+ #x1050 128)) ; Offset of second LOAD segment + its size

(test-assert "Appropriate padding before section headers"
  (and (>= (- #x2180 last-segment-end) 0)
       (<= (- #x2180 last-segment-end) 4096))) ; Allow up to 4K alignment

; Test 4: Verify section header string table index
(test-equal "Section header string table index"
  5
  (bytevector-u16-ref test-elf-file 62 (endianness little)))

; Test 5: Check program header values
(define (get-program-header-value offset)
  (bytevector-u64-ref test-elf-file (+ 64 offset) (endianness little)))

(test-equal "First LOAD segment offset"
  #x1000
  (get-program-header-value 8))

(test-equal "First LOAD segment size"
  73
  (get-program-header-value 32))

(test-equal "Second LOAD segment offset"
  #x1050
  (get-program-header-value 64))

(test-equal "Second LOAD segment size"
  128
  (get-program-header-value 88))

(test-end "elf-header-issues")
