(use-modules (srfi srfi-64)
             (shared-object-creator)
             (elf-header)
             (program-headers)
             (section-headers)
             (dynamic-section)
             (symbol-table)
             (string-table)
             (utils)
             (rnrs bytevectors)
             (ice-9 format))

(test-begin "shared-object-creator")

;; Helper function to create a mock ELF layout
(define (create-mock-layout)
  '((program-headers-offset . #x40)
    (code-size . #x1000)
    (data-size . #x1000)
    (symtab-size . #x100)
    (strtab-size . #x100)
    (shstrtab-size . #x100)
    (dynamic-symbol-table-size . #x200)
    (relocation-table-size . #x200)
    (got-size . #x100)
    (data-offset . #x2000)
    (dynamic-offset . #x3000)
    (dynamic-size . #x200)
    (dynsym-offset . #x3200)
    (dynstr-offset . #x3400)
    (rodata-size . #x100)
    (rela-offset . #x3600)
    (got-offset . #x3800)
    (plt-offset . #x3900)
    (total-dynamic-size . #x1000)
    (section-headers-offset . #x4000)
    (shstrtab-addr . #x5000)
    (total-size . #x6000)
    (data-addr . #x2000)
    (dynamic-addr . #x3000)
    (bss-size . #x100)
    (plt-size . #x100)
    (hash-offset . #x3A00)
    (hash-size . #x100)
    (symtab-offset . #x4100)
    (strtab-offset . #x4200)))

;; Test custom-assert function
(test-group "custom-assert"
  (test-assert (custom-assert #t "This should not raise an error"))
  (test-error (custom-assert #f "This should raise an error")))

;; Test verify-dynamic-section function
(test-group "verify-dynamic-section"
  (let* ((mock-layout (create-mock-layout))
         (dynamic-section (create-dynamic-section
                            (assoc-ref mock-layout 'dynstr-offset)
                            (assoc-ref mock-layout 'dynsym-offset)
                            (assoc-ref mock-layout 'strtab-size)
                            (assoc-ref mock-layout 'dynamic-symbol-table-size)
                            (assoc-ref mock-layout 'rela-offset)
                            (assoc-ref mock-layout 'relocation-table-size)
                            (assoc-ref mock-layout 'got-offset)
                            (assoc-ref mock-layout 'hash-offset))))
    (test-assert (verify-dynamic-section 
                   dynamic-section
                   (assoc-ref mock-layout 'dynstr-offset)
                   (assoc-ref mock-layout 'dynsym-offset)
                   (assoc-ref mock-layout 'strtab-size)
                   (assoc-ref mock-layout 'dynamic-symbol-table-size)
                   (assoc-ref mock-layout 'rela-offset)
                   (assoc-ref mock-layout 'relocation-table-size)))))

;; Test check-section-overlaps function
(test-group "check-section-overlaps"
  (test-assert (check-section-overlaps
                 '((#x1000 #x100 "Section1")
                   (#x1200 #x100 "Section2")
                   (#x1400 #x100 "Section3"))))
  (test-error (check-section-overlaps
                '((#x1000 #x200 "Section1")
                  (#x1100 #x200 "Section2")))))

;; Test verify-segment-contents function
(test-group "verify-segment-contents"
  (let ((mock-layout (create-mock-layout)))
    (test-assert (verify-segment-contents
                   (assoc-ref mock-layout 'data-offset)
                   (+ (assoc-ref mock-layout 'dynamic-offset)
                      (assoc-ref mock-layout 'total-dynamic-size))
                   (assoc-ref mock-layout 'dynstr-offset)
                   (assoc-ref mock-layout 'dynsym-offset)
                   (assoc-ref mock-layout 'rela-offset)
                   (assoc-ref mock-layout 'relocation-table-size)))))

;; Test create-shared-object function (this is more of an integration test)
(test-group "create-shared-object"
  (let* ((mock-code (make-bytevector #x1000 #xAA))
         (mock-data (make-bytevector #x1000 #xBB))
         (mock-symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (mock-label-positions '((label1 . #x100) (label2 . #x200)))
         (output-file "test-output.so"))
    (test-assert (> (create-shared-object
                      mock-code
                      `((data . ,mock-data))
                      output-file
                      mock-symbol-addresses
                      mock-label-positions)
                    0))
    (test-assert (file-exists? output-file))
    (delete-file output-file)))  ; Clean up after test

(test-end "shared-object-creator")
