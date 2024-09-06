(define-module (elf-dynamic-calculator)
  #:use-module (rnrs bytevectors)
  #:export (calculate-entry-point
            calculate-code-offset
            calculate-num-sections
            calculate-shstrtab-index
            calculate-num-program-headers
            calculate-shstrtab-addr
            calculate-shstrtab-size
            calculate-section-offset
            calculate-text-section-offset))

(define (calculate-text-section-offset elf-header-size program-headers-size)
  "Calculate the offset of the .text section from the start of the file."
  (+ elf-header-size program-headers-size))

(define (calculate-entry-point text-addr text-section-offset)
  "Calculate the entry point based on the text address and offset within the .text section."
  text-addr)

(define (calculate-code-offset elf-header-size program-headers)
  "Calculate the code offset based on the size of the ELF header and program headers."
  (+ elf-header-size (* (length program-headers) 56))) ; 56 is the size of a program header

(define (calculate-num-sections sections)
  "Calculate the number of sections based on the actual sections in the ELF file."
  (length sections))

(define (calculate-shstrtab-index sections)
  "Calculate the index of the section header string table."
  (let ((shstrtab-section (find (lambda (section) (eq? (car section) '.shstrtab)) sections)))
    (if shstrtab-section
        (list-index (lambda (section) (eq? section shstrtab-section)) sections)
        (error "Section header string table not found"))))

(define (calculate-num-program-headers program-headers)
  "Calculate the number of program headers."
  (length program-headers))

(define (calculate-shstrtab-addr file-layout)
  "Calculate the address of the section header string table based on the file layout."
  (let ((shstrtab-section (assoc '.shstrtab file-layout)))
    (if shstrtab-section
        (cdr shstrtab-section)
        (error "Section header string table not found in file layout"))))

(define (calculate-shstrtab-size shstrtab-content)
  "Calculate the size of the section header string table."
  (bytevector-length shstrtab-content))

(define (calculate-section-offset elf-header-size program-headers alignment)
  "Calculate the section offset based on the size of the ELF header, program headers, and alignment."
  (let ((total-header-size (+ elf-header-size (* (length program-headers) 56))))
    (align-to alignment total-header-size)))

(define (align-to alignment value)
  "Align a value to the specified alignment."
  (let ((remainder (modulo value alignment)))
    (if (zero? remainder)
        value
        (+ value (- alignment remainder)))))
