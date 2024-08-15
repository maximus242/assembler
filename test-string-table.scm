;; Import necessary modules for testing
(use-modules (srfi srfi-64)  ; SRFI-64 for unit testing
             (string-table)   ; Import the string-table module
             (rnrs bytevectors))  ; Bytevectors module

;; Local definition of string-match? if not exported
(define (string-match? str bv offset)
  (let ((str-len (string-length str)))
    (and (<= (+ offset str-len) (bytevector-length bv))
         (let loop ((i 0))
           (or (= i str-len)
               (and (= (char->integer (string-ref str i))
                       (bytevector-u8-ref bv (+ offset i)))
                    (loop (+ i 1))))))))

;; Test create-string-table with an empty input
(test-begin "create-string-table-empty")

(let ((table (create-string-table '())))
  (format #t "Length of empty string table: ~a\n" (bytevector-length table))
  (test-assert "string table should be non-empty due to null terminator"
    (= (bytevector-length table) 1))
  (test-assert "string table should contain only the null terminator"
    (= (bytevector-u8-ref table 0) 0)))

(test-end)

;; Test create-string-table with a single entry
(test-begin "create-string-table-single-entry")
(let* ((symbol-addresses '((symbol1 . #x1000)))
       (table (create-string-table symbol-addresses)))
  (format #t "Length of string table with one entry: ~a\n" (bytevector-length table))
  (test-assert "string table should contain the correct length for one entry"
    (= (bytevector-length table) 9))  ; Adjusted expected length
  (test-assert "string table should start with a null terminator"
    (= (bytevector-u8-ref table 0) 0))
  (test-assert "string 'symbol1' should start at the correct offset"
    (string-match? "symbol1" table 1)))
(test-end)

;; Test create-string-table with multiple entries
(test-begin "create-string-table-multiple-entries")
(let* ((symbol-addresses '((symbol1 . #x1000)
                           (symbol2 . #x2000)
                           (symbol3 . #x3000)))
       (table (create-string-table symbol-addresses)))
  (format #t "Length of string table with multiple entries: ~a\n" (bytevector-length table))
  (test-assert "string table should contain the correct length for three entries"
    (= (bytevector-length table) (+ 1 7 1 7 1 7 1)))  ; Empty string + "symbol1\0symbol2\0symbol3\0"
  
  ;; Check each entry's offset
  (test-assert "string 'symbol1' should start at offset 1"
    (string-match? "symbol1" table 1))
  (test-assert "string 'symbol2' should start at offset 9"
    (string-match? "symbol2" table 9))
  (test-assert "string 'symbol3' should start at offset 17"
    (string-match? "symbol3" table 17)))
(test-end)

(test-begin "create-section-header-string-table-default")

(let ((table (create-section-header-string-table)))
  (format #t "Length of default section header string table: ~a\n" (bytevector-length table))
  
  ;; Print offsets for each section name
  (for-each (lambda (section)
              (let ((offset (get-section-name-offset table section)))
                (format #t "Offset for ~a: ~a\n" section offset)))
            '(".text" ".data" ".bss" ".rodata" ".symtab" ".strtab" ".shstrtab" ".rela.text" ".dynamic" ".dynstr" ".dynsym" ".rela.dyn" ".got" ".plt"))
  
  (test-assert "string table should contain the correct default sections length"
    (= (bytevector-length table) 108))  ; Correct length based on observed data
  
  ;; Check specific section names
  (test-assert "string '.text' should be found at the correct offset"
    (string-match? ".text" table 1))
  (test-assert "string '.data' should be found at the correct offset"
    (string-match? ".data" table 7))
  (test-assert "string '.symtab' should be found at the correct offset"
    (string-match? ".symtab" table 26))
  (test-assert "string '.shstrtab' should be found at the correct offset"
    (string-match? ".shstrtab" table 42)))  ; Updated to the observed offset

(test-end)

;; Test string-table-offset for locating strings in a table
(test-begin "string-table-offset")

(let* ((symbol-addresses '((symbol1 . #x1000)
                           (symbol2 . #x2000)
                           (symbol3 . #x3000)))
       (table (create-string-table symbol-addresses)))
  (format #t "Offsets in string table:\n'symbol1': ~a\n'symbol2': ~a\n'symbol3': ~a\n"
          (string-table-offset "symbol1" table)
          (string-table-offset "symbol2" table)
          (string-table-offset "symbol3" table))
  (test-assert "string-table-offset should return the correct offset for 'symbol1'"
    (= (string-table-offset "symbol1" table) 1))
  (test-assert "string-table-offset should return the correct offset for 'symbol2'"
    (= (string-table-offset "symbol2" table) 9))
  (test-assert "string-table-offset should return the correct offset for 'symbol3'"
    (= (string-table-offset "symbol3" table) 17))
  (test-assert "string-table-offset should return #f for a non-existent string"
    (not (string-table-offset "symbol4" table))))

(test-end)

;; Test get-section-name-offset for locating section names in a table
(test-begin "get-section-name-offset")

(let ((table (create-section-header-string-table)))
  (format #t "Offsets in section header string table:\n'.text': ~a\n'.data': ~a\n'.symtab': ~a\n"
          (get-section-name-offset table ".text")
          (get-section-name-offset table ".data")
          (get-section-name-offset table ".symtab"))
  (test-assert "get-section-name-offset should return the correct offset for '.text'"
    (= (get-section-name-offset table ".text") 1))
  (test-assert "get-section-name-offset should return the correct offset for '.data'"
    (= (get-section-name-offset table ".data") 7))
  (test-assert "get-section-name-offset should return #f for a non-existent section name"
    (not (get-section-name-offset table ".foobar"))))

(test-end)
