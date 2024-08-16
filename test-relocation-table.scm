(use-modules (srfi srfi-64)
             (relocation-table)
             (rnrs bytevectors)
             (ice-9 format))

(define little (endianness little))

(define (print-table-info table entry-size . rest)
  (let* ((options (if (null? rest) '() (car rest)))
         (len (bytevector-length table))
         (r-offset-offset (or (assoc-ref options 'r-offset-offset) 0))
         (r-info-offset (or (assoc-ref options 'r-info-offset) 8))
         (r-addend-offset (or (assoc-ref options 'r-addend-offset) 16)))
    (format #t "Table length: ~a~%" len)
    (do ((i 0 (+ i entry-size)))
        ((>= i len))
      (format #t "Entry ~a:~%" (/ i entry-size))
      (format #t "  r_offset: #x~16,'0x~%" (bytevector-u64-ref table (+ i r-offset-offset) little))
      (format #t "  r_info: #x~16,'0x~%" (bytevector-u64-ref table (+ i r-info-offset) little))
      (format #t "  r_addend: #x~16,'0x~%" (bytevector-u64-ref table (+ i r-addend-offset) little)))))

;; Initialize the test runner
(test-runner-current (test-runner-create))

(test-begin "create-relocation-table-comprehensive")

;; Test for a single entry (old format with symbol)
(let* ((symbol-addresses '((symbol1 . #x1000)))
       (table (create-relocation-table symbol-addresses)))
  (print-table-info table 24)
  (test-assert "relocation table should have the correct size for one entry"
               (= 24 (bytevector-length table)))
  (test-assert "r_offset should be correct for the first entry"
               (= #x1000 (bytevector-u64-ref table 0 little)))
  (test-assert "r_info should be correct for the first entry"
               (= #x0000000100000001 (bytevector-u64-ref table 8 little)))
  (test-assert "r_addend should be zero for the first entry"
               (= 0 (bytevector-u64-ref table 16 little))))

;; Test for a single entry (old format with number)
(let* ((symbol-addresses '(#x2000))
       (table (create-relocation-table symbol-addresses)))
  (print-table-info table 24)
  (test-assert "relocation table should have the correct size for one entry"
               (= 24 (bytevector-length table)))
  (test-assert "r_offset should be correct for the first entry"
               (= #x2000 (bytevector-u64-ref table 0 little)))
  (test-assert "r_info should be correct for the first entry"
               (= #x0000000100000001 (bytevector-u64-ref table 8 little)))
  (test-assert "r_addend should be zero for the first entry"
               (= 0 (bytevector-u64-ref table 16 little))))

;; Test for multiple entries
(let* ((symbol-addresses '((symbol1 . #x1000)
                           (symbol2 . #x2000)
                           (symbol3 . #x3000)))
       (table (create-relocation-table symbol-addresses)))
  (print-table-info table 24)
  (test-assert "relocation table should have the correct size for three entries"
               (= 72 (bytevector-length table)))
  
  ;; Check first entry
  (test-assert "r_offset should be correct for the first entry"
               (= #x1000 (bytevector-u64-ref table 0 little)))
  (test-assert "r_info should be correct for the first entry"
               (= #x0000000100000001 (bytevector-u64-ref table 8 little)))
  (test-assert "r_addend should be zero for the first entry"
               (= 0 (bytevector-u64-ref table 16 little)))
  
  ;; Check second entry
  (test-assert "r_offset should be correct for the second entry"
               (= #x2000 (bytevector-u64-ref table 24 little)))
  (test-assert "r_info should be correct for the second entry"
               (= #x0000000200000002 (bytevector-u64-ref table 32 little)))
  (test-assert "r_addend should be zero for the second entry"
               (= 0 (bytevector-u64-ref table 40 little)))
  
  ;; Check third entry
  (test-assert "r_offset should be correct for the third entry"
               (= #x3000 (bytevector-u64-ref table 48 little)))
  (test-assert "r_info should be correct for the third entry"
               (= #x0000000300000003 (bytevector-u64-ref table 56 little)))
  (test-assert "r_addend should be zero for the third entry"
               (= 0 (bytevector-u64-ref table 64 little))))

;; Test with custom options
(let* ((symbol-addresses '((symbol1 . #x1000)))
       (custom-options '((reloc-entry-size . 32)
                         (r-offset-offset . 0)
                         (r-info-offset . 16)
                         (r-addend-offset . 24)
                         (r-x86-64-64 . 1)
                         (symbol-index-start . 1)))
       (table (create-relocation-table symbol-addresses custom-options)))
  (print-table-info table 32 custom-options)
  (test-assert "relocation table should have the correct size with custom entry size"
               (= 32 (bytevector-length table)))
  (let ((r-offset (bytevector-u64-ref table 0 little))
        (r-info (bytevector-u64-ref table 16 little))
        (r-addend (bytevector-u64-ref table 24 little)))
    (format #t "Custom options test:~%")
    (format #t "  r_offset: #x~16,'0x~%" r-offset)
    (format #t "  r_info: #x~16,'0x~%" r-info)
    (format #t "  r_addend: #x~16,'0x~%" r-addend)
    (test-assert "r_offset should be correct with custom offset"
                 (= #x1000 r-offset))
    (test-assert "r_info should be correct with custom offset and type"
                 (= #x0000000100000001 r-info))
    (test-assert "r_addend should be zero with custom offset"
                 (= 0 r-addend))))

;; Test for multiple entries with different relocation types (new format)
(let* ((symbol-addresses '((#x0000 . "buffer1")
                           (#x0008 . "buffer2")
                           (#x0010 . "result")
                           (#x0018 . "multiplier")))
       (custom-options '((r-x86-64-64 . 1)
                         (r-x86-64-pc32 . 2)
                         (r-x86-64-got32 . 3)
                         (r-x86-64-plt32 . 4)
                         (symbol-index-start . 1)))
       (table (create-relocation-table symbol-addresses custom-options)))
  (print-table-info table 24)
  (test-assert "relocation table should have the correct size for four entries"
               (= 96 (bytevector-length table)))
  
  ;; Check first entry (R_X86_64_64)
  (test-assert "r_offset should be correct for the first entry"
               (= #x0000 (bytevector-u64-ref table 0 little)))
  (test-assert "r_info should be correct for the first entry (R_X86_64_64)"
               (= #x0000000100000001 (bytevector-u64-ref table 8 little)))
  (test-assert "r_addend should be zero for the first entry"
               (= 0 (bytevector-u64-ref table 16 little)))
  
  ;; Check second entry (R_X86_64_PC32)
  (test-assert "r_offset should be correct for the second entry"
               (= #x0008 (bytevector-u64-ref table 24 little)))
  (test-assert "r_info should be correct for the second entry (R_X86_64_PC32)"
               (= #x0000000200000002 (bytevector-u64-ref table 32 little)))
  (test-assert "r_addend should be zero for the second entry"
               (= 0 (bytevector-u64-ref table 40 little)))
  
  ;; Check third entry (R_X86_64_GOT32)
  (test-assert "r_offset should be correct for the third entry"
               (= #x0010 (bytevector-u64-ref table 48 little)))
  (test-assert "r_info should be correct for the third entry (R_X86_64_GOT32)"
               (= #x0000000300000003 (bytevector-u64-ref table 56 little)))
  (test-assert "r_addend should be zero for the third entry"
               (= 0 (bytevector-u64-ref table 64 little)))
  
  ;; Check fourth entry (R_X86_64_PLT32)
  (test-assert "r_offset should be correct for the fourth entry"
               (= #x0018 (bytevector-u64-ref table 72 little)))
  (test-assert "r_info should be correct for the fourth entry (R_X86_64_PLT32)"
               (= #x0000000400000004 (bytevector-u64-ref table 80 little)))
  (test-assert "r_addend should be zero for the fourth entry"
               (= 0 (bytevector-u64-ref table 88 little))))

(test-end "create-relocation-table-comprehensive")

;; Exit with appropriate status
(exit (= (test-runner-fail-count (test-runner-current)) 0))