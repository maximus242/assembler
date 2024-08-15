(use-modules (srfi srfi-64)
             (relocation-table)
             (rnrs bytevectors))

(define little (endianness little))

(test-begin "create-relocation-table-comprehensive")

;; Test for a single entry
(let* ((symbol-addresses '((symbol1 . #x1000)))
       (table (create-relocation-table symbol-addresses)))
  (test-equal "relocation table should have the correct size for one entry"
    24 (bytevector-length table))
  (test-equal "r_offset should be correct for the first entry"
    #x1000 (bytevector-u64-ref table 0 little))
  (test-equal "r_info should be correct for the first entry"
    #x0000000100000001 (bytevector-u64-ref table 8 little))
  (test-equal "r_addend should be zero for the first entry"
    0 (bytevector-u64-ref table 16 little)))

;; Test for multiple entries
(let* ((symbol-addresses '((symbol1 . #x1000)
                           (symbol2 . #x2000)
                           (symbol3 . #x3000)))
       (table (create-relocation-table symbol-addresses)))
  (test-equal "relocation table should have the correct size for three entries"
    72 (bytevector-length table))
  
  ;; Check first entry
  (test-equal "r_offset should be correct for the first entry"
    #x1000 (bytevector-u64-ref table 0 little))
  (test-equal "r_info should be correct for the first entry"
    #x0000000100000001 (bytevector-u64-ref table 8 little))
  (test-equal "r_addend should be zero for the first entry"
    0 (bytevector-u64-ref table 16 little))
  
  ;; Check second entry
  (test-equal "r_offset should be correct for the second entry"
    #x2000 (bytevector-u64-ref table 24 little))
  (test-equal "r_info should be correct for the second entry"
    #x0000000100000002 (bytevector-u64-ref table 32 little))
  (test-equal "r_addend should be zero for the second entry"
    0 (bytevector-u64-ref table 40 little))
  
  ;; Check third entry
  (test-equal "r_offset should be correct for the third entry"
    #x3000 (bytevector-u64-ref table 48 little))
  (test-equal "r_info should be correct for the third entry"
    #x0000000100000003 (bytevector-u64-ref table 56 little))
  (test-equal "r_addend should be zero for the third entry"
    0 (bytevector-u64-ref table 64 little)))

;; Test with custom options
(let* ((symbol-addresses '((symbol1 . #x1000)))
       (custom-options '((reloc-entry-size . 32)
                         (r-offset-offset . 0)
                         (r-info-offset . 16)
                         (r-addend-offset . 24)
                         (r-x86-64-64 . 2)))
       (table (create-relocation-table symbol-addresses custom-options)))
  (test-equal "relocation table should have the correct size with custom entry size"
    32 (bytevector-length table))
  (test-equal "r_offset should be correct with custom offset"
    #x1000 (bytevector-u64-ref table 0 little))
  (test-equal "r_info should be correct with custom offset and type"
    #x0000000200000001 (bytevector-u64-ref table 16 little))
  (test-equal "r_addend should be zero with custom offset"
    0 (bytevector-u64-ref table 24 little)))

(test-end)
