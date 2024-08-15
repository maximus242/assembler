(use-modules (srfi srfi-64)  ; SRFI-64 for unit testing
             (relocation-table)  ; Import the relocation table module
             (rnrs bytevectors))  ; Bytevectors module

(define little (endianness little))

;; Test for a single entry
(test-begin "create-relocation-table-single-entry")
(let* ((symbol-addresses '((symbol1 . #x1000)))
       (table (create-relocation-table symbol-addresses)))
  (test-assert "relocation table should have the correct size for one entry"
    (= (bytevector-length table) 24))
  (test-assert "r_offset should be correct for the first entry"
    (= (bytevector-u64-ref table 0 little) #x1000))
  (test-assert "r_info should be correct for the first entry"
    (= (bytevector-u64-ref table 8 little) #x0000000100000000))  ; index << 32 | type
  (test-assert "r_addend should be zero for the first entry"
    (= (bytevector-u64-ref table 16 little) 0)))
(test-end)

;; Test for multiple entries
(test-begin "create-relocation-table-multiple-entries")
(let* ((symbol-addresses '((symbol1 . #x1000)
                           (symbol2 . #x2000)
                           (symbol3 . #x3000)))
       (table (create-relocation-table symbol-addresses)))
  (test-assert "relocation table should have the correct size for three entries"
    (= (bytevector-length table) (* 3 24)))
  
  ;; Check first entry
  (test-assert "r_offset should be correct for the first entry"
    (= (bytevector-u64-ref table 0 little) #x1000))
  (test-assert "r_info should be correct for the first entry"
    (= (bytevector-u64-ref table 8 little) #x0000000100000000))
  (test-assert "r_addend should be zero for the first entry"
    (= (bytevector-u64-ref table 16 little) 0))
  
  ;; Check second entry
  (test-assert "r_offset should be correct for the second entry"
    (= (bytevector-u64-ref table 24 little) #x2000))
  (test-assert "r_info should be correct for the second entry"
    (= (bytevector-u64-ref table 32 little) #x0000000100000001))
  (test-assert "r_addend should be zero for the second entry"
    (= (bytevector-u64-ref table 40 little) 0))
  
  ;; Check third entry
  (test-assert "r_offset should be correct for the third entry"
    (= (bytevector-u64-ref table 48 little) #x3000))
  (test-assert "r_info should be correct for the third entry"
    (= (bytevector-u64-ref table 56 little) #x0000000100000002))
  (test-assert "r_addend should be zero for the third entry"
    (= (bytevector-u64-ref table 64 little) 0)))
(test-end)
