(use-modules (srfi srfi-64)
             (symbol-table)
             (rnrs bytevectors))

(define little (endianness little))

(test-begin "symbol-table")

;; Test make-symbol-table function
(test-group "make-symbol-table"
  (test-assert "symbol table is a hash-table"
    (hash-table? (make-symbol-table))))

;; Test add-symbol! and get-symbol functions
(test-group "add-symbol! and get-symbol"
  (let ((table (make-symbol-table)))
    (add-symbol! table 'symbol1 #x1000)
    (test-equal "symbol1 should be in the table with the correct address"
      #x1000 (get-symbol table 'symbol1))
    (test-assert "non-existent symbol should return #f"
      (not (get-symbol table 'symbol2)))))

;; New test group for label symbols
(test-group "label-symbols"
  (let ((table (make-symbol-table)))
    (test-assert "Add label symbol"
      (begin
        (add-label-symbol! table 'main #x1000)
        (get-symbol table 'main)))
    
    (test-eqv "Get label symbol address"
      #x1000
      (get-symbol table 'main))
    
    (test-assert "Label symbol should be in symbol table"
      (symbol-in-table? table 'main))
    
    (test-assert "Label symbol should be marked as a function"
      (symbol-is-function? table 'main))
    
    (test-assert "Data symbol should not be marked as a function"
      (begin
        (add-symbol! table 'data1 #x2000)
        (not (symbol-is-function? table 'data1))))))

;; Updated test for create-symbol-table function
(test-group "create-symbol-table"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (label-positions '((main . #x3000) (func1 . #x3100)))
         (sym-table (create-symbol-table symbol-addresses label-positions)))
    (test-assert "data symbols should be in the table"
      (and (symbol-in-table? sym-table 'symbol1)
           (symbol-in-table? sym-table 'symbol2)))
    (test-assert "label symbols should be in the table"
      (and (symbol-in-table? sym-table 'main)
           (symbol-in-table? sym-table 'func1)))
    (test-assert "label symbols should be marked as functions"
      (and (symbol-is-function? sym-table 'main)
           (symbol-is-function? sym-table 'func1)))
    (test-assert "data symbols should not be marked as functions"
      (and (not (symbol-is-function? sym-table 'symbol1))
           (not (symbol-is-function? sym-table 'symbol2))))))

;; Test create-dynamic-symbol-table function
(test-group "create-dynamic-symbol-table"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (label-positions '((main . #x3000) (func1 . #x3100)))
         (sym-table (create-symbol-table symbol-addresses label-positions))
         (dynsym-table (create-dynamic-symbol-table sym-table)))
    (test-assert "dynamic symbol table bytevector should be of the correct size"
      (= (bytevector-length dynsym-table) (* (+ (length symbol-addresses) (length label-positions) 1) 24)))
    (test-equal "first entry name offset should be zero"
      0 (bytevector-u32-ref dynsym-table 0 little))
    (test-equal "first entry info should be zero"
      0 (bytevector-u8-ref dynsym-table 4))))

;; Test create-dynamic-symbol-table edge cases
(test-group "create-dynamic-symbol-table-edge-cases"
  ;; Test with maximum address value
  (let* ((max-address #xFFFFFFFFFFFFFFFF)
         (symbol-addresses `((symbol1 . ,max-address)))
         (label-positions '((main . #x1000)))
         (sym-table (create-symbol-table symbol-addresses label-positions))
         (dynsym-table (create-dynamic-symbol-table sym-table)))
    (test-equal "dynamic symbol table should handle maximum address value"
      max-address (bytevector-u64-ref dynsym-table 32 little)))

  ;; Test for endianness with a known pattern
  (let* ((test-address #x0102030405060708)
         (symbol-addresses `((symbol1 . ,test-address)))
         (label-positions '((main . #x1000)))
         (sym-table (create-symbol-table symbol-addresses label-positions))
         (dynsym-table (create-dynamic-symbol-table sym-table)))
    (test-equal "dynamic symbol table should handle endianness correctly"
      test-address (bytevector-u64-ref dynsym-table 32 little))))

;; Test create-hash-section function
(test-group "create-hash-section"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (label-positions '((main . #x3000) (func1 . #x3100)))
         (sym-table (create-symbol-table symbol-addresses label-positions))
         (dynsym-table (create-dynamic-symbol-table sym-table))
         (hash-section (create-hash-section dynsym-table)))
    (test-assert "hash section bytevector should be of the correct size"
      (= (bytevector-length hash-section) (+ 8 (* 4 1) (* 4 (+ (length symbol-addresses) (length label-positions) 1)))))
    (test-equal "number of buckets should be 1"
      1 (bytevector-u32-ref hash-section 0 little))
    (test-equal "number of chains should match the total number of symbols"
      (+ (length symbol-addresses) (length label-positions) 1) (bytevector-u32-ref hash-section 4 little))))

(test-end "symbol-table")
