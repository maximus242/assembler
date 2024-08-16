(use-modules (srfi srfi-64)
             (symbol-table)
             (rnrs bytevectors))

(define little (endianness little))

(define (bytevector->list bv)
  (let loop ((i 0) (lst '()))
    (if (= i (bytevector-length bv))
        (reverse lst)
        (loop (+ i 1) (cons (bytevector-u8-ref bv i) lst)))))

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

;; Test create-symbol-table function
(test-group "create-symbol-table"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (result (create-symbol-table symbol-addresses))
         (sym-table (car result))
         (string-table (cdr result)))
    (test-assert "symbol table bytevector should be of the correct size"
      (= (bytevector-length sym-table) (* (+ (length symbol-addresses) 1) 24)))
    (test-assert "string table should be created"
      (bytevector? string-table))))

;; Test create-dynamic-symbol-table function
(test-group "create-dynamic-symbol-table"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (dynsym-table (create-dynamic-symbol-table symbol-addresses)))
    (test-assert "dynamic symbol table bytevector should be of the correct size"
      (= (bytevector-length dynsym-table) (* (+ (length symbol-addresses) 1) 24)))
    (test-equal "first entry name offset should be zero"
      0 (bytevector-u32-ref dynsym-table 0 little))
    (test-equal "first entry info should be zero"
      0 (bytevector-u8-ref dynsym-table 4))))

;; Test create-dynamic-symbol-table edge cases
(test-group "create-dynamic-symbol-table-edge-cases"
  ;; Test with maximum address value
  (let* ((max-address #xFFFFFFFFFFFFFFFF)
         (symbol-addresses `((symbol1 . ,max-address)))
         (dynsym-table (create-dynamic-symbol-table symbol-addresses)))
    (test-equal "dynamic symbol table should handle maximum address value"
      max-address (bytevector-u64-ref dynsym-table 32 little)))

  ;; Test for endianness with a known pattern
  (let* ((test-address #x0102030405060708)
         (symbol-addresses `((symbol1 . ,test-address)))
         (dynsym-table (create-dynamic-symbol-table symbol-addresses)))
    (test-equal "dynamic symbol table should handle endianness correctly"
      test-address (bytevector-u64-ref dynsym-table 32 little))))

;; Test create-hash-section function
(test-group "create-hash-section"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (dynsym-table (create-dynamic-symbol-table symbol-addresses))
         (hash-section (create-hash-section dynsym-table)))
    (test-assert "hash section bytevector should be of the correct size"
      (= (bytevector-length hash-section) (+ 8 (* 4 1) (* 4 (+ (length symbol-addresses) 1)))))
    (test-equal "number of buckets should be 1"
      1 (bytevector-u32-ref hash-section 0 little))
    (test-equal "number of chains should match the number of symbols"
      (+ (length symbol-addresses) 1) (bytevector-u32-ref hash-section 4 little))))

;; Test alignment and padding in Symbol Table
(test-group "alignment-and-padding-in-symbol-table"
  (let* ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000)))
         (result (create-symbol-table symbol-addresses))
         (sym-table (car result)))
    (test-equal "symbol table should have correct alignment for second entry"
      #x1000 (bytevector-u64-ref sym-table 32 little))
    (test-equal "symbol table should have correct alignment for third entry"
      #x2000 (bytevector-u64-ref sym-table 56 little))))

(test-end "symbol-table")
