;; Import the necessary modules for testing
(use-modules (srfi srfi-64)  ; SRFI-64 provides a framework for writing unit tests in Scheme
             (symbol-table)   ; Import the symbol-table module to test
             (rnrs bytevectors))  ; Import bytevectors module

;; Define `little` as an alias for `endianness little`
(define little (endianness little))

;; Convert a bytevector to a list of its elements for debugging purposes
(define (bytevector->list bv)
  (let loop ((i 0) (lst '()))
    (if (= i (bytevector-length bv))
        (reverse lst)
        (loop (+ i 1) (cons (bytevector-u8-ref bv i) lst)))))

;; Test make-symbol-table function
(test-begin "make-symbol-table")

(test-assert "symbol table is a hash-table"
  (hash-table? (make-symbol-table)))

(test-end)

;; Test add-symbol! and get-symbol functions
(test-begin "add-symbol! and get-symbol")

(let ((table (make-symbol-table)))
  (add-symbol! table 'symbol1 #x1000)
  (test-assert "symbol1 should be in the table with the correct address"
    (= (get-symbol table 'symbol1) #x1000))

  (test-assert "non-existent symbol should return #f"
    (not (get-symbol table 'symbol2))))

(test-end)

(define (test-create-dynamic-symbol-table-edge-cases)
  (display "Testing create-dynamic-symbol-table edge cases...\n")
  
  (define (check-stored-value table offset expected-value)
    (let* ((raw-bytes (let ((raw-content '()))
                        (do ((i 0 (+ i 1)))
                            ((= i 8) (reverse raw-content))
                          (set! raw-content 
                                (cons (bytevector-u8-ref table (+ offset i))
                                      raw-content)))))
           (stored-value (bytevector-u64-ref table offset (endianness little))))
      (display (format #f "Stored value: ~x\n" stored-value))
      (display (format #f "Expected value: ~x\n" expected-value))
      (display (format #f "Raw bytevector content: ~a\n" raw-bytes))
      (= stored-value expected-value)))
  
  ;; Test with maximum address value
  (let* ((max-address #xFFFFFFFFFFFFFFFF)
         (symbol-addresses `((test . ,max-address)))
         (table (create-dynamic-symbol-table symbol-addresses)))
    (if (check-stored-value table 32 max-address)
        (display "Max address test passed\n")
        (display "Max address test failed\n")))
  
  ;; Test endianness
  (let* ((test-address #x0102030405060708)
         (symbol-addresses `((test . ,test-address)))
         (table (create-dynamic-symbol-table symbol-addresses)))
    (if (check-stored-value table 32 test-address)
        (display "Endianness test passed\n")
        (display "Endianness test failed\n"))))

;; Run the test
(test-create-dynamic-symbol-table-edge-cases)

;; Test create-symbol-table function
(test-begin "create-symbol-table")

(let ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000))))
  (let ((sym-table (create-symbol-table symbol-addresses)))
    (test-assert "symbol table bytevector should be of the correct size"
      (= (bytevector-length sym-table) (* (+ (length symbol-addresses) 1) 24)))))

(test-end)

;; Additional Test: create-symbol-entry
(test-begin "create-symbol-entry")

(let ((entry (make-symbol-entry 'symbol1 #x1000 0 0 1 0)))
  (display entry) ;; Debugging output to inspect entry

  ;; Test the record accessors
  (test-assert "symbol-entry should have the correct name"
    (eq? (symbol-entry-name entry) 'symbol1))
  (test-assert "symbol-entry should have the correct address"
    (= (symbol-entry-address entry) #x1000))
  (test-assert "symbol-entry should have the correct shndx"
    (= (symbol-entry-shndx entry) 1)))

(test-end)

;; Test create-dynamic-symbol-table function
(test-begin "create-dynamic-symbol-table")

(let ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000))))
  (let ((dynsym-table (create-dynamic-symbol-table symbol-addresses)))
    (test-assert "dynamic symbol table bytevector should be of the correct size"
      (= (bytevector-length dynsym-table) (* (+ (length symbol-addresses) 1) 24)))

    ;; Check the contents of the first entry in the table
    (test-assert "first entry name offset should be zero"
      (= (bytevector-u32-ref dynsym-table 0 little) 0))

    (test-assert "first entry info should be zero"
      (= (bytevector-u8-ref dynsym-table 4) 0))))

(test-end)


(test-begin "create-dynamic-symbol-table-edge-cases")

;; Test with maximum address value
(let ((symbol-addresses '((symbol1 . #xffffffffffffffff))))
  (let ((dynsym-table (create-dynamic-symbol-table symbol-addresses)))
    (let ((stored-value (bytevector-u64-ref dynsym-table 32 little)))
      (display (format #f "Stored value: ~x~%" stored-value))
      (display (format #f "Expected value: ffffffffffffffff~%"))
      (display (format #f "Raw bytevector content: ~a~%" (bytevector->list dynsym-table)))
      (test-assert "dynamic symbol table should handle maximum address value"
        (= stored-value #xffffffffffffffff)))))

;; Test for endianness with a known pattern
(let ((symbol-addresses '((symbol1 . #x0102030405060708))))
  (let ((dynsym-table (create-dynamic-symbol-table symbol-addresses)))
    (let ((stored-value (bytevector-u64-ref dynsym-table 32 little)))
      (display (format #f "Stored value: ~x~%" stored-value))
      (display (format #f "Expected value: 102030405060708~%"))
      (display (format #f "Raw bytevector content: ~a~%" (bytevector->list dynsym-table)))
      (test-assert "dynamic symbol table should handle endianness correctly"
        (= stored-value #x0102030405060708)))))

(test-end)

;; Test create-hash-section function
(test-begin "create-hash-section")

(let ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000))))
  (let ((dynsym-table (create-dynamic-symbol-table symbol-addresses))
        (hash-section (create-hash-section (create-dynamic-symbol-table symbol-addresses))))
    (test-assert "hash section bytevector should be of the correct size"
      (= (bytevector-length hash-section) (+ 8 (* 4 1) (* 4 (+ (length symbol-addresses) 1)))))

    ;; Adjusted tests to check the actual values in the hash section
    (test-assert "number of buckets should be 1"
      (= (bytevector-u32-ref hash-section 0 little) 1))

    (test-assert "number of chains should match the number of symbols"
      (= (bytevector-u32-ref hash-section 4 little) (+ (length symbol-addresses) 1)))))

(test-end)

;; Additional Test: Alignment and Padding in Symbol Table
(test-begin "alignment-and-padding-in-symbol-table")

(let ((symbol-addresses '((symbol1 . #x1000) (symbol2 . #x2000))))
  (let ((sym-table (create-symbol-table symbol-addresses)))
    (test-assert "symbol table should have correct alignment for second entry"
      (= (bytevector-u64-ref sym-table 24 little) #x1000))
    (test-assert "symbol table should have correct alignment for third entry"
      (= (bytevector-u64-ref sym-table 48 little) #x2000))))

(test-end)
