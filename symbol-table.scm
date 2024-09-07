(define-module (symbol-table)
               #:use-module (ice-9 hash-table)
               #:use-module (rnrs bytevectors)
               #:use-module (rnrs io ports)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (string-table)
               #:export (make-symbol-table
                          add-symbol!
                          add-label-symbol!
                          get-symbol
                          symbol-in-table?
                          symbol-is-function?
                          create-symtab-and-strtab
                          create-dynsym-and-dynstr
                          create-hash-section
                          create-symbol-table
                          make-symbol-entry
                          symbol-entry-name
                          symbol-entry-address
                          create-version-section))

(define-record-type <symbol-entry>
  (make-symbol-entry name address info other shndx size)
  symbol-entry?
  (name symbol-entry-name)
  (address symbol-entry-address)
  (info symbol-entry-info)
  (other symbol-entry-other)
  (shndx symbol-entry-shndx)
  (size symbol-entry-size))

(define* (create-symbol-table symbol-addresses label-positions #:optional (options '()))
         (let ((table (make-symbol-table)))
           (add-symbols-to-table! table symbol-addresses add-symbol!)
           (add-symbols-to-table! table (or label-positions '()) add-label-symbol!)
           table))

(define (string-trim-suffix str suffix)
  (if (string-suffix? suffix str)
    (substring str 0 (- (string-length str) (string-length suffix)))
    str))

(define (make-symbol-table)
  (make-hash-table))

(define (add-symbol! table name address)
  (hash-set! table name (cons address #f)))

(define (add-label-symbol! table name address)
  (hash-set! table name (cons address #t)))

(define (get-symbol table name)
  (let ((entry (hash-ref table name #f)))
    (and entry (car entry))))

(define (symbol-in-table? table name)
  (hash-ref table name #f))

(define (symbol-is-function? table name)
  (let ((entry (hash-ref table name #f)))
    (and entry (cdr entry))))

(define (add-symbols-to-table! table symbols add-func)
  (if (hash-table? symbols)
    (hash-for-each (lambda (name address)
                     (add-func table name address))
                   symbols)
    (for-each (lambda (sym)
                (add-func table (car sym) (cdr sym)))
              symbols)))

(define (calculate-string-table-size symbol-table)
  (hash-fold 
    (lambda (key value acc) 
      (+ acc (string-length (symbol->string key)) 1))
    1  ; Start with 1 for the initial null byte
    symbol-table))

(define (write-initial-data string-table opts)
  (let ((initial-offset (assoc-ref opts 'initial-string-offset)))
    initial-offset))

(define* (create-symtab-and-strtab symbol-addresses label-positions #:optional (options '()))
         (format #t "create-symtab-and-strtab called with:~%")
         (format #t "  symbol-addresses: ~S~%" symbol-addresses)
         (format #t "  label-positions: ~S~%" label-positions)
         (format #t "  options: ~S~%" options)

         (define (merge-options default-options custom-options)
           (append custom-options default-options))

         (define (create-symbol-table addresses labels)
           (let ((table (make-symbol-table)))
             (add-symbols-to-table! table addresses add-symbol!)
             (add-symbols-to-table! table (or labels '()) add-label-symbol!)
             table))

         (define (calculate-table-size symbol-table opts)
           (let* ((symbol-entry-size (assoc-ref opts 'symbol-entry-size))
                  (symbol-count (+ (hash-count (const #t) symbol-table) 1))
                  (table-size (* symbol-count symbol-entry-size))
                  (string-table-size (calculate-string-table-size symbol-table)))
             (cons table-size string-table-size)))

         (define (initialize-table table-size string-table-size)
           (cons (make-bytevector table-size 0)
                 (make-bytevector string-table-size 0)))

         (define section-index-table
           (alist->hash-table
             '((".note.gnu.build-id" . 1)
               (".hash" . 2)
               (".dynsym" . 3)
               (".dynstr" . 4)
               (".rela.dyn" . 5)
               (".text" . 6)
               (".eh_frame" . 7)
               (".dynamic" . 8)
               (".got" . 9)
               (".got.plt" . 10)
               (".data" . 11)
               (".symtab" . 12)
               (".strtab" . 13)
               (".shstrtab" . 14))))

         (define (process-table symbol-table table string-table opts initial-str-offset symbol-addresses)
           (format #t "process-table input:~%")
           (format #t "  symbol-table: ~S~%" symbol-table)
           (format #t "  initial-str-offset: ~A~%" initial-str-offset)
           (format #t "  symbol-addresses: ~S~%" symbol-addresses)

           (let* ((symbols (hash-map->list cons symbol-table))
                  (sorted-symbols 
                    (sort symbols
                          (lambda (a b)
                            (let ((name-a (symbol->string (car a)))
                                  (name-b (symbol->string (car b)))
                                  (index-a (list-index (lambda (addr) (equal? (car addr) (car a))) symbol-addresses))
                                  (index-b (list-index (lambda (addr) (equal? (car addr) (car b))) symbol-addresses)))
                              (format #t "Comparing symbols: ~A (index ~A) and ~A (index ~A)~%" 
                                      name-a index-a name-b index-b)
                              (cond
                                ((eq? (string-contains name-a "@LOCAL")
                                      (string-contains name-b "@LOCAL"))
                                 (cond
                                   ((and index-a index-b) (< index-a index-b))
                                   (index-a #t)
                                   (index-b #f)
                                   (else (string<? name-a name-b))))
                                ((string-contains name-a "@LOCAL") #t)
                                (else #f)))))))
             (format #t "Sorted symbols: ~S~%" sorted-symbols)
             (let loop ((symbols sorted-symbols)
                        (index 1)
                        (str-offset initial-str-offset))
               (if (null? symbols)
                 (begin
                   (format #t "Finished processing symbols~%")
                   (cons table string-table))
                 (let* ((symbol (car symbols))
                        (name (symbol->string (car symbol)))
                        (value (cdr symbol))
                        (address (car value))
                        (is-function (cdr value))
                        (is-local (string-contains name "@LOCAL"))
                        (is-section (char=? (string-ref name 0) #\.))
                        (is-dynamic (equal? name "_DYNAMIC@LOCAL"))
                        (is-got-local (equal? name "_GLOBAL_OFFSET_TABLE_@LOCAL"))
                        (clean-name (if is-local
                                      (string-trim-suffix name "@LOCAL")
                                      name))
                        (name-bytes (string->utf8 clean-name)))
                   (format #t "Processing symbol: ~A~%" clean-name)
                   (format #t "  address: ~A, is-function: ~A, is-local: ~A~%" 
                           address is-function is-local)
                   (format #t "  is-section: ~A, is-dynamic: ~A, is-got-local: ~A~%" 
                           is-section is-dynamic is-got-local)
                   (let* ((stb-value (if is-local
                                       (assoc-ref opts 'stb-local)
                                       (assoc-ref opts 'stb-global)))
                          (stt-func (assoc-ref opts 'stt-func))
                          (stt-object (assoc-ref opts 'stt-object))
                          (stt-section (assoc-ref opts 'stt-section))
                          (shn-text (assoc-ref opts 'shn-text))
                          (shn-data (assoc-ref opts 'shn-data))
                          (shn-dynamic (assoc-ref opts 'shn-dynamic))
                          (stt-notype (assoc-ref opts 'stt-notype))
                          (shn-got-local 10)
                          (null-terminator-size (assoc-ref opts 'null-terminator-size))
                          (symbol-entry-size (assoc-ref opts 'symbol-entry-size))
                          (section-index (if is-section
                                           (hash-ref section-index-table clean-name 0)
                                           (cond
                                             (is-got-local shn-got-local)
                                             (is-dynamic shn-dynamic)
                                             (is-function shn-text)
                                             (else shn-data))))
                          (symbol-type (cond
                                         (is-section stt-section)
                                         (is-function stt-notype)
                                         ((or (string=? clean-name "_DYNAMIC")
                                              (string=? clean-name "_GLOBAL_OFFSET_TABLE_"))
                                          stt-object)
                                         (else stt-notype)))
                          (entry (make-symbol-entry str-offset address 
                                                    (logior (ash stb-value 4)
                                                            symbol-type)
                                                    0
                                                    section-index
                                                    (if is-function 0 0)))
                          (entry-offset (* index symbol-entry-size)))
                     (format #t "  section-index: ~A, symbol-type: ~A~%" section-index symbol-type)
                     (format #t "  entry-offset: ~A, str-offset: ~A~%" entry-offset str-offset)
                     (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
                     (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
                     (write-symbol-entry! table entry-offset opts entry)
                     (loop (cdr symbols)
                           (+ index 1)
                           (+ str-offset (bytevector-length name-bytes) null-terminator-size))))))))

         (let* ((default-options '((symbol-entry-size . 24)
                                   (st-name-offset . 0)
                                   (st-info-offset . 4)
                                   (st-other-offset . 5)
                                   (st-shndx-offset . 6)
                                   (st-value-offset . 8)
                                   (st-size-offset . 16)
                                   (null-terminator-size . 1)
                                   (initial-string-offset . 1)
                                   (stt-notype . 0)
                                   (stt-object . 1)
                                   (stt-func . 2)
                                   (stt-section . 3)
                                   (stb-global . 1)
                                   (stb-local . 0)
                                   (shn-data . 11)
                                   (shn-text . 6)
                                   (shn-dynamic . 8)))
                (opts (merge-options default-options options))
                (symbol-table (create-symbol-table symbol-addresses label-positions))
                (symbol-sizes (calculate-table-size symbol-table opts))
                (symbol-tables (initialize-table (car symbol-sizes) (cdr symbol-sizes)))
                (symbol-table-bytevector (car symbol-tables))
                (symbol-string-table (cdr symbol-tables))
                (initial-str-offset (write-initial-data symbol-string-table opts)))

           (write-symbol-entry! symbol-table-bytevector 0 opts (make-symbol-entry 0 0 0 0 0 0))

           (process-table symbol-table symbol-table-bytevector symbol-string-table opts initial-str-offset symbol-addresses)))

(define* (create-dynsym-and-dynstr dynamic-symbol-addresses 
                                   #:optional (label-positions '()) (options '()))
         (define (merge-options default-options custom-options)
           (append custom-options default-options))

         (define (create-symbol-table addresses labels)
           (let ((table (make-symbol-table)))
             (add-symbols-to-table! table addresses add-symbol!)
             (add-symbols-to-table! table (or labels '()) add-label-symbol!)
             table))

         (define (calculate-table-size symbol-table opts)
           (let* ((symbol-entry-size (assoc-ref opts 'symbol-entry-size))
                  (symbol-count (+ (hash-count (const #t) symbol-table) 1))
                  (table-size (* symbol-count symbol-entry-size))
                  (string-table-size (calculate-string-table-size symbol-table)))
             (cons table-size string-table-size)))

         (define (initialize-table table-size string-table-size)
           (cons (make-bytevector table-size 0)
                 (make-bytevector string-table-size 0)))

         (define (process-table symbol-table table string-table opts initial-str-offset dynamic-symbol-addresses)
           (let* ((symbols (hash-map->list cons symbol-table))
                  (sorted-symbols 
                    (sort symbols
                          (lambda (a b)
                            (let ((name-a (symbol->string (car a)))
                                  (name-b (symbol->string (car b)))
                                  (index-a (list-index (lambda (addr) (equal? (car addr) (car a))) dynamic-symbol-addresses))
                                  (index-b (list-index (lambda (addr) (equal? (car addr) (car b))) dynamic-symbol-addresses)))
                              (cond
                                ;; Both local or both global, sort by original order
                                ((eq? (string-contains name-a "@LOCAL")
                                      (string-contains name-b "@LOCAL"))
                                 (cond
                                   ((and index-a index-b) (< index-a index-b))
                                   (index-a #t)
                                   (index-b #f)
                                   (else (string<? name-a name-b))))  ; fallback to alphabetical order
                                ;; a is local, b is global
                                ((string-contains name-a "@LOCAL") #t)
                                ;; a is global, b is local
                                (else #f)))))))
             (let loop ((symbols sorted-symbols)
                        (index 1)
                        (str-offset initial-str-offset))
               (if (null? symbols)
                 (cons table string-table)
                 (let* ((symbol (car symbols))
                        (name (symbol->string (car symbol)))
                        (value (cdr symbol))
                        (address (car value))
                        (is-function (cdr value))
                        (is-local (string-contains name "@LOCAL"))
                        (clean-name (if is-local
                                      (string-trim-suffix name "@LOCAL")
                                      name))
                        (name-bytes (string->utf8 clean-name))
                        (entry (make-symbol-entry str-offset address 
                                                  (logior (ash (if is-local
                                                                 (assoc-ref opts 'stb-local)
                                                                 (assoc-ref opts 'stb-global))
                                                               4)
                                                          (if is-function 
                                                            (assoc-ref opts 'stt-notype) 
                                                            (assoc-ref opts 'stt-notype)))
                                                  0
                                                  (if is-function 
                                                    (assoc-ref opts 'shn-text) 
                                                    (assoc-ref opts 'shn-data))
                                                  (if is-function 0 0)))
                        (entry-offset (* index (assoc-ref opts 'symbol-entry-size))))
                   (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
                   (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
                   (write-symbol-entry! table entry-offset opts entry)
                   (loop (cdr symbols)
                         (+ index 1)
                         (+ str-offset (bytevector-length name-bytes) (assoc-ref opts 'null-terminator-size))))))))

         (let* ((default-options '((symbol-entry-size . 24)
                                   (st-name-offset . 0)
                                   (st-info-offset . 4)
                                   (st-other-offset . 5)
                                   (st-shndx-offset . 6)
                                   (st-value-offset . 8)
                                   (st-size-offset . 16)
                                   (null-terminator-size . 1)
                                   (initial-string-offset . 1)
                                   (stt-notype . 0)
                                   (stt-object . 1)
                                   (stt-func . 2)
                                   (stb-global . 1)
                                   (stb-local . 0)
                                   (shn-data . 2)
                                   (shn-text . 1)))
                (opts (merge-options default-options options))
                (dynamic-symbol-table (create-symbol-table dynamic-symbol-addresses label-positions))
                (dynamic-sizes (calculate-table-size dynamic-symbol-table opts))
                (dynamic-tables (initialize-table (car dynamic-sizes) (cdr dynamic-sizes)))
                (dynamic-table-bytevector (car dynamic-tables))
                (dynamic-string-table (cdr dynamic-tables))
                (dynamic-initial-str-offset (write-initial-data dynamic-string-table opts)))

           (write-symbol-entry! dynamic-table-bytevector 0 opts (make-symbol-entry 0 0 0 0 0 0))

           (process-table dynamic-symbol-table dynamic-table-bytevector dynamic-string-table opts dynamic-initial-str-offset dynamic-symbol-addresses)))

(define (write-symbol-entry! table entry-offset opts entry)
  (define (write-field offset size value)
    (case size
      ((1) (bytevector-u8-set! table (+ entry-offset offset) value))
      ((2) (bytevector-u16-set! table (+ entry-offset offset) value (endianness little)))
      ((4) (bytevector-u32-set! table (+ entry-offset offset) value (endianness little)))
      ((8) (bytevector-u64-set! table (+ entry-offset offset) value (endianness little)))))

  (write-field (assoc-ref opts 'st-name-offset) 4 (symbol-entry-name entry))
  (write-field (assoc-ref opts 'st-info-offset) 1 (symbol-entry-info entry))
  (write-field (assoc-ref opts 'st-other-offset) 1 (symbol-entry-other entry))
  (write-field (assoc-ref opts 'st-shndx-offset) 2 (symbol-entry-shndx entry))
  (write-field (assoc-ref opts 'st-value-offset) 8 (symbol-entry-address entry))
  (write-field (assoc-ref opts 'st-size-offset) 8 (symbol-entry-size entry)))

(define (ash n k)
  (if (>= k 0)
    (* n (expt 2 k))  ; Left shift
    (quotient n (expt 2 (- k)))))  ; Right shift

(define (arithmetic-shift n k)
  (if (>= k 0)
    (ash n k)  ; Left shift
    (let ((m (ash 1 (- k))))  ; Right shift
      (if (>= n 0)
        (quotient n m)
        (- (quotient (+ n 1) m) 1)))))

(define* (create-hash-section dynsym-table dynstr-table #:optional (options '()))
         (let* ((opts (append options '((hash-header-size . 8)
                                        (hash-entry-size . 4)
                                        (symbol-entry-size . 24))))
                (symbol-count (/ (bytevector-length dynsym-table) (assoc-ref opts 'symbol-entry-size)))
                (nbucket (next-prime (max 1 (min symbol-count 3)))) ; Adjust bucket count
                (nchain symbol-count)
                (hash-size (+ (assoc-ref opts 'hash-header-size)
                              (* (assoc-ref opts 'hash-entry-size) (+ nbucket nchain))))
                (hash-section (make-bytevector hash-size 0))
                (buckets (make-vector nbucket 0))
                (chains (make-vector nchain 0)))

           (format #t "Creating hash section:~%")
           (format #t "  Symbol count: ~a~%" symbol-count)
           (format #t "  Number of buckets: ~a~%" nbucket)
           (format #t "  Number of chains: ~a~%" nchain)
           (format #t "  Hash section size: ~a bytes~%" hash-size)

           ; Write number of buckets and chains
           (bytevector-u32-set! hash-section 0 nbucket (endianness little))
           (bytevector-u32-set! hash-section 4 nchain (endianness little))

           ; Hash each symbol and add to appropriate bucket
           (let loop ((i 1)) ; Start from 1 to skip the null symbol
             (when (< i symbol-count)
               (let* ((name (get-symbol-name dynsym-table dynstr-table i))
                      (hash (elf-hash name))
                      (bucket (modulo hash nbucket))
                      (chain-index (vector-ref buckets bucket)))
                 (format #t "Symbol ~a: name=~a, hash=~a, bucket=~a~%" i name hash bucket)
                 (if (= chain-index 0)
                   (begin
                     (vector-set! buckets bucket i)
                     (format #t "  Added to empty bucket ~a~%" bucket))
                   (let chain-loop ((prev chain-index))
                     (if (= (vector-ref chains prev) 0)
                       (begin
                         (vector-set! chains prev i)
                         (format #t "  Added to chain at index ~a~%" prev))
                       (chain-loop (vector-ref chains prev)))))
                 (vector-set! chains i 0))
               (loop (+ i 1))))

           (format #t "Bucket contents:~%")
           (let loop ((i 0))
             (when (< i nbucket)
               (format #t "  Bucket ~a: ~a~%" i (vector-ref buckets i))
               (loop (+ i 1))))

           (format #t "Chain contents:~%")
           (let loop ((i 0))
             (when (< i nchain)
               (format #t "  Chain ~a: ~a~%" i (vector-ref chains i))
               (loop (+ i 1))))

           ; Write bucket array
           (let loop ((i 0))
             (when (< i nbucket)
               (bytevector-u32-set! hash-section 
                                    (+ (assoc-ref opts 'hash-header-size) 
                                       (* (assoc-ref opts 'hash-entry-size) i))
                                    (vector-ref buckets i)
                                    (endianness little))
               (loop (+ i 1))))

           ; Write chain array
           (let loop ((i 0))
             (when (< i nchain)
               (bytevector-u32-set! hash-section 
                                    (+ (assoc-ref opts 'hash-header-size)
                                       (* (assoc-ref opts 'hash-entry-size) nbucket)
                                       (* (assoc-ref opts 'hash-entry-size) i))
                                    (vector-ref chains i)
                                    (endianness little))
               (loop (+ i 1))))

           (format #t "Hash section created successfully.~%")
           hash-section))

; Helper function to get symbol name from dynsym and dynstr tables
(define (get-symbol-name dynsym-table dynstr-table index)
  (let* ((entry-offset (* index 24)) ; 24-byte entries in dynsym
         (name-offset (bytevector-u32-ref dynsym-table entry-offset (endianness little))))
    (let loop ((i 0))
      (if (zero? (bytevector-u8-ref dynstr-table (+ name-offset i)))
        (utf8->string (bytevector-slice dynstr-table name-offset (+ name-offset i)))
        (loop (+ i 1))))))

(define (elf-hash name)
  (let loop ((h 0) (chars (string->list name)))
    (if (null? chars)
      h
      (let* ((h (logand #xffffffff (+ (arithmetic-shift h 4) (char->integer (car chars)))))
             (g (logand h #xf0000000)))
        (loop (logand #xffffffff (logxor (logand h (lognot g)) (arithmetic-shift g -24)))
              (cdr chars))))))

; Helper function to find the next prime number (unchanged)
(define (next-prime n)
  (let loop ((i (if (even? n) (+ n 1) n)))
    (if (prime? i)
      i
      (loop (+ i 2)))))

; Helper function to check if a number is prime (unchanged)
(define (prime? n)
  (if (< n 2)
    #f
    (let loop ((i 2))
      (cond ((> (* i i) n) #t)
            ((zero? (modulo n i)) #f)
            (else (loop (+ i 1)))))))

; Helper function to slice a bytevector (unchanged)
(define (bytevector-slice bv start end)
  (let* ((length (- end start))
         (result (make-bytevector length)))
    (bytevector-copy! bv start result 0 length)
    result))
