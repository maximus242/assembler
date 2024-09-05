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

(define *version-string* "VERS_1.0")

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
  (+ 1 (string-length *version-string*) 1
     (hash-fold 
       (lambda (key value acc) 
         (+ acc (string-length (symbol->string key)) 1))
       0 
       symbol-table)))

(define (write-initial-data string-table opts)
  (let ((initial-offset (assoc-ref opts 'initial-string-offset)))
    (bytevector-copy! (string->utf8 *version-string*) 0 string-table initial-offset (string-length *version-string*))
    (bytevector-u8-set! string-table (+ initial-offset (string-length *version-string*)) 0)
    (+ initial-offset (string-length *version-string*) 1)))

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

         (define (process-table symbol-table table string-table opts initial-str-offset symbol-addresses)
           (let* ((symbols (hash-map->list cons symbol-table))
                  (sorted-symbols 
                    (sort symbols
                          (lambda (a b)
                            (let ((name-a (symbol->string (car a)))
                                  (name-b (symbol->string (car b)))
                                  (index-a (list-index (lambda (addr) (equal? (car addr) (car a))) symbol-addresses))
                                  (index-b (list-index (lambda (addr) (equal? (car addr) (car b))) symbol-addresses)))
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
                        (is-section (char=? (string-ref name 0) #\.))
                        (is-dynamic (equal? name "_DYNAMIC@LOCAL"))
                        (is-got-local (equal? name "_GLOBAL_OFFSET_TABLE_@LOCAL"))
                        (clean-name (if is-local
                                      (string-trim-suffix name "@LOCAL")
                                      name))
                        (name-bytes (string->utf8 clean-name))
                        (stb-value (if is-local
                                     (assoc-ref opts 'stb-local)
                                     (assoc-ref opts 'stb-global)))
                        (stt-func (assoc-ref opts 'stt-func))
                        (stt-object (assoc-ref opts 'stt-object))
                        (stt-section (assoc-ref opts 'stt-section))
                        (shn-text (assoc-ref opts 'shn-text))
                        (shn-data (assoc-ref opts 'shn-data))
                        (shn-dynamic (assoc-ref opts 'shn-dynamic))
                        (stt-notype (assoc-ref opts 'stt-notype))
                        (shn-got-local 16) ; Use 16 for _GLOBAL_OFFSET_TABLE_@LOCAL
                        (null-terminator-size (assoc-ref opts 'null-terminator-size))
                        (symbol-entry-size (assoc-ref opts 'symbol-entry-size))
                        (entry (make-symbol-entry str-offset address 
                                                  (logior (ash stb-value 4)
                                                          (cond
                                                            (is-section stt-section)
                                                            (is-function stt-func)
                                                            (else stt-notype)))
                                                  0
                                                  (cond
                                                    (is-got-local shn-got-local) ; Use 16 for _GLOBAL_OFFSET_TABLE_@LOCAL
                                                    (is-dynamic shn-dynamic)
                                                    (is-function shn-text)
                                                    (else shn-data))
                                                  (if is-function 0 32)))
                        (entry-offset (* index symbol-entry-size)))
                   (bytevector-copy! name-bytes 0 string-table str-offset (bytevector-length name-bytes))
                   (bytevector-u8-set! string-table (+ str-offset (bytevector-length name-bytes)) 0)
                   (write-symbol-entry! table entry-offset opts entry)
                   (loop (cdr symbols)
                         (+ index 1)
                         (+ str-offset (bytevector-length name-bytes) null-terminator-size)))))))

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
                                   (shn-data . 2)
                                   (shn-text . 1)
                                   (shn-dynamic . 5)))  ; Changed to 5 for .dynamic section
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
                                                            (assoc-ref opts 'stt-func) 
                                                            (assoc-ref opts 'stt-notype)))
                                                  0
                                                  (if is-function 
                                                    (assoc-ref opts 'shn-text) 
                                                    (assoc-ref opts 'shn-data))
                                                  (if is-function 0 32)))
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

(define* (create-hash-section dynsym-table #:optional (options '()))
         (let* ((opts (append options '((hash-header-size . 8)
                                        (hash-entry-size . 4)
                                        (symbol-entry-size . 24))))
                (nbucket 1)
                (nchain (/ (bytevector-length dynsym-table) (assoc-ref opts 'symbol-entry-size)))
                (hash-size (+ (assoc-ref opts 'hash-header-size)
                              (* (assoc-ref opts 'hash-entry-size) (+ nbucket nchain))))
                (hash-section (make-bytevector hash-size 0)))
           (bytevector-u32-set! hash-section 0 nbucket (endianness little))
           (bytevector-u32-set! hash-section 4 nchain (endianness little))
           (bytevector-u32-set! hash-section 8 0 (endianness little))
           (let loop ((i 0))
             (when (< i nchain)
               (bytevector-u32-set! hash-section 
                                    (+ (assoc-ref opts 'hash-header-size) 
                                       (* (assoc-ref opts 'hash-entry-size) i))
                                    i 
                                    (endianness little))
               (loop (+ i 1))))
           hash-section))

(define (create-version-section)
  (let* ((version-bytes (string->utf8 *version-string*))
         (version-section-size (+ (bytevector-length version-bytes) 1))
         (version-section (make-bytevector version-section-size 0)))
    (bytevector-copy! version-bytes 0 version-section 0 (bytevector-length version-bytes))
    version-section))
