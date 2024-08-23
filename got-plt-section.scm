(define-module (got-plt-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (create-got-plt-section))

(define (create-got-plt-section function-labels dynamic-addr plt-addr)
  (let* ((num-entries (length function-labels))
         (got-plt-size (* (+ num-entries 3) 8))  ; 3 reserved entries + num_entries
         (got-plt (make-bytevector got-plt-size 0)))
    
    ; First entry points to the dynamic section
    (bytevector-u64-set! got-plt 0 dynamic-addr (endianness little))
    
    ; Second entry is reserved (set to 0)
    (bytevector-u64-set! got-plt 8 0 (endianness little))
    
    ; Third entry is reserved (set to 0)
    (bytevector-u64-set! got-plt 16 0 (endianness little))
    
    ; Remaining entries point back to the PLT (will be overwritten by dynamic linker)
    (do ((i 3 (1+ i)))
        ((= i (+ num-entries 3)))
      (bytevector-u64-set! got-plt (* i 8) (+ plt-addr (* i 16)) (endianness little)))
    
    got-plt))

(define (print-got-plt-section got-plt function-labels)
  (let ((size (bytevector-length got-plt)))
    (format #t "GOT.PLT Section (size: ~a bytes):~%" size)
    (format #t "  Reserved entries:~%")
    (do ((i 0 (+ i 8)))
        ((= i 24))
      (let ((value (bytevector-u64-ref got-plt i (endianness little))))
        (format #t "    Entry ~a: 0x~16,'0x~%" (/ i 8) value)))
    (format #t "  Function entries:~%")
    (do ((i 24 (+ i 8))
         (labels function-labels (cdr labels)))
        ((or (>= i size) (null? labels)))
      (let ((value (bytevector-u64-ref got-plt i (endianness little))))
        (format #t "    ~a: 0x~16,'0x~%" (caar labels) value)))))
