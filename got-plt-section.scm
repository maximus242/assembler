(define-module (got-plt-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (create-got-plt-section))

(define (create-got-plt-section function-labels dynamic-addr plt-addr)
  (let* ((num-entries 0)
         (got-plt-size (* (+ num-entries 3) 8))  ; 3 reserved entries + num_entries
         (got-plt (make-bytevector got-plt-size 0)))
    
    ; First entry points to the dynamic section (dynamic-addr)
    (bytevector-u64-set! got-plt 0 dynamic-addr (endianness little))
    
    ; Second and third entries are reserved (set to 0)
    (bytevector-u64-set! got-plt 8 0 (endianness little))
    (bytevector-u64-set! got-plt 16 0 (endianness little))
    
    ; Ensure the rest of the entries are correct
    (do ((i 3 (1+ i)))
        ((= i (+ num-entries 3)))
      ; Each GOT entry points to the corresponding PLT entry (plt-addr + 16*i)
      (bytevector-u64-set! got-plt (* i 8) (+ plt-addr (* i 16)) (endianness little)))
    
    got-plt))

(define (print-got-plt-section got-plt)
  (let ((size (bytevector-length got-plt)))
    (format #t "GOT.PLT Section (size: ~a bytes):~%" size)
    (do ((i 0 (+ i 8)))
        ((>= i size))
      (let ((value (bytevector-u64-ref got-plt i (endianness little))))
        (format #t "  0x~4,'0x: 0x~16,'0x~%" i value)))))
