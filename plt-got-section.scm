(define-module (plt-got-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (create-plt-got-section))

(define (create-plt-got-entry got-offset)
  (let ((entry (make-bytevector 8 0)))  ; Each PLT.GOT entry is typically 8 bytes
    (bytevector-u8-set! entry 0 #xff)   ; jmpq
    (bytevector-u8-set! entry 1 #x25)   ; *32(%rip)
    (bytevector-u32-set! entry 2 got-offset (endianness little))
    entry))

(define (create-plt-got-section function-labels got-plt-offset)
  (let* ((num-entries (length function-labels))
         (plt-got-size (* num-entries 8))
         (plt-got (make-bytevector plt-got-size 0)))
    
    (do ((i 0 (1+ i))
         (labels function-labels (cdr labels)))
        ((null? labels) plt-got)
      (let ((entry (create-plt-got-entry (+ got-plt-offset (* (1+ i) 8)))))
        (bytevector-copy! entry 0 plt-got (* i 8) 8)))))

(define (print-plt-got-section plt-got function-labels)
  (let ((size (bytevector-length plt-got)))
    (format #t "PLT.GOT Section (size: ~a bytes):~%" size)
    (do ((i 0 (+ i 8))
         (labels function-labels (cdr labels)))
        ((or (>= i size) (null? labels)))
      (format #t "  ~a: ~a~%" 
              (caar labels)
              (bytevector->hex-string (bytevector-slice plt-got i (min (+ i 8) size)))))))

(define (bytevector->hex-string bv)
  (string-join (map (lambda (byte) (format #f "~2,'0x" byte))
                    (bytevector->u8-list bv))
               " "))

(define (bytevector-slice bv start end)
  (let ((result (make-bytevector (- end start))))
    (bytevector-copy! bv start result 0 (- end start))
    result))
