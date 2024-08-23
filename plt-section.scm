(define-module (plt-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:export (create-plt-section))

(define (create-plt-entry function-name got-plt-offset)
  (let ((entry (make-bytevector 16 0)))  ; Each PLT entry is typically 16 bytes
    (bytevector-u8-set! entry 0 #xff)   ; pushq
    (bytevector-u8-set! entry 1 #x35)   ; *32(%rip)
    (bytevector-u32-set! entry 2 got-plt-offset (endianness little))
    (bytevector-u8-set! entry 6 #xff)   ; jmpq
    (bytevector-u8-set! entry 7 #x25)   ; *32(%rip)
    (bytevector-u32-set! entry 8 (- got-plt-offset 4) (endianness little))
    entry))

(define (create-plt-section label-positions got-plt-offset)
  (let* ((function-labels (hash-fold 
                            (lambda (key value result)
                              (if (and (symbol? key) (not (eq? key 'start)))
                                  (cons (cons key value) result)
                                  result))
                            '()
                            label-positions))
         (num-entries (length function-labels))
         (plt-size (* (1+ num-entries) 16))  ; +1 for PLT0 entry
         (plt (make-bytevector plt-size 0)))
    
    ; Create PLT0 entry
    (bytevector-u8-set! plt 0 #xff)    ; pushq
    (bytevector-u8-set! plt 1 #x35)    ; *32(%rip)
    (bytevector-u32-set! plt 2 8 (endianness little))
    (bytevector-u8-set! plt 6 #xff)    ; jmpq
    (bytevector-u8-set! plt 7 #x25)    ; *32(%rip)
    (bytevector-u32-set! plt 8 12 (endianness little))
    
    ; Create other PLT entries
    (let loop ((entries function-labels) (index 1))
      (if (null? entries)
          plt
          (let* ((function-name (caar entries))
                 (entry (create-plt-entry function-name (+ got-plt-offset (* index 8))))
                 (offset (* index 16)))
            (bytevector-copy! entry 0 plt offset 16)
            (loop (cdr entries) (1+ index)))))
    
    (values plt function-labels)))

(define (print-plt-section plt function-labels)
  (let ((size (bytevector-length plt)))
    (format #t "PLT Section (size: ~a bytes):~%" size)
    (format #t "  PLT0: ~a~%" (bytevector->hex-string (bytevector-slice plt 0 16)))
    (do ((i 16 (+ i 16))
         (labels function-labels (cdr labels)))
        ((or (>= i size) (null? labels)))
      (format #t "  ~a: ~a~%" 
              (caar labels)
              (bytevector->hex-string (bytevector-slice plt i (min (+ i 16) size)))))))

(define (bytevector->hex-string bv)
  (string-join (map (lambda (byte) (format #f "~2,'0x" byte))
                    (bytevector->u8-list bv))
               " "))

(define (bytevector-slice bv start end)
  (let ((result (make-bytevector (- end start))))
    (bytevector-copy! bv start result 0 (- end start))
    result))
