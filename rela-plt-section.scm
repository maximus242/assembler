(define-module (rela-plt-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (create-rela-plt-section))

(define (create-rela-entry offset sym-index type)
  (let ((entry (make-bytevector 24 0)))  ; Each RELA entry is 24 bytes
    (bytevector-u64-set! entry 0 offset (endianness little))  ; r_offset
    (bytevector-u64-set! entry 8 
                         (logior (ash (if (number? sym-index) sym-index 0) 32) 
                                 type) 
                         (endianness little))  ; r_info
    (bytevector-u64-set! entry 16 0 (endianness little))  ; r_addend
    entry))

(define (create-rela-plt-section function-labels got-plt-offset dynsym-indices)
  (let* ((num-entries (length function-labels))
         (rela-plt-size (* num-entries 24))
         (rela-plt (make-bytevector rela-plt-size 0)))
    
    (let loop ((i 0)
               (labels function-labels))
      (if (null? labels)
          rela-plt
          (let* ((function-pair (car labels))
                 (function-name (car function-pair))
                 (raw-sym-index (hash-ref dynsym-indices function-name))
                 (sym-index (+ raw-sym-index 1))
                 (entry (create-rela-entry 
                         (+ got-plt-offset (* (+ i 3) 8))  ; +3 because first 3 GOT entries are reserved
                         (or sym-index 0)  ; Use 0 if sym-index is #f
                         7)))  ; 7 is R_X86_64_JUMP_SLOT
            ;; Logging added here:
            (format #t "Processing function: ~a~%" function-name)
            (if sym-index
                (format #t "  Found symbol index: ~a for function ~a~%" sym-index function-name)
                (format #t "  Warning: No symbol index found for function ~a~%" function-name))
            (bytevector-copy! entry 0 rela-plt (* i 24) 24)
            (loop (+ i 1) (cdr labels)))))))

(define (print-rela-plt-section rela-plt function-labels)
  (let ((size (bytevector-length rela-plt)))
    (format #t "RELA.PLT Section (size: ~a bytes):~%" size)
    (do ((i 0 (+ i 24))
         (labels function-labels (cdr labels)))
        ((or (>= i size) (null? labels)))
      (let ((offset (bytevector-u64-ref rela-plt i (endianness little)))
            (info (bytevector-u64-ref rela-plt (+ i 8) (endianness little)))
            (addend (bytevector-u64-ref rela-plt (+ i 16) (endianness little))))
        (format #t "  ~a: offset=0x~16,'0x, info=0x~16,'0x, addend=0x~16,'0x~%"
                (car labels) offset info addend)))))
