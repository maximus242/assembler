(define-module (dynamic-section)
  #:use-module (rnrs bytevectors)
  #:export (create-dynamic-section))

(define (create-dynamic-section)
  (let ((dynamic-entries (make-bytevector 16 0))) ; Minimal dynamic section
    ; Add a DT_NULL entry to terminate the dynamic section
    (bytevector-u64-set! dynamic-entries 0 0 (endianness little))
    (bytevector-u64-set! dynamic-entries 8 0 (endianness little))
    dynamic-entries))
