(define-module (validate-relocations)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (validate-relocations))

(define (validate-relocations relocation-table got-offset got-size data-segment-start data-segment-end)
  (let* ((rela-entry-size 24)  ; Size of each relocation entry
         (num-entries (/ (bytevector-length relocation-table) rela-entry-size))
         (valid? #t))
    
    (do ((i 0 (+ i 1)))
        ((>= i num-entries) valid?)
      (let* ((offset (* i rela-entry-size))
             (r_offset (bytevector-u64-ref relocation-table offset (endianness little)))
             (r_info (bytevector-u64-ref relocation-table (+ offset 8) (endianness little)))
             (r_addend (bytevector-s64-ref relocation-table (+ offset 16) (endianness little))))
        
        ;; Check if r_offset is within the bounds of the GOT
        (when (or (< r_offset got-offset)
                  (>= r_offset (+ got-offset got-size)))
          (format #t "Warning: Relocation ~a: offset 0x~x is out of GOT bounds (GOT: 0x~x - 0x~x)~%"
                  i r_offset got-offset (+ got-offset got-size))
          (set! valid? #f))
        
        ;; Check if r_offset is within the data segment
;;        (when (or (< r_offset data-segment-start)
;;                  (>= r_offset data-segment-end))
;;          (format #t "Warning: Relocation ~a: offset 0x~x is out of data segment bounds (Data: 0x~x - 0x~x)~%"
;;                  i r_offset data-segment-start data-segment-end)
;;          (set! valid? #f))
        
        ;; Additional checks can be added here
        ;; For example, checking if the symbol index in r_info is valid
        ))))
