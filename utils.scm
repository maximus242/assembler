(define-module (utils)
  #:use-module (rnrs bytevectors)
  #:export (align-to
            bytevector-append))

(define (align-to value alignment)
  (* (ceiling (/ value alignment)) alignment))

; Add this definition near the top of the file, after the module definition
(define (bytevector-append . bvs)
  (let* ((total-length (apply + (map bytevector-length bvs)))
         (result (make-bytevector total-length)))
    (let loop ((offset 0)
               (bvs bvs))
      (if (null? bvs)
          result
          (let ((bv (car bvs)))
            (bytevector-copy! bv 0 result offset (bytevector-length bv))
            (loop (+ offset (bytevector-length bv)) (cdr bvs)))))))
