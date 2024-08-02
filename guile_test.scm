(use-modules (system foreign))

(define simd-lib (dynamic-link "./libsimd_ops.so"))

(define simd-ops
  (pointer->procedure
    void
    (dynamic-func "simd_ops_wrapper" simd-lib)
    (list '* '* '* '*)))

(define (run-simd-ops buffer1 buffer2 result multiplier)
  (simd-ops
    (bytevector->pointer buffer1)
    (bytevector->pointer buffer2)
    (bytevector->pointer result)
    (bytevector->pointer multiplier)))

; Example usage
(define buffer1 (f32vector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
(define buffer2 (f32vector 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0))
(define result (make-f32vector 8 0.0))
(define multiplier (f32vector 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0))

(run-simd-ops buffer1 buffer2 result multiplier)

(display "Result: ")
(display (f32vector->list result))
(newline)
