(use-modules (system foreign))

(define-module (buffer test)
  #:use-module (system foreign)
  #:export (get-buffer1 get-result))

;; Load the shared library and initialize
(load-extension "./liboutput.so" "perform_operations")

(define lib (dynamic-link "./liboutput.so"))
(define result-ptr (dynamic-pointer "result" lib))
(define buffer1-ptr (dynamic-pointer "buffer1" lib))
(define buffer2-ptr (dynamic-pointer "buffer2" lib))
(define multiplier-ptr (dynamic-pointer "multiplier" lib))

(define (get-result)
  (pointer->bytevector result-ptr 32)) ; Assuming result is a 32-byte vector

(define (get-buffer1)
  (pointer->bytevector buffer1-ptr 32)) ; Assuming result is a 32-byte vector

(define (get-buffer2)
  (pointer->bytevector buffer2-ptr 32)) ; Assuming result is a 32-byte vector

(define (get-multiplier)
  (pointer->bytevector multiplier-ptr 32)) ; Assuming result is a 32-byte vector

(define perform-operations
  (pointer->procedure void
                      (dynamic-func "perform_operations" lib)
                      '()))


(display "Calling perform_operations...\n")
(perform-operations)
;; Now call the functions to get buffer1 and result values
(display "result value: ")
(display (get-result))
(newline)
(display "buffer1 value: ")
(display (get-buffer1))
(newline)
(display "buffer2 value: ")
(display (get-buffer2))
(newline)
(display "multiplier value: ")
(display (get-multiplier))
(newline)
