(define (add-buffers buffer1 buffer2)
  "Add two buffers byte by byte and print the result."
  (let loop ((i 0) (result '()))
    (if (>= i (min (length buffer1) (length buffer2)))
        (reverse result)
        (let ((sum (+ (list-ref buffer1 i) (list-ref buffer2 i))))
          (loop (+ i 1) (cons sum result))))))

(define (print-buffer buffer)
  "Print the raw bytes in the buffer."
  (for-each (lambda (byte)
              (format #t "~A " byte))
            buffer)
  (newline))

;; Buffers represented as lists of bytes
(define buffer1 '(0 0 128 63 0 0 0 64 0 0 64 64 0 0 128 64
                  0 0 160 64 0 0 192 64 0 0 224 64 0 0 0 65))
(define buffer2 '(0 0 0 63 0 0 64 63 0 0 128 63 0 0 160 63
                  0 0 192 63 0 0 224 63 0 0 0 64 0 0 16 64))

;; Add the buffers byte by byte and print the result
(display "Sum of buffers: ")
(print-buffer (add-buffers buffer1 buffer2))
