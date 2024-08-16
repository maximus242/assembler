(define-module (buffer test)
  #:export (get-buffer1))

;; Load the shared library and initialize
(load-extension "./load_buffer.so" "init_load_buffer")

;; Now call the function to get buffer1 value
(display "buffer1 value: ")
(display (get-buffer1))
(newline)
