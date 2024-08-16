;; load_shared_object.scm
(define-module (my-extension test)
  #:export (main))

;; Get the directory of the current script
(define current-directory
  (dirname (current-filename)))

(display "Current directory: ")
(display current-directory)
(newline)

;; Use absolute path to load the shared object
(display "Attempting to load: /tmp/output.so\n")
(load-extension "/tmp/output.so" "main")
