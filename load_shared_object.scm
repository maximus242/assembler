;; load_shared_object.scm
(define-module (my-extension test)
  #:export (main))

;; Get the directory of the current script
(define current-directory
  (dirname (canonicalize-path (car (command-line)))))

;; Load the shared object from the current directory
(load-extension (string-append current-directory "/output.so") "main")
