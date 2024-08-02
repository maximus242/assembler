(use-modules (linker)
             (utils))

(define (main args)
  (if (< (length args) 2)
      (begin
        (display "Usage: guile main.scm <output-file>\n")
        (exit 1))
      (let ((output-file (cadr args)))
        (create-shared-object code data-sections output-file symbol-addresses label-positions)
        (format #t "Shared object file created: ~a\n" output-file))))

(main (command-line))