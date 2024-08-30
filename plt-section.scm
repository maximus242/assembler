(define-module (plt-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 hash-table)
  #:use-module ((utils) #:select (align-to))
  #:export (create-plt-section))

(define plt-entry-size 16)
(define plt-base-address #x1080)
(define got-plt-base-address #x31a0)

(define (log-debug message . args)
  (apply format (current-error-port) (string-append "DEBUG: " message "\n") args))

(define (create-plt-main-entry)
  (log-debug "Creating main PLT entry")
  (let ((entry (make-bytevector plt-entry-size 0)))
    (bytevector-u8-set! entry 0 #xff)
    (bytevector-u8-set! entry 1 #x35)
    (bytevector-u32-set! entry 2 8 (endianness little))
    (bytevector-u8-set! entry 6 #xff)
    (bytevector-u8-set! entry 7 #x25)
    (bytevector-u32-set! entry 8 12 (endianness little))
    (bytevector-u32-set! entry 12 #x00401f0f (endianness little))
    (log-debug "Main PLT entry created: ~a" (bytevector->hex-string entry))
    entry))

(define (create-plt-function-entry index got-plt-address)
  (log-debug "Creating function PLT entry for index: ~d" index)
  (let* ((entry (make-bytevector plt-entry-size 0))
         (got-plt-offset (- got-plt-address (+ plt-base-address (* index plt-entry-size) 6))))
    (log-debug "Calculated got-plt-offset: ~x" got-plt-offset)
    ;; Jump to GOT entry
    (bytevector-u8-set! entry 0 #xff)
    (bytevector-u8-set! entry 1 #x25)
    (bytevector-u32-set! entry 2 got-plt-offset (endianness little))
    ;; Push the index (should be 1 for the first and only entry)
    (bytevector-u8-set! entry 6 #x68)
    (bytevector-u32-set! entry 7 1 (endianness little))  ; Changed from 0 to 1
    ;; Jump back to PLT start
    (bytevector-u8-set! entry 11 #xe9)
    (bytevector-u32-set! entry 12 #xffffffe0 (endianness little))
    (log-debug "Function PLT entry created: ~a" (bytevector->hex-string entry))
    entry))

(define (create-plt-section label-positions)
  (log-debug "Creating PLT section")
  (let* ((num-entries (hash-count (const #t) label-positions))
         (plt-size (* (+ num-entries 1) plt-entry-size))
         (plt (make-bytevector plt-size 0)))
    (log-debug "Number of entries: ~d, PLT size: ~d bytes" (+ num-entries 1) plt-size)
    
    ;; Create main PLT entry
    (let ((main-entry (create-plt-main-entry)))
      (bytevector-copy! main-entry 0 plt 0 plt-entry-size))
    
    ;; Create function PLT entries
    (let ((index 1))
      (hash-for-each
       (lambda (label offset)
         (log-debug "Creating PLT entry for label: ~a, offset: ~x, index: ~d" label offset index)
         (let ((entry (create-plt-function-entry index got-plt-base-address)))
           (bytevector-copy! entry 0 plt (* index plt-entry-size) plt-entry-size))
         (set! index (+ index 1)))
       label-positions))
    
    (log-debug "PLT section created: ~a" (bytevector->hex-string plt))
    plt))

(define (bytevector->hex-string bv)
  (string-join (map (lambda (byte) (format #f "~2,'0x" byte))
                    (bytevector->u8-list bv))
               " "))

(define (bytevector-slice bv start end)
  (let ((result (make-bytevector (- end start))))
    (bytevector-copy! bv start result 0 (- end start))
    result))
