(define-module (note-gnu-build-id-section)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (create-note-gnu-build-id-section))

;; Helper function to check if a character is a valid hexadecimal digit
(define (hex-digit? char)
  (or (char<=? #\0 char #\9) (char<=? #\a char #\f) (char<=? #\A char #\F)))

;; Convert a hex string to a bytevector
(define (hex-string->bytevector hex-str)
  (let* ((cleaned-str (string-downcase (string-filter hex-digit? hex-str)))
         (len (/ (string-length cleaned-str) 2))
         (bv (make-bytevector len)))
    (do ((i 0 (+ i 2))
         (byte-offset 0 (+ byte-offset 1)))
        ((>= i (string-length cleaned-str)) bv)
      (let ((hex-byte (string->number (substring cleaned-str i (+ i 2)) 16)))
        (bytevector-u8-set! bv byte-offset hex-byte)))))

;; Align size to 4 bytes
(define (align-to-4 size)
  (+ size (modulo (- 4 (modulo size 4)) 4)))

(define (create-note-gnu-build-id-section build-id)
  (let* ((name "GNU\0")
         (name-bytes (string->utf8 name))
         (name-size (bytevector-length name-bytes))
         (name-size-aligned (align-to-4 name-size))
         (descriptor (hex-string->bytevector build-id))
         (descriptor-size (bytevector-length descriptor))
         (descriptor-size-aligned (align-to-4 descriptor-size))
         (total-size (+ 12 name-size-aligned descriptor-size-aligned))
         (note-section (make-bytevector total-size 0)))
    
    ;; Write name size (4 bytes, little-endian)
    (bytevector-u32-set! note-section 0 name-size (endianness little))
    
    ;; Write descriptor size (4 bytes, little-endian)
    (bytevector-u32-set! note-section 4 descriptor-size (endianness little))
    
    ;; Write type (4 bytes, little-endian) (NT_GNU_BUILD_ID is 0x3)
    (bytevector-u32-set! note-section 8 #x3 (endianness little))
    
    ;; Write the name ("GNU\0")
    (bytevector-copy! name-bytes 0 note-section 12 name-size)
    
    ;; Write the descriptor (the actual build ID)
    (bytevector-copy! descriptor 0 note-section (+ 12 name-size-aligned) descriptor-size)
    
    (format #t "Created .note.gnu.build-id section:~%")
    (format #t "  Total size: ~a bytes~%" total-size)
    (format #t "  Name size: ~a bytes (aligned: ~a bytes)~%" name-size name-size-aligned)
    (format #t "  Descriptor size: ~a bytes (aligned: ~a bytes)~%" descriptor-size descriptor-size-aligned)
    (format #t "  Build ID: ~a~%" build-id)
    
    ;; Return the bytevector representing the .note.gnu.build-id section
    note-section))
