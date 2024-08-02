(use-modules (linker))
(use-modules (rnrs bytevectors))
(use-modules (srfi srfi-1)) ; For 'map' procedure

(define (byte->hex-string byte)
  (let ((hex (number->string byte 16)))
    (if (= (string-length hex) 1)
        (string-append "0" hex)
        hex)))

(define (bytevector->hex-string bv)
  (string-join (map byte->hex-string (bytevector->u8-list bv)) " "))

(define symbol-addresses
  '((buffer1 . #x403000)
    (buffer2 . #x403020)
    (result . #x403040)
    (multiplier . #x403060)))

(define (test-link name input expected)
  (format #t "~%Test: ~a~%" name)
  (format #t "Input:    ~a~%" (bytevector->hex-string input))
  (format #t "Symbol addresses: ~a~%" symbol-addresses)
  (let ((result (link-code input symbol-addresses)))
    (format #t "Result:   ~a~%" (bytevector->hex-string result))
    (format #t "Expected: ~a~%" (bytevector->hex-string expected))
    (if (equal? result expected)
        (format #t "PASS~%")
        (begin
          (format #t "FAIL~%")
          (let loop ((i 0))
            (when (< i (bytevector-length result))
              (let ((result-byte (bytevector-u8-ref result i))
                    (expected-byte (bytevector-u8-ref expected i)))
                (when (not (= result-byte expected-byte))
                  (format #t "Mismatch at offset ~a: result=~x, expected=~x~%" i result-byte expected-byte)))
              (loop (+ i 1))))))))

;; Test 1: Simple mov.imm32 instructions
(test-link "Simple mov.imm32 instructions"
  (u8-list->bytevector
   '(#x48 #xC7 #xC7 #x00 #x00 #x00 #x00  ; mov.imm32 rdi, buffer1
     #x48 #xC7 #xC6 #x00 #x00 #x00 #x00  ; mov.imm32 rsi, buffer2
     #x48 #xC7 #xC2 #x00 #x00 #x00 #x00)) ; mov.imm32 rdx, result
  (u8-list->bytevector
   '(#x48 #xC7 #xC7 #x00 #x30 #x40 #x00  ; mov.imm32 rdi, 0x403000 (buffer1)
     #x48 #xC7 #xC6 #x20 #x30 #x40 #x00  ; mov.imm32 rsi, 0x403020 (buffer2)
     #x48 #xC7 #xC2 #x40 #x30 #x40 #x00))) ; mov.imm32 rdx, 0x403040 (result)

;; Test 2: vmovaps instruction with symbolic reference
(test-link "vmovaps with symbolic reference"
  (u8-list->bytevector
   '(#xC5 #xFC #x28 #x05 #x00 #x00 #x00 #x00)) ; vmovaps ymm0, [multiplier]
  (u8-list->bytevector
   '(#xC5 #xFC #x28 #x05 #x60 #x30 #x40 #x00))) ; vmovaps ymm0, [0x403060]

;; Test 3: Mixed instructions
(test-link "Mixed instructions"
  (u8-list->bytevector
   '(#x48 #xC7 #xC7 #x00 #x00 #x00 #x00  ; mov.imm32 rdi, buffer1
     #xC5 #xFC #x28 #x07                 ; vmovaps ymm0, [rdi]
     #xC5 #xFC #x28 #x05 #x00 #x00 #x00 #x00  ; vmovaps ymm1, [multiplier]
     #x48 #xC7 #xC2 #x00 #x00 #x00 #x00  ; mov.imm32 rdx, result
     #xC5 #xFC #x29 #x02))               ; vmovaps [rdx], ymm0
  (u8-list->bytevector
   '(#x48 #xC7 #xC7 #x00 #x30 #x40 #x00  ; mov.imm32 rdi, 0x403000 (buffer1)
     #xC5 #xFC #x28 #x07                 ; vmovaps ymm0, [rdi]
     #xC5 #xFC #x28 #x05 #x60 #x30 #x40 #x00  ; vmovaps ymm1, [0x403060] (multiplier)
     #x48 #xC7 #xC2 #x40 #x30 #x40 #x00  ; mov.imm32 rdx, 0x403040 (result)
     #xC5 #xFC #x29 #x02)))              ; vmovaps [rdx], ymm0

;; Test 4: Unresolved reference (should not change)
(test-link "Unresolved reference"
  (u8-list->bytevector
   '(#x48 #xC7 #xC0 #x42 #x00 #x00 #x00)) ; mov.imm32 rax, 0x42
  (u8-list->bytevector
   '(#x48 #xC7 #xC0 #x42 #x00 #x00 #x00))) ; mov.imm32 rax, 0x42

(newline)
(display "All tests completed.\n")
