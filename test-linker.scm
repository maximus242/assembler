(use-modules (linker))
(use-modules (rnrs bytevectors))
(use-modules (srfi srfi-1)) ; For 'map' procedure
(use-modules (ice-9 hash-table)) ; For hash tables

(define (byte->hex-string byte)
  (let ((hex (number->string byte 16)))
    (if (= (string-length hex) 1)
        (string-append "0" hex)
        hex)))

(define (bytevector->hex-string bv)
  (string-join (map byte->hex-string (bytevector->u8-list bv)) " "))

(define symbol-table
  '((buffer1 . #x403000)
    (buffer2 . #x403020)
    (result . #x403040)
    (multiplier . #x403060)))

(define (test-link name input expected)
  (format #t "~%Test: ~a~%" name)
  (format #t "Input:    ~a~%" (bytevector->hex-string input))
  (format #t "Symbol table: ~a~%" symbol-table)
  (let* ((label-positions (make-hash-table))
         (reg-to-symbol-map '((0 . "buffer1")
                              (1 . "buffer2")
                              (2 . "result")
                              (3 . "multiplier")))
         (result (resolve-references input symbol-table label-positions reg-to-symbol-map)))
    (hash-set! label-positions 'label1 7)  ; Set label1 to offset 7 (after the jump and two nops)
    (format #t "Label positions: ~a~%" label-positions)
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
   '(#x48 #xC7 #xC0 #x00 #x00 #x00 #x00  ; mov.imm32 rax, buffer1
     #x48 #xC7 #xC1 #x00 #x00 #x00 #x00  ; mov.imm32 rcx, buffer2
     #x48 #xC7 #xC2 #x00 #x00 #x00 #x00)) ; mov.imm32 rdx, result
  (u8-list->bytevector
   '(#x48 #xC7 #xC0 #x00 #x30 #x40 #x00  ; mov.imm32 rax, 0x403000 (buffer1)
     #x48 #xC7 #xC1 #x20 #x30 #x40 #x00  ; mov.imm32 rcx, 0x403020 (buffer2)
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
   '(#x48 #xC7 #xC0 #x00 #x00 #x00 #x00  ; mov.imm32 rax, buffer1
     #xC5 #xFC #x28 #x00                 ; vmovaps ymm0, [rax]
     #xC5 #xFC #x28 #x05 #x00 #x00 #x00 #x00  ; vmovaps ymm1, [multiplier]
     #x48 #xC7 #xC2 #x00 #x00 #x00 #x00  ; mov.imm32 rdx, result
     #xC5 #xFC #x29 #x02))               ; vmovaps [rdx], ymm0
  (u8-list->bytevector
   '(#x48 #xC7 #xC0 #x00 #x30 #x40 #x00  ; mov.imm32 rax, 0x403000 (buffer1)
     #xC5 #xFC #x28 #x00                 ; vmovaps ymm0, [rax]
     #xC5 #xFC #x28 #x05 #x60 #x30 #x40 #x00  ; vmovaps ymm1, [0x403060] (multiplier)
     #x48 #xC7 #xC2 #x40 #x30 #x40 #x00  ; mov.imm32 rdx, 0x403040 (result)
     #xC5 #xFC #x29 #x02)))              ; vmovaps [rdx], ymm0

;; Test 4: Unresolved reference (should not change)
(test-link "Unresolved reference"
  (u8-list->bytevector
   '(#x48 #xC7 #xC0 #x42 #x00 #x00 #x00)) ; mov.imm32 rax, 0x42
  (u8-list->bytevector
   '(#x48 #xC7 #xC0 #x42 #x00 #x00 #x00))) ; mov.imm32 rax, 0x42

;; Test 5: Labels and jumps
(test-link "Labels and jumps"
  (u8-list->bytevector
   '(#xE9 #x00 #x00 #x00 #x00  ; jmp label1 (placeholder)
     #x90                      ; nop (represents some code)
     #x90                      ; nop (represents some code)
     #x48 #xC7 #xC0 #x3C #x00 #x00 #x00  ; mov rax, 60 (exit syscall)
     #x48 #x31 #xFF            ; xor rdi, rdi
     #x0F #x05))               ; syscall
  (u8-list->bytevector
   '(#xE9 #x05 #x00 #x00 #x00  ; jmp to label1 (5 bytes forward)
     #x90                      ; nop (represents some code)
     #x90                      ; nop (represents some code)
     #x48 #xC7 #xC0 #x3C #x00 #x00 #x00  ; mov rax, 60 (exit syscall)
     #x48 #x31 #xFF            ; xor rdi, rdi
     #x0F #x05)))              ; syscall

(newline)
(display "All tests completed.\n")