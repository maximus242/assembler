(use-modules (dsl-parser))
(use-modules (ice-9 pretty-print))

(define (run-test name expected actual)
  (if (equal? expected actual)
      (format #t "PASS: ~a~%" name)
      (begin
        (format #t "FAIL: ~a~%" name)
        (format #t "  Expected: ~a~%" expected)
        (format #t "  Actual:   ~a~%" actual))))

(define (test-error name thunk)
  (catch #t
    (lambda ()
      (thunk)
      (format #t "FAIL: ~a (Expected error, but none occurred)~%" name))
    (lambda (key . args)
      (format #t "PASS: ~a (Error occurred as expected)~%" name))))

;; Test valid program with new instructions
(run-test "Valid program parsing with new instructions"
  '((mov.imm32 rdi buffer1)
    (mov.imm32 rsi buffer2)
    (mov.imm32 rdx result)
    (vmovaps ymm0 (mem rdi))
    (vmovaps ymm1 (mem rsi))
    (vaddps ymm2 ymm0 ymm1)
    (vmovaps ymm3 (mem multiplier))
    (vfmadd132ps ymm2 ymm2 ymm3)
    (vmovaps (mem rdx) ymm2)
    (vxorps ymm2 ymm2 ymm2)
    (vmovaps (mem rdx) ymm2)
    (mov.imm32 eax 60)
    (xor edi edi)
    (syscall))
  (parse-dsl
   '(program
     (mov.imm32 rdi buffer1)
     (mov.imm32 rsi buffer2)
     (mov.imm32 rdx result)
     (vmovaps ymm0 (rdi))
     (vmovaps ymm1 (rsi))
     (vaddps ymm2 ymm0 ymm1)
     (vmovaps ymm3 (multiplier))
     (vfmadd132ps ymm2 ymm2 ymm3)
     (vmovaps (rdx) ymm2)
     (vxorps ymm2 ymm2 ymm2)
     (vmovaps (rdx) ymm2)
     (mov.imm32 eax 60)
     (xor edi edi)
     (syscall))))

;; Test invalid instruction
(test-error "Invalid instruction"
  (lambda () (parse-dsl '(program (invalid-instruction eax 42)))))

;; Test invalid instruction format
(test-error "Invalid instruction format"
  (lambda () (parse-dsl '(program (mov)))))

;; Test empty program
(run-test "Empty program"
  '()
  (parse-dsl '(program)))

;; Test program with single instruction
(run-test "Single instruction program"
  '((ret))
  (parse-dsl '(program (ret))))

;; Test instruction with immediate value
(run-test "Instruction with immediate value"
  '((mov.imm32 eax 42))
  (parse-dsl '(program (mov.imm32 eax 42))))

;; Test instruction with register operands
(run-test "Instruction with register operands"
  '((add eax ebx))
  (parse-dsl '(program (add eax ebx))))

;; Test instruction with memory operand
(run-test "Instruction with memory operand"
  '((vmovaps ymm0 (mem rdi)))
  (parse-dsl '(program (vmovaps ymm0 (rdi)))))

;; Test label instruction
(run-test "Label instruction"
  '((label my_function)
    (mov rax 0)
    (ret))
  (parse-dsl
   '(program
     (label my_function)
     (mov rax 0)
     (ret))))

(newline)
(display "All tests completed.\n")
