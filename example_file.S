.globl perform_operations
.text
perform_operations:
    push %rbp
    mov %rsp, %rbp
    
    # Load addresses using GOTPCREL for PIC
    lea buffer1@GOTPCREL(%rip), %rdi
    lea buffer2@GOTPCREL(%rip), %rsi
    lea result@GOTPCREL(%rip), %rdx
    lea multiplier@GOTPCREL(%rip), %r8
    
    # Check for null pointers
    test %rdi, %rdi
    jz .error
    test %rsi, %rsi
    jz .error
    test %rdx, %rdx
    jz .error
    test %r8, %r8
    jz .error
    
    # Dereference GOT entries
    mov (%rdi), %rdi
    mov (%rsi), %rsi
    mov (%rdx), %rdx
    mov (%r8), %r8
    
    # SIMD operations (using unaligned loads/stores)
    vmovups (%rdi), %ymm0
    vmovups (%rsi), %ymm1
    vaddps %ymm1, %ymm1, %ymm2
    vmovups (%r8), %ymm3
    vfmadd132ps %ymm0, %ymm3, %ymm2
    vmovups %ymm2, (%rdx)
    
    # XOR operation
    vxorps %ymm1, %ymm2, %ymm2
    vmovups %ymm2, (%rdx)
    
    # End of function
    xor %eax, %eax
    pop %rbp
    ret

.error:
    mov $-1, %eax  # Return -1 to indicate error
    pop %rbp
    ret
