.data
    .align 32
buffer1:
    .byte 0, 0, 128, 63, 0, 0, 0, 64, 0, 0, 64, 64, 0, 0, 128, 64
    .byte 0, 0, 160, 64, 0, 0, 192, 64, 0, 0, 224, 64, 0, 0, 0, 65

    .align 32
buffer2:
    .byte 0, 0, 0, 63, 0, 0, 64, 63, 0, 0, 128, 63, 0, 0, 160, 63
    .byte 0, 0, 192, 63, 0, 0, 224, 63, 0, 0, 0, 64, 0, 0, 16, 64

    .align 32
multiplier:
    .float 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0

result:
    .zero 32

.globl perform_operations
.globl buffer1
.globl buffer2
.globl multiplier
.globl result

.text
perform_operations:
    push %rbp
    mov %rsp, %rbp
    
    # Load addresses using GOTPCREL for PIC
    lea buffer1@GOTPCREL(%rip), %rdi
    lea buffer2@GOTPCREL(%rip), %rsi
    lea result@GOTPCREL(%rip), %rdx
    lea multiplier@GOTPCREL(%rip), %r8
    
    # Dereference GOT entries
    mov (%rdi), %rdi
    mov (%rsi), %rsi
    mov (%rdx), %rdx
    mov (%r8), %r8
    
    # SIMD operations (using unaligned loads/stores)
    vmovaps (%rdi), %ymm0
    vmovaps (%rsi), %ymm1
    vaddps %ymm1, %ymm1, %ymm2
    vmovaps (%r8), %ymm3
    vfmadd132ps %ymm0, %ymm3, %ymm2
    vmovaps %ymm2, (%rdx)
    
    # XOR operation
    vxorps %ymm1, %ymm2, %ymm2
    vmovaps %ymm2, (%rdx)
    
    # End of function
    xor %eax, %eax
    pop %rbp
    ret
