.data
    .align 32
buffer1:
    .byte 0, 0, 128, 63  # 1.0
    .byte 0, 0, 0, 64    # 2.0
    .byte 0, 0, 64, 64   # 3.0
    .byte 0, 0, 128, 64  # 4.0
    .byte 0, 0, 160, 64  # 5.0
    .byte 0, 0, 192, 64  # 6.0
    .byte 0, 0, 224, 64  # 7.0
    .byte 0, 0, 0, 65    # 8.0

    .align 32
buffer2:
    .byte 0, 0, 0, 65    # 8.0
    .byte 0, 0, 224, 64  # 7.0
    .byte 0, 0, 192, 64  # 6.0
    .byte 0, 0, 160, 64  # 5.0
    .byte 0, 0, 128, 64  # 4.0
    .byte 0, 0, 64, 64   # 3.0
    .byte 0, 0, 0, 64    # 2.0
    .byte 0, 0, 128, 63  # 1.0

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
    lea multiplier@GOTPCREL(%rip), %rax

    # Dereference GOT entries to get the actual addresses
    mov (%rdi), %rdi     # Now %rdi contains the actual address of buffer1
    mov (%rsi), %rsi     # Now %rsi contains the actual address of buffer2
    mov (%rdx), %rdx     # Now %rdx contains the actual address of result
    mov (%rax), %rax     # Now %rax contains the actual address of multiplier
    
    # Addition
    vmovaps (%rdi), %ymm0
    vmovaps (%rsi), %ymm1
    vaddps %ymm1, %ymm0, %ymm0
    vmovaps %ymm0, (%rdx)
   
    # Multiplication
    vxorps %ymm1, %ymm1, %ymm1
    vmovaps (%rax), %ymm2
    vfmadd132ps %ymm0, %ymm1, %ymm2
    vmovaps %ymm0, (%rdx)


    # End of function
    xor %rax, %rax
    pop %rbp
    ret
