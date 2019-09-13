    .text
    .intel_syntax noprefix

    .globl "string-length"
"string-length":
    push rbp
    mov rbp, rsp
    mov rax, [rbp - 8]  # rax = &str + 5
    mov rax, [rax - 5]  # rax = &str
    shl rax, 3          # rax = rax * 8
    nop
    mov rbp, rsp
    pop rbp
    ret
