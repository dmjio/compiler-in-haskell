.data

.text

.globl ___dl_main
___dl_main:
    push %rbp
    mov %rsp, %rbp

    push %rdi
    push %rdi

    # __dlib_print_num(fib(40))
    mov $40, %rdi
    call ___dl_fib
    push %rax
    pop %rdi
    call _dlib_print_num

    # __dlib_print_char('\n')
    push $10
    pop %rdi
    call _dlib_print_char

    # return 0
    push $0
    pop %rax
    pop %rdi # add $8, %rsp
    pop %rdi # add $8, %rsp
    jmp JTAG___dl_main_0

JTAG___dl_main_0:
    pop %rbp
    ret

.globl ___dl_fib
___dl_fib:
    push %rbp
    mov %rsp, %rbp
    
    push %rdi # arg

    movq -8(%rbp), %rax
    push %rax
    push $2
    pop %rdi
    pop %rax
    cmp %rdi, %rax
    jl JTAG___dl_fib_1
    push $0
    jmp JTAG___dl_fib_2
JTAG___dl_fib_1:
    push $1
JTAG___dl_fib_2:
    pop %rax
    test %al, %al
    je JTAG___dl_fib_3 # if al == 0
    push $1
    pop %rax
    pop %rdi # add $8, %rsp # pop arg
    jmp JTAG___dl_fib_0
JTAG___dl_fib_3:
    movq -8(%rbp), %rax
    sub $1, %rax
    push %rax
    pop %rdi
    call ___dl_fib
    push %rax
    movq -8(%rbp), %rax
    sub $2, %rax
    push %rax
    pop %rdi
    call ___dl_fib
    push %rax
    pop %rdi
    pop %rax
    add %rdi, %rax
    push %rax
    pop %rax
    pop %rdi # add $8, %rsp # pop arg
    jmp JTAG___dl_fib_0

JTAG___dl_fib_0:
    pop %rbp
    ret
