.data

    .globl _heap_end
_heap_end:
    .long 0

.text

    .code32
    .align 4
    .globl _call_scheme
_call_scheme:
    movl %esp, %eax                     # pointer to argument
    pushl %ebx
    pushl %esi
    pushl %edi
    pushl %ebp
    subl $12, %esp                      # balance the c-stack onto a 16-byte boundary for OS X
                                        # 1 return addr + 4 regs + 3 paddings = 8 words
    movl $0, %esi                       # set cp to 0
    movl 8(%eax), %edi                  # heap
    movl 4(%eax), %ebp                  # stack
    movl $return_from_scheme, (%ebp)    # set fp to return address
    jmp _scheme_entry

    .align 4
return_from_scheme:
    addl $12, %esp
    popl %ebp
    popl %edi
    popl %esi
    popl %ebx
    ret
