.data

    .globl _stack_bottom
_stack_bottom:
    .long 0
    .globl _gc_free
_gc_free:
    .long 0
    .globl _heap_end
_heap_end:
    .long 0

.text

    .code32
    .align 4
    .globl _call_scheme
_call_scheme:
    pushl %ebx
    pushl %esi
    pushl %edi
    pushl %ebp
    subl $12, %esp                      # balance the c-stack onto a 16-byte boundary for OS X
                                        # 1 return addr + 4 regs + 3 paddings = 8 words
    movl $0, %esi                       # set cp to 0
    movl _gc_free, %edi                 # ap
    movl _stack_bottom, %ebp            # fp
    movl $0, (%ebp)                     # size of the frame (indeterminate)
    addl $4, %ebp
    movl $return_from_scheme, (%ebp)    # set fp[0] to return address
    movl $0, %ebx                       # set t1 to 0 (number of arguments)
    jmp _scheme_entry

    .align 4
return_from_scheme:
    addl $12, %esp
    popl %ebp
    popl %edi
    popl %esi
    popl %ebx
    ret
