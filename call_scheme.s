.data

    .globl _stack_bottom, _gc_free, _heap_end, _global_refs, context_fp
    .align 4
_stack_bottom:
    .long 0
_gc_free:
    .long 0
_heap_end:
    .long 0
_global_refs:
    .long 0
context_fp:
    .long 0

.text

    .code32
    .globl _call_scheme
_call_scheme:
    pushl %ebx
    pushl %esi
    pushl %edi
    pushl %ebp
    subl $12, %esp                      # balance the c-stack onto a 16-byte boundary for OS X
                                        # 1 return addr + 4 regs + 3 paddings = 8 words
    movl $0, %esi                       # cp=0
    movl _gc_free, %edi                 # ap
    movl _stack_bottom, %ebp            # fp
    addl $4, %ebp
    movl $0, -4(%ebp)                   # fp[-1]=size of the frame (indeterminate)
    movl $return_from_scheme, (%ebp)    # fp[0]=return address
    movl $0, %ebx                       # t1=0 (number of arguments)
    jmp _scheme_entry

return_from_scheme:
    addl $12, %esp
    popl %ebp
    popl %edi
    popl %esi
    popl %ebx
    ret

    .globl _call_closure
_call_closure:
    pushl %ebx
    pushl %esi
    pushl %edi
    pushl %ebp
    subl $12, %esp                      # 16-byte alignment
    movl _global_refs, %eax             # ac=contents of vector
    addl $-1, %eax                      # -vector-tag+ws = -1
    movl 4(%eax), %esi                  # cp=global_refs[1]
    subl $6, %esi                       # remove closure-tag
    movl _gc_free, %edi                 # ap
    movl context_fp, %ebp               # fp
    movl $return_from_closure, (%ebp)   # fp[0]=return address
    movl (%eax), %eax                   # ac=global_refs[0]
    movl %eax, 4(%ebp)                  # fp[1]=ac
    movl $1, %ebx                       # t1=1
    jmp *4(%esi)

return_from_closure:
    sarl $3, %eax                       # fixnum->int
    movl %ebp, context_fp
    addl $12, %esp
    popl %ebp
    popl %edi
    popl %esi
    popl %ebx
    ret
