.data

    .global heap_end
heap_end:
    .long 0

.text

    .code32
    .align 4
    .global call_scheme
call_scheme:
    movl %esp, %eax
    pushl %ebx
    pushl %esi
    pushl %edi
    pushl %ebp
    movl $0, %esi       # set cp to 0
    movl 8(%eax), %edi  # heap
    movl 4(%eax), %ebp  # stack
    movl $return_from_scheme, (%ebp)
    jmp _scheme_entry

    .align 4
return_from_scheme:
    popl %ebp
    popl %edi
    popl %esi
    popl %ebx
    ret
