.section    ".text"

    .code32
    .align 4
    .global call_scheme
call_scheme:
    movl 4(%esp), %ecx  # ctxt
    movl %ebx, 4(%ecx)
    movl %esi, 16(%ecx)
    movl %edi, 20(%ecx)
    movl %ebp, 24(%ecx)
    movl %esp, 28(%ecx)
    movl 12(%esp), %edi # heap
    movl 8(%esp), %ebp  # stack
    movl %ecx, (%ebp)   # push context address to our stack
    addl $4, %ebp
    movl $return_from_scheme, (%ebp)
    jmp _scheme_entry

    .align 4
return_from_scheme:
    subl $4, %ebp
    movl (%ebp), %ecx
    movl 4(%ecx), %ebx
    movl 16(%ecx), %esi
    movl 20(%ecx), %edi
    movl 24(%ecx), %ebp
    movl 28(%ecx), %esp
    ret
