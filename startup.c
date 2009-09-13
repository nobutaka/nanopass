#include <stdio.h>
#include <stdlib.h>

typedef long PTR;

typedef struct {
    void *eax;  /* 0    scratch  */
    void *ebx;  /* 4    preserve */
    void *ecx;  /* 8    scratch  */
    void *edx;  /* 12   scratch  */
    void *esi;  /* 16   preserve */
    void *edi;  /* 20   preserve */
    void *ebp;  /* 24   preserve */
    void *esp;  /* 28   preserve */
} context;

#define number_tag  0

#define mask        7

#define TAG(x) ((x) & mask)

#define default_stack_size 10000

extern PTR call_scheme();

int main(int argc, char *argv[])
{
    unsigned stack_size = default_stack_size;
    context ctxt;

    print(call_scheme(&ctxt,(PTR)malloc(4*stack_size)));

    printf("\n");
    return 0;
}

print(PTR x)
{
    switch (TAG(x)) {
    case number_tag:
        printf("%ld", x/(mask+1));
        break;
    default:
        printf("#<garbage %x>", x);
        break;
    }
}
