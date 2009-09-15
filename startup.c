#include <stdio.h>
#include <stdlib.h>

typedef long PTR;

#define number_tag  0
#define immed_tag   1
#define proc_tag    6

#define mask        7

#define bool_tag    0x01
#define null_tag    0x09

#define imm_tag_len 8

#define imm_mask    0xFF

#define TAG(x) ((x) & mask)
#define IMMTAG(x) ((x) & imm_mask)

#define default_heap_size 10000
#define default_stack_size 10000

extern PTR call_scheme();

int main(int argc, char *argv[])
{
    unsigned heap_size = default_heap_size;
    unsigned stack_size = default_stack_size;

    print(call_scheme((PTR)malloc(4*stack_size),(PTR)malloc(4*heap_size)));

    printf("\n");
    return 0;
}

print(PTR x)
{
    switch (TAG(x)) {
    case number_tag:
        printf("%ld", x/(mask+1));
        break;
    case immed_tag:
        switch (IMMTAG(x)) {
        case bool_tag:
            printf((x>>imm_tag_len) ? "#t" : "#f");
            break;
        case null_tag:
            printf("()");
            break;
        }
        break;
    case proc_tag:
        printf("<procedure>", x);
        break;
    default:
        printf("#<garbage %x>", x);
        break;
    }
}

void gc(int *array)
{
    int i;
    printf("gc called\n");
    for (i=0; i<3; i++) {
        printf("%x\n", array[i]);
    }
}
