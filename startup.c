#include <stdio.h>
#include <stdlib.h>

typedef long PTR;

#define number_tag  0
#define immed_tag   1
#define vector_tag  5
#define proc_tag    6

#define mask        7

#define tag_len     3

#define bool_tag    0x01
#define null_tag    0x09

#define imm_tag_len 8

#define imm_mask    0xFF

#define TAG(x) ((x) & mask)
#define UNTAG(x) ((x) & (~mask))
#define IMMTAG(x) ((x) & imm_mask)

#define VECTORLENGTH(x) (*((PTR *)UNTAG(x)) >> (tag_len+1))
#define VECTORDATA(x) ((PTR *)UNTAG(x) + 1)

#define default_heap_size (4*10000)
#define default_stack_size (4*10000)

extern PTR call_scheme();

struct rootset {
    unsigned int usedregs;
    char *stack_end;
    union {
        unsigned int ind[6];
        struct {
            unsigned int cp;
            unsigned int ap;
            unsigned int ac;
            unsigned int t1;
            unsigned int t2;
            unsigned int t3;
        } sym;
    } regs;
};

static char *stack_ptr;

static unsigned int gc_space_size;
static char *heap_ptr;
extern char *heap_end;
static char *gc_cur_space;
static char *gc_to_space;

void gc_initialize(unsigned int heap_size)
{
    gc_space_size = heap_size;
    gc_cur_space = heap_ptr = calloc(1, gc_space_size);
    gc_to_space = calloc(1, gc_space_size);
    heap_end = heap_ptr + gc_space_size;
}

void gc(struct rootset *root)
{
    int i;
    char *new_heap_ptr = calloc(1, gc_space_size);
    gc_cur_space = new_heap_ptr;

    printf(";; gc called\n");
    printf("stack_ptr=%p\n", stack_ptr);
    printf("stack_end=%p\n", root->stack_end);
    for (i=0; i<6; i++) {
        printf("regs[%d]=%#x\n", i, root->regs.ind[i]);
    }
    root->regs.sym.ap = (unsigned int)new_heap_ptr;
    heap_end = new_heap_ptr + gc_space_size;
    printf("gc_cur_space=%p\n", gc_cur_space);
    printf("heap_end=%p\n", heap_end);
}

int main(int argc, char *argv[])
{
    stack_ptr = calloc(1, default_stack_size);
    gc_initialize(default_heap_size);

    print(call_scheme((PTR)stack_ptr,(PTR)gc_cur_space));

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
    case vector_tag:
        {
            int n;
            PTR *p;
            printf("#(");
            n = VECTORLENGTH(x);
            p = VECTORDATA(x);
            if (n != 0) {
                print(*p);
                while (--n) {
                    printf(" ");
                    print(*++p);
                }
            }
            printf(")");
            break;
        }
    case proc_tag:
        printf("<procedure>", x);
        break;
    default:
        printf("#<garbage %x>", (unsigned int)x);
        break;
    }
}
