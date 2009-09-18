#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

#define ws          4

#define TAG(x) ((x) & mask)
#define UNTAG(x) ((x) & (~mask))
#define IMMTAG(x) ((x) & imm_mask)

#define VECTORLENGTH(x) (*((PTR *)UNTAG(x)) >> (tag_len+1))
#define VECTORDATA(x) ((PTR *)UNTAG(x) + 1)

#define default_heap_size (4*10000)
#define default_stack_size (4*10000)

extern PTR call_scheme();
static void gc_copy_forward(unsigned int *cell);

unsigned int *points_to(unsigned int *x) { return (unsigned int *)UNTAG(*x); }

void set_forward(unsigned int *x, unsigned int *to) { *points_to(x) = ((unsigned int)to) | 0x1; }
int forwarded(unsigned int *x) { return (*points_to(x) & 0x1); }

unsigned int align(unsigned int n) { return (n+1) & ~1; } /* 2n alignment */

struct rootset {
    unsigned int usedregs;
    char *stack_top;
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

static char *stack_bottom;

static unsigned int gc_space_size;
extern char *heap_end;
static char *gc_cur_space;
static char *gc_to_space;

void gc_initialize(unsigned int heap_size)
{
    gc_space_size = heap_size;
    gc_cur_space = calloc(1, gc_space_size);
    gc_to_space = calloc(1, gc_space_size);
    heap_end = gc_cur_space + gc_space_size;

    if (TAG((unsigned int)gc_cur_space) != 0 || TAG((unsigned int)gc_to_space) != 0) {
        printf("memory not aligned\n"); exit(1);
    }
}

static char *gc_scan;
static char *gc_free;

void gc_walk_roots(struct rootset *root)
{
    /*printf("gc_walk_roots\n");*/
    if (root->usedregs != 0) {
        printf("not implemented\n"); exit(1);
    }
    /* registers */
    /*printf("walk registers\n");*/
    root->regs.sym.cp |= proc_tag;                  /* cp is untagged. tag it temporarily. */
    gc_copy_forward(&root->regs.sym.cp);
    root->regs.sym.cp = UNTAG(root->regs.sym.cp);
    /* stack */
    /* TODO: walk frames over the top frame */
    /*printf("\nwalk stack\n");*/
    unsigned int *p = (unsigned int *)stack_bottom;
    p++; /* skip return code pointer */
    for (; p < (unsigned int *)root->stack_top; p++)
        gc_copy_forward(p);
    /*printf("\nend gc_walk_roots\n");*/
}

/* Cheney collector */
void gc_copy_forward(unsigned int *cell)
{
    unsigned int tag = TAG(*cell);
    if (tag == number_tag || tag == immed_tag) { return; }  /* number & immediate -> ignore */
    unsigned int *obj = points_to(cell);
    if (obj == 0) { return; }                               /* null pointer -> ignore */
    if (forwarded(cell)) {  /* update pointer with forwarding info */
        /*printf("U%c", ((tag==proc_tag)? 'c':'v'));*/
        *cell = ((unsigned int)points_to(obj)) | tag;
    } else {                /* copy and forward object */
        /*printf("C%c", ((tag==proc_tag)? 'c':'v'));*/
        unsigned int len = 1 + ((tag == proc_tag)? 1 : 0) + VECTORLENGTH(*cell);
        unsigned int size = align(len) * ws;
        memcpy(gc_free, obj, size);
        set_forward(cell, (unsigned int *)gc_free);
        *cell = ((unsigned int)gc_free) | tag;
        gc_free += size;
    }
}

static char *check_begin;
static char *check_end;

static void check(unsigned int x)
{
    printf("%#x\n", x);
    switch (TAG(x)) {
    case vector_tag:
    case proc_tag:
        {
            char *obj = (char *)UNTAG(x);
            if (!(obj >= check_begin && obj < check_end))
                printf("out of range\n");
            break;
        }
    }
}

/* duplicated */
static void gc_checker(struct rootset *root)
{
    printf("gc_checker\n");
    /* registers */
    printf("registers\n");
    check(root->regs.sym.cp | proc_tag);
    /* stack */
    printf("stack\n");
    unsigned int *p = (unsigned int *)stack_bottom;
    p++; /* skip return code pointer */
    for (; p < (unsigned int *)root->stack_top; p++)
        check(*p);
    /* heap */
    printf("heap\n");
    char *scan = check_begin;
    while (scan < check_end) {
        unsigned int *obj = (unsigned int *)scan;
        unsigned int tag = TAG(*obj >> 1);
        switch (tag) {
        case number_tag:
        case immed_tag:
            printf("scan incorrect, unboxed type. val=%#x, obj=%p, scan=%p, check_end=%p\n", *obj, obj, scan, check_end); *((unsigned int *)0) = 0;
            break;
        case vector_tag:
        case proc_tag:
            {
                unsigned int i, size = *obj >> (tag_len+1);
                unsigned int offset = 1 + ((tag == proc_tag)? 1 : 0);
                printf("%s, size=%d\n", ((tag == proc_tag)? "proc" : "vector"), size);
                for (i=0; i<size; i++)
                    check(obj[i+offset]);
                scan += align(size + offset) * ws;
            }
            break;
        }
    }
    printf("end gc_checker\n");
}

static void print_info(struct rootset *root)
{
    int i;
    for (i=0; i<6; i++) {
        printf("regs[%d]=%#x\n", i, root->regs.ind[i]);
    }
    printf("stack_bottom=%p\n", stack_bottom);
    printf("stack_top=%p\n", root->stack_top);
    printf("gc_cur_space=%p\n", gc_cur_space);
    printf("ap=%#x\n", root->regs.sym.ap);
    printf("heap_end=%p\n", heap_end);
}

void gc_collect(struct rootset *root)
{
    /*printf(";; gc_collect called\n");
    print_info(root);
    check_begin = gc_cur_space;
    check_end = (char *)root->regs.sym.ap;
    printf("first check\n");
    gc_checker(root);
    memset(gc_to_space, 0, gc_space_size);*/
    gc_scan = gc_free = gc_to_space;
    gc_walk_roots(root);
    while (gc_scan < gc_free) {
        unsigned int *obj = (unsigned int *)gc_scan;
        unsigned int tag = TAG(*obj >> 1);
        switch (tag) {
        case number_tag:
        case immed_tag:
            printf("scan incorrect, unboxed type.\n"); *((unsigned int *)0) = 0;
            break;
        case vector_tag:
        case proc_tag:
            {
                unsigned int i, size = *obj >> (tag_len+1);
                unsigned int offset = 1 + ((tag == proc_tag)? 1 : 0);
                for (i=0; i<size; i++)
                    gc_copy_forward(&obj[i+offset]);
                gc_scan += align(size + offset) * ws;
            }
            break;
        }
    }
    /*printf("\nswitch roles of gc spaces\n");*/
    char *temp = gc_to_space;
    gc_to_space = gc_cur_space;
    gc_cur_space = temp;
    root->regs.sym.ap = (unsigned int)gc_free;
    heap_end = gc_cur_space + gc_space_size;
    /*print_info(root);
    check_begin = gc_cur_space;
    check_end = gc_free;
    printf("second check\n");
    gc_checker(root);*/
}

int main(int argc, char *argv[])
{
    setbuf(stdout, NULL); /* for debug */
    stack_bottom = calloc(1, default_stack_size);
    gc_initialize(default_heap_size);

    print(call_scheme((PTR)stack_bottom,(PTR)gc_cur_space));

    printf("\n");
    return 0;
}

print(PTR x)
{
    switch (TAG(x)) {
    case number_tag:
        printf("%ld", x/(mask+1)); break;
    case immed_tag:
        switch (IMMTAG(x)) {
        case bool_tag:
            printf((x>>imm_tag_len) ? "#t" : "#f"); break;
        case null_tag:
            printf("()"); break;
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
