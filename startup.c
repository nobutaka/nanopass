#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef long Ptr;

struct RootSet {
    unsigned int usedregs;
    char *stack_top;
    union {
        Ptr ind[6];
        struct {
            Ptr cp;
            char *ap;
            Ptr ac;
            Ptr t1;
            Ptr t2;
            Ptr t3;
        } sym;
    } regs;
};

#define ws          4

#define number_tag  0
#define immed_tag   1
#define pair_tag    2
#define string_tag  3
#define symbol_tag  4
#define vector_tag  5
#define proc_tag    6
#define float_tag   7

#define mask        7
#define tag_len     3

#define bool_tag    0x01
#define null_tag    0x09
#define char_tag    0x11

#define imm_tag_len 8
#define imm_mask    0xFF

#define attr_len    (tag_len + 1)

#define default_heap_size   (4*10000)
#define default_stack_size  (4*10000)

#define PTR(ptr)    ((Ptr)(ptr))
#define TAG(ptr)    (PTR(ptr) & mask)
#define UNTAG(ptr)  (PTR(ptr) & (~mask))
#define IMMTAG(ptr) (PTR(ptr) & imm_mask)

#define OBJ(ptr)        ((Ptr *)UNTAG(ptr))
#define OBJLENGTH(ptr)  (*OBJ(ptr) >> attr_len)
#define OBJTAG(ptr)     TAG(*OBJ(ptr) >> 1)
#define CAR(ptr)        (*(OBJ(ptr) + 1))
#define CDR(ptr)        (*(OBJ(ptr) + 2))
#define STRINGDATA(ptr) ((char *)(OBJ(ptr) + 1))
#define SYMBOLNAME(ptr) (*(OBJ(ptr) + 1))
#define VECTORDATA(ptr) (OBJ(ptr) + 1)

/*#define VERBOSE*/

#ifdef VERBOSE
# define LOG(...) printf(__VA_ARGS__)
#else
# define LOG(...)
#endif

extern Ptr call_scheme(char *, char *);
extern char *heap_end;

static char *stack_bottom;
static unsigned int gc_space_size;
static char *gc_cur_space;
static char *gc_to_space;
static char *gc_scan;
static char *gc_free;

static void error_exit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(1);
}

static unsigned int align(unsigned int n) { return (n+1) & ~1; } /* 2n alignment */

static unsigned int object_size(Ptr *obj)
{
    unsigned int tag = OBJTAG(obj);
    unsigned int len = OBJLENGTH(obj);
    unsigned int full_len = 1;
    if (tag == string_tag) {
        full_len += ((len+3) / 4);
    } else if (tag == proc_tag) {
        full_len += 1+len;
    } else {                /* pair or symbol or vector */
        full_len += len;
    }
    return align(full_len) * ws;
}

static unsigned int content_offset(Ptr *obj)
{
    unsigned int tag = OBJTAG(obj);
    unsigned int offset = 1;
    if (tag == proc_tag) offset++;
    return offset;
}

static void gc_initialize(unsigned int heap_size)
{
    gc_space_size = heap_size;
    gc_cur_space = calloc(1, gc_space_size);
    gc_to_space = calloc(1, gc_space_size);
    heap_end = gc_cur_space + gc_space_size;

    if (TAG(gc_cur_space) != 0 || TAG(gc_to_space) != 0) {
        error_exit("memory not aligned\n");
    }
}

static void set_forward(Ptr *obj, Ptr to) { *obj = to | 0x1; }
static int forwarded(Ptr *obj) { return *obj & 0x1; }

/* Cheney collector */
static void gc_copy_forward(Ptr *p)
{
    unsigned int tag = TAG(*p);
    if (tag == number_tag || tag == immed_tag || tag == float_tag) { return; }  /* number & immediate & float -> ignore */
    Ptr *obj = OBJ(*p);
    if (obj == 0) { return; }                                                   /* null pointer -> ignore */
    if (forwarded(obj)) {   /* update pointer with forwarding info */
        LOG("U%x", tag);
        *p = UNTAG(*obj) | tag;
    } else {                /* copy and forward object */
        LOG("C%x", tag);
        unsigned int size = object_size(obj);
        memcpy(gc_free, obj, size);
        set_forward(obj, PTR(gc_free));
        *p = PTR(gc_free) | tag;
        gc_free += size;
    }
}

static void gc_walk_roots(struct RootSet *root)
{
    LOG("gc_walk_roots\n");
    if (root->usedregs != 0) {
        error_exit("not implemented\n");
    }
    LOG("walk registers\n");
    root->regs.sym.cp |= proc_tag;                  /* cp is untagged. tag it temporarily. */
    gc_copy_forward(&root->regs.sym.cp); LOG("\n");
    root->regs.sym.cp = UNTAG(root->regs.sym.cp);
    LOG("walk stack\n");                            /* walk only one frame now */
    Ptr *p = (Ptr *)stack_bottom;
    p++;                                            /* skip return code pointer */
    for (; p < (Ptr *)root->stack_top; p++)
        gc_copy_forward(p);
    LOG("\n");
    LOG("end gc_walk_roots\n");
}

void gc_collect(struct RootSet *root)
{
    LOG(";; gc_collect called\n");
    /*memset(gc_to_space, 0, gc_space_size);*/
    gc_scan = gc_free = gc_to_space;
    gc_walk_roots(root);
    while (gc_scan < gc_free) {
        Ptr *obj = OBJ(gc_scan);
        switch (OBJTAG(obj)) {
        case number_tag:
        case immed_tag:
        case float_tag:
            error_exit("scan incorrect, unboxed type.\n");
            break;
        case pair_tag:
        case symbol_tag:
        case vector_tag:
        case proc_tag:
            {
                unsigned int i, len = OBJLENGTH(obj);
                unsigned int offset = content_offset(obj);
                for (i=0; i<len; i++)
                    gc_copy_forward(&obj[i+offset]);
                LOG("\n");
                gc_scan += align(offset + len) * ws;
                break;
            }
        case string_tag:
            gc_scan += object_size(obj);
            break;
        }
    }
    LOG("switch roles of gc spaces\n");
    char *temp = gc_to_space;
    gc_to_space = gc_cur_space;
    gc_cur_space = temp;
    root->regs.sym.ap = gc_free;
    heap_end = gc_cur_space + gc_space_size;
}

static void print_string(Ptr ptr)
{
    int n;
    char *s;
    n = OBJLENGTH(ptr);
    s = STRINGDATA(ptr);
    while (n--) {
        if (*s == '"' || *s == '\\') printf("\\");
        printf("%c", *s++);
    }
}

static void print(Ptr ptr)
{
    switch (TAG(ptr)) {
    case number_tag:
        printf("%ld", ptr/(mask+1)); break;
    case immed_tag:
        switch (IMMTAG(ptr)) {
        case bool_tag:
            printf((ptr>>imm_tag_len) ? "#t" : "#f"); break;
        case null_tag:
            printf("()"); break;
        case char_tag:
            switch (ptr>>imm_tag_len) {
            case '\n':
                printf("#\\newline"); break;
            case ' ':
                printf("#\\space"); break;
            case 9:
                printf("#\\tab"); break;
            default:
                printf("#\\%c", (char)(ptr>>imm_tag_len)); break;
            }
            break;
        }
        break;
    case pair_tag:
        printf("(");
        print(CAR(ptr));
        ptr = CDR(ptr);
        while (TAG(ptr) == pair_tag) {
            printf(" ");
            print(CAR(ptr));
            ptr = CDR(ptr);
        }
        if (IMMTAG(ptr) != null_tag) {
            printf(" . ");
            print(ptr);
        }
        printf(")");
        break;
    case string_tag:
        printf("\"");
        print_string(ptr);
        printf("\"");
        break;
    case symbol_tag:
        print_string(SYMBOLNAME(ptr));
        break;
    case vector_tag:
        {
            int n;
            Ptr *p;
            printf("#(");
            n = OBJLENGTH(ptr);
            p = VECTORDATA(ptr);
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
        printf("<procedure>", ptr);
        break;
    case float_tag:
        {
            Ptr x = UNTAG(ptr);
            printf("%f", *(float *)&x);
            break;
        }
    default:
        printf("#<garbage %x>", (unsigned int)ptr);
        break;
    }
}

int main(int argc, char *argv[])
{
    stack_bottom = calloc(1, default_stack_size);
    gc_initialize(default_heap_size);

    print(call_scheme(stack_bottom, gc_cur_space));

    printf("\n");
    return 0;
}
