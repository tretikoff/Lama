/* Lama SM Bytecode interpreter */

# include <string.h>
# include <stdio.h>
# include <errno.h>
# include <stdlib.h>
# include "../runtime/runtime.h"

# define make_box(x) ((((int) (x)) << 1) | 0x0001)
# define remove_box(x)    (((int) (x)) >> 1)

void *__start_custom_data;
void *__stop_custom_data;

enum opcode {
    CONST, STRIN, SEXP, STI, STA, JMP, END, RET, DROP, DUP, SWAP, ELEM
};
enum placement {GLOB, LOC, ARGS, ACC};
enum Lds {LD = 2, LDA = 3, ST = 4};
enum builtin { LREAD, LWRITE, LLENGTH, LSTRING, LARRAY };

enum tag {TSTR, TSTRTYPE, TARR, TSEXP, TREF, TVAL, TFUN};
enum control {CJMPz, CJMPnz, BEGIN, CBEGIN, CLOSURE, CALLC, CALL, TAG, PARRAY, PFAIL, LINE};

enum opps {BINOP, STOR, CONTR=5, PATT=6, BUILTIN=7, E_O_F = 15};

enum binops {BADD,BSUB,BMUL,BDIV,BMOD,BLT,BLTE,BGT,BGTE,BEQ,BNEQ,BAND,BOR};


/* The unpacked representation of bytecode file */
typedef struct {
    char *string_ptr;              /* A pointer to the beginning of the string table */
    int *public_ptr;              /* A pointer to the beginning of publics table    */
    char *code_ptr;                /* A pointer to the bytecode itself               */
    int *global_ptr;              /* A pointer to the global area                   */
    int stringtab_size;          /* The size (in bytes) of the string table        */
    int global_area_size;        /* The size (in words) of global area             */
    int public_symbols_number;   /* The number of public symbols                   */
    char buffer[0];
} bytefile;

/* Gets a string from a string table by an index */
char *get_string(bytefile *f, int pos) {
    return &f->string_ptr[pos];
}

/* Gets a name for a public symbol */
char *get_public_name(bytefile *f, int i) {
    return get_string(f, f->public_ptr[i * 2]);
}

/* Gets an offset for a publie symbol */
int get_public_offset(bytefile *f, int i) {
    return f->public_ptr[i * 2 + 1];
}

/* Reads a binary bytecode file by name and unpacks it */
bytefile *read_file(char *fname) {
    FILE *f = fopen(fname, "rb");
    long size;
    bytefile *file;

    if (f == 0) {
        failure("%s\n", strerror(errno));
    }

    if (fseek(f, 0, SEEK_END) == -1) {
        failure("%s\n", strerror(errno));
    }

    file = (bytefile *) malloc(sizeof(int) * 4 + (size = ftell(f)));

    if (file == 0) {
        failure("*** FAILURE: unable to allocate memory.\n");
    }

    rewind(f);

    if (size != fread(&file->stringtab_size, 1, size, f)) {
        failure("%s\n", strerror(errno));
    }

    fclose(f);

    file->string_ptr = &file->buffer[file->public_symbols_number * 2 * sizeof(int)];
    file->public_ptr = (int *) file->buffer;
    file->code_ptr = &file->string_ptr[file->stringtab_size];
    file->global_ptr = (int *) malloc(file->global_area_size * sizeof(int));

    return file;
}

typedef struct Function Function;

struct Function {
    int *vars;
    int *args;
    int *acc;
    char* saved_ip;
    Function *prev;
};

typedef struct {
    int *mem;
    int *sp;
} Stack;

int make_hash (char *s) {
  char *p;
  int  h = 0, limit = 0;
               
  p = s;

  while (*p && limit++ < 4) {
    char *q = chars;
    int pos = 0;
    
    for (; *q && *q != *p; q++, pos++);

    if (*q) h = (h << 6) | pos;    
    else failure ("tagHash: character not found: %c\n", *p);

    p++;
  }

  if (strcmp (s, de_hash (h)) != 0) {
    failure ("%s <-> %s\n", s, de_hash(h));
  }
  
  
  return make_box(h);
}

int check_tag (void *d, int t, int n) {
  data * r = TO_DATA(d);
    if (UNBOXED(d)) return BOX(0);
//   printf("%d %d %d %d %d %d",
//    TAG(r->tag), SEXP_TAG,
//     TO_SEXP(d)->tag, t, LEN(r->tag), n);
    return make_box(TAG(r->tag) == SEXP_TAG && TO_SEXP(d)->tag == t && LEN(r->tag) == n);
}


Stack stack = {NULL, NULL};
Function *function = NULL;
int* stack_start = NULL;
int* stack_end = NULL;
const int stack_capacity = 1000000;
int pop() {
    if (stack.sp == stack_start) {
        if (stack.sp == stack_start) {
            failure("Pop from empty stack\n")
        }
    }
    stack.sp--;
    int ret = *stack.sp;
    return ret;
}

void push(int v) {
    if (stack.sp == stack_end) {
        failure("Stack overflow\n");
    }
    *stack.sp = v;
    stack.sp++;
}

void init() {
    stack.mem = malloc(stack_capacity * sizeof(int));
    stack.sp = stack.mem;
    stack_start = stack.sp;
    stack_end = stack.sp + stack_capacity;
}

void destruct() {
    // free(stack.mem);
}


void* make_array(int n) {
    int     i, ai; 
    data    *r; 
    r = (data*) malloc (sizeof(int) * (n+1));
    r->tag = ARRAY_TAG | (n << 3);
    
    for (i = n -1; i >=0; i--) {
        ((int*)r->contents)[i] = pop();
    }
    return r->contents;
}

void* make_sexp (int n, int hash) {
    // printf("with hash %d", hash);
  int     i, ai;  
  sexp   *r;  
  data   *d;  
  r = (sexp*) malloc (sizeof(int) * (n + 2));
  d = &(r->contents);    
  d->tag = SEXP_TAG | (n << 3);

  for (i=n - 1; i>= 0; i--) {
    ((int*)d->contents)[i] = pop();
  }

  r->tag = hash;

    // printf("checking %d", d);
  return d->contents;
}

void* make_closure (int n, void *entry) {
  int     i, ai;
  data    *r; 
  
  r = (data*) malloc (sizeof(int) * (n+2));
  
  r->tag = CLOSURE_TAG | ((n + 1) << 3);
  ((void**) r->contents)[0] = entry;
  
  for (i = 0; i<n; i++) {
        ai = pop();
        ((int*)r->contents)[i+1] = ai;
        // printf("Closure %d\n", ai);
  }


  return r->contents;
}



/* Disassembles the bytecode pool */
void interpret(FILE *f, bytefile *bf, FILE* log) {
    init();

# define INT    (ip += sizeof (int), *(int*)(ip - sizeof (int)))
# define BYTE   *ip++
# define STRING get_string (bf, INT)
# define FAIL   failure ("ERROR: invalid opcode %d-%d\n", h, l)

    char *ops[] = {"+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "!!"};
    char *pats[] = {"=str", "#string", "#array", "#sexp", "#ref", "#val", "#fun"};
    char *lds[] = {"LD", "LDA", "ST"};
    char *ip = bf->code_ptr;
    // fprintf(log, "xxxx\n");
    do {
        char x = BYTE,
                h = (x & 0xF0) >> 4,
                l = x & 0x0F;
        int a, b, value, res, n, pos;
        int *p;
        Function *fun;

        // fprintf(log, "pppp%d %d\n", h, l);
        switch (h) {
            // fprintf(log, "dddd%d\n", h);
            case E_O_F:
                goto stop;

            case BINOP:
                b = remove_box(pop());
                a = remove_box(pop());
                res = 0;
                switch(l-1) {
                    case BADD: res = a + b; break;
                    case BSUB: res = a - b; break;
                    case BMUL: res = a * b; break;
                    case BDIV: res = a / b; break;
                    case BMOD: res = a % b; break;
                    case BLT: res = a < b; break;
                    case BLTE: res = a <= b; break;
                    case BGT: res = a > b; break;
                    case BGTE: res = a >= b; break;
                    case BEQ: res = a == b; break;
                    case BNEQ: res = a != b; break;
                    case BAND: res = a && b; break;
                    case BOR: res = a || b; break;
                }
                char *op = ops[l - 1];
                push(make_box(res));
                // fprintf(log, "binop %d %s %d == %d\n", a, op, b, res);
                break;

            case STOR:
                switch (l) {
                    case CONST:
                        push(make_box(INT));
                        break;

                    case STRIN:
                        ; res = make_string(STRING);
                        push(res);
                        break;

                    case SEXP:
                        ;char* s = STRING;
                        int hash = make_hash(s);
                        n = INT;                        
                        push(make_sexp(n, hash));
                        break;

                    case STA:
                        value = pop();
                        int i = pop();
                        int place = pop();
                        push(Bsta(value, i, place));
                        break;

                    case JMP:
                        ip = bf->code_ptr + INT;
                        break;

                    case END:
                        if (function->prev == NULL) {
                            return;
                        }
                        res = pop();
                        stack.sp = function->args;
                        ip = function->saved_ip;
                        function = function->prev;
                        push(res);
                        break;

                    case DROP:
                        pop();
                        break;

                    case DUP:
                        value = pop();
                        push(value);
                        push(value);
                        break;

                    case SWAP:
                        a = pop();
                        b = pop();
                        push(b);
                        push(a);
                        break;

                    case ELEM:
                        b = pop();
                        a = pop();
                        push(Belem(a, b));
                        // fprintf(log, "Elem = %d\n", b)
                        break;

                    default:
                        FAIL;
                }
                break;

            case LD:
            case LDA:
            case ST:
                // fprintf(log, "%d %d\n", h, l);
                p = 0;
                switch (l) {
                    case GLOB:
                        // fprintf(log, "global ");
                        p = bf->global_ptr + INT;
                        break;
                    case LOC:
                        // fprintf(log, "local ");
                        p = function->vars + INT;
                        break;
                    case ARGS:
                        // fprintf(log, "arg ");
                        p = function->args + INT;
                        break;
                    case ACC:
                        p = *(function->acc + INT);
                        break;
                    default:
                        FAIL;
                }
                if (h == LD) {
                    value = *p;
                    if (l == 3) {
                        // printf("loading %d\n", value);
                    }
                    // fprintf(f, "Loading %d\n", remove_box(value));
                    push(value);
                } else if (h == LDA) {
                    push(p);
                    push(p);
                } else if (h == ST) {
                    res = pop();
                    // printf("Storing %d\n", res);
                    // fprintf(log, "store %d\n", res);
                    *p = res;
                    push(res);
                }
                break;

            case CONTR:
                switch (l) {
                    case CJMPz:
                        value = remove_box(pop());
                        res = INT;
                        if (!value) {
                            ip = bf->code_ptr + res;
                        }
                        break;

                    case CJMPnz:
                        value = remove_box(pop());
                        res = INT;
                        if (value) {
                            ip = bf->code_ptr + res;
                        }
                        break;

                    case BEGIN:
                        // fprintf(log, "BEGIN\n");
                        fun = malloc(sizeof(Function));
                        fun->args = stack.sp;
                        stack.sp += INT + 1; // args
                        fun->saved_ip = pop();
                        fun->vars = stack.sp;
                        fun->acc = fun->vars;
                        stack.sp += INT; // local vars
                        fun->prev = function;
                        function = fun;
                        break;

                    case CBEGIN:
                        fun = malloc(sizeof(Function));
                        n = INT;
                        // printf("CBEGIN %d\n", n);
                        fun->args = stack.sp;
                        stack.sp += n + 2;
                        int nn = pop();
                        fun->saved_ip = pop();
                        stack.sp += 2;
                        fun->acc = stack.sp;
                        stack.sp += nn;
                        fun->vars = stack.sp;
                        stack.sp += INT;
                        fun->prev = function;
                        function = fun;
                        break;

                    case CLOSURE:
                        pos = INT;
                        n = INT;
                        for (int i = 0; i < n; i++) {
                            switch (BYTE) {
                                case 0: res = *(bf->global_ptr + INT); break;
                                case 1: res = *(function->vars + INT); break;
                                case 2: res = *(function->args + INT); break;
                                case 3: res = *(int*)*(function->acc + INT); break;
                                default: FAIL;
                            }
                            push(res);
                        }
                        push(make_closure(n, pos));
                        break;

                    case CALLC:
                        n = INT;
                        data* d = TO_DATA(*(stack.sp - n - 1));

                        memmove(stack.sp - n - 1, stack.sp - n, n * sizeof(int));
                        stack.sp--;
                        int offset = *(int*)d->contents;
                        int accN = LEN(d->tag) - 1;
                        push(ip);
                        push(accN);
                        for (int i = accN - 1; i >= 0; i--) {
                            push(((int*)d->contents) + i + 1);
                        }
                        ip = bf->code_ptr + offset;
                        stack.sp -= accN+n+2;
                        // printf("CALLC %d %d\n", accN, n);
                        break;

                    case CALL:
                        // fprintf(log, "CALL %d", remove_box(res));
                        value = INT;
                        n = INT;
                        // if (strcmp(bf->string_ptr + value, ".array" == 0)) {
                        //     // fprintf(f, "Array create %d", n);
                        //     make_array(n);
                        //     break;
                        // }
                        push(ip);
                        push(0);
                        ip = bf->code_ptr + value; // goto label
                        stack.sp -= (n + 2); // count of args + ip
                        // fprintf(log, "%d", INT);
                        break;

                    case TAG:
                        value = make_hash(STRING);
                        n = INT;
                        res = check_tag(pop(), value, n);
                        push(res);
                        break;

                    case PARRAY:
                        n = make_box(INT);
                        push(Barray_patt(pop(), n));
                        break;

                    case PFAIL:
                        a = make_box(INT);
                        b = make_box(INT);
                        Bmatch_failure(pop(), "", a, b);
                        break;

                    case LINE:
                        INT;
                        break;

                    default:
                        FAIL;
                }
                break;

            case PATT: 
                switch (l) {
                    case 0: res = Bstring_patt(pop(), pop()); break;
                    case 1: res = Bstring_tag_patt(pop()); break;
                    case 2: res = Barray_tag_patt(pop()); break;
                    case 5: res = Bunboxed_patt(pop()); break;
                    case 6: res = Bclosure_tag_patt(pop()); break;
                    default: FAIL;
                }
                push(res);
                break;

            case BUILTIN: {
                switch (l) {
                    case LREAD:
                        res = Lread();
                        // fprintf(log, "Reading %d\n", res);
                        break;
                    case LWRITE:
                        res = Lwrite(pop());
                        // fprintf(log, "wrote %d\n", res);
                        break;

                    case LLENGTH:
                        // fprintf(f, "Length %d\n", res);
                        res = Llength(pop());
                        break;

                    case LSTRING:
                        res = make_lstring(pop());
                        break;

                    case LARRAY:
                        res = make_array(INT);
                        break;

                    default:
                        FAIL;
                }
                push(res);
            }
                break;

            default:
                FAIL;
        }

        // fprintf(log, "\n");
    } while (1);
    stop:
        destruct();
}

int main(int argc, char *argv[]) {
    bytefile *f = read_file(argv[1]);
    interpret(stdout, f, stdout);
    return 0;
}
