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
    int *variables;
    int *args;
    int *c; // whatever this is
    char* saved_ip;
    Function *prev;
};

typedef struct {
    int *mem;
    int *sp;
} Stack;

static char* chars = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'";
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
  
  return make_box(h);
}

int check_tag (void *d, int t, int n) {
//   if (UNBOXED(d)) return BOX(0);
  data * r = TO_DATA(d);
//   printf("%d %d %d %d %d %d",
//    TAG(r->tag), SEXP_TAG,
//     TO_SEXP(d)->tag, t, LEN(r->tag), n);
    return make_box(TAG(r->tag) == SEXP_TAG && TO_SEXP(d)->tag == t && LEN(r->tag) == n);
}


Stack stack = {NULL, NULL};
Function *function = NULL;

int pop() {
    stack.sp--;
    int ret = *stack.sp;
    return ret;
}

void push(int v) {
    *stack.sp = v;
    stack.sp++;
}

void init() {
    stack.mem = malloc(1000000 * sizeof(int));
    stack.sp = stack.mem;
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
        int a, b, value, res, n;
        int *p;

        // fprintf(log, "pppp%d %d\n", h, l);
        switch (h) {
            // fprintf(log, "dddd%d\n", h);
            case 15:
                goto stop;

            case 0: /* BINOP */
                b = remove_box(pop());
                a = remove_box(pop());
                res = 0;
                char *op = ops[l - 1];
                if (strcmp(op, "+") == 0) {
                    res = a + b;
                } else if (strcmp(op, "-") == 0) {
                    res = a - b;
                } else if (strcmp(op, "*") == 0) {
                    res = a * b;
                } else if (strcmp(op, "/") == 0) {
                    res = a / b;
                } else if (strcmp(op, "%") == 0) {
                    res = a % b;
                } else if (strcmp(op, "<") == 0) {
                    res = a < b;
                } else if (strcmp(op, "<=") == 0) {
                    res = a <= b;
                } else if (strcmp(op, ">") == 0) {
                    res = a > b;
                } else if (strcmp(op, ">=") == 0) {
                    res = a >= b;
                } else if (strcmp(op, "==") == 0) {
                    res = a == b;
                } else if (strcmp(op, "!=") == 0) {
                    res = a != b;
                } else if (strcmp(op, "&&") == 0) {
                    res = a && b;
                } else if (strcmp(op, "!!") == 0) {
                    res = a || b;
                }
                push(make_box(res));
                // fprintf(log, "binop %d %s %d == %d\n", a, op, b, res);
                break;

            case 1:
                switch (l) {
                    case 0: // CONST
                        push(make_box(INT));
                        break;

                    case 1: // STRING
                        ; void* p = STRING;
                        int   n = strlen (p);
                        data *r = (data*) malloc (n + 1 + sizeof (int));
                        r->tag = STRING_TAG | (n << 3);

                        strncpy (r->contents, p, n + 1);
                        push(r->contents);
                        // fprintf(f, "string %s\n", r->contents);
                        break;

                    case 2: // SEXP
                        ;char* s = STRING;
                        int hash = make_hash(s);
                        n = INT;                        
                        push(make_sexp(n, hash));
                        break;

                    case 3: // STI
                        // TODO how?
                        // fprintf(log, "STI");
                        a = pop();
                        b = pop();
                        push(a);
                        break;

                    case 4: // STA
                        value = pop();
                        int i = pop();
                        int place = pop();
                        push(Bsta(value, i, place));
                        break;

                    case 5: // JMP
                        ip = bf->code_ptr + INT;
                        break;

                    case 6: // END
                        if (function->prev == NULL) {
                            return;
                        }
                        res = pop();
                        stack.sp = function->args;
                        ip = function->saved_ip;
                        function = function->prev;
                        push(res);
                        // fprintf(log, "END");
                        break;

                    case 7: // RET
                        FAIL;
                        // TODO 
                        return;
                        // fprintf(log, "RET");
                        break;

                    case 8: // DROP
                        pop();
                        break;

                    case 9: // DUP
                        value = pop();
                        push(value);
                        push(value);
                        break;

                    case 10: //SWAP
                        a = pop();
                        b = pop();
                        push(b);
                        push(a);
                        break;

                    case 11: // ELEM
                        b = pop();
                        a = pop();
                        push(Belem(a, b));
                        // fprintf(log, "Elem = %d\n", b)
                        break;

                    default:
                        FAIL;
                }
                break;

            case 2: // LD
            case 3: // LDA
            case 4: // ST
                // fprintf(log, "%d %d\n", h, l);
                p = 0;
                switch (l) {
                    case 0: // G
                        // fprintf(log, "global ");
                        p = bf->global_ptr + INT;
                        break;
                    case 1: // L
                        // fprintf(log, "local ");
                        p = function->variables + INT;
                        break;
                    case 2: // A
                        // fprintf(log, "arg ");
                        p = function->args + INT;
                        break;
                    case 3: // C
                        p = function->c + INT;
                        break;
                    default:
                        FAIL;
                }
                if (h == 2) { //LD
                    value = *p;
                    // fprintf(f, "Loading %d\n", remove_box(value));
                    push(value);
                } else if (h == 3) {
                    push(p);
                    push(p);
                } else if (h == 4) {//ST
                    res = pop();
                    // printf("Storing %d\n", res);
                    // fprintf(log, "store %d\n", res);
                    *p = res;
                    push(res);
                }
                break;

            case 5:
                switch (l) {
                    case 0: // CJMPz
                        value = remove_box(pop());
                        res = INT;
                        if (!value) {
                            ip = bf->code_ptr + res;
                        }
                        break;

                    case 1: // CJMPnz
                        value = remove_box(pop());
                        res = INT;
                        if (value) {
                            ip = bf->code_ptr + res;
                        }
                        break;

                    case 2: // BEGIN
                    // fprintf(log, "BEGIN\n");
                        ;Function *fun = malloc(sizeof(Function));
                        fun->args = stack.sp;
                        stack.sp += INT + 1; // args
                            fun->saved_ip = pop();
                        fun->variables = stack.sp;
                        fun->c = fun->variables;
                        stack.sp += INT; // local vars
                        fun->prev = function;
                        function = fun;
                        break;

                    case 3:
                        FAIL;
                        // fprintf(log, "CBEGIN\t%d ", INT);
                        // fprintf(log, "%d", INT);
                        break;

                    case 4:
                        // fprintf(log, "CLOSURE\t0x%.8x", INT);
                        {
                            int n = INT;
                            for (int i = 0; i < n; i++) {
                                switch (BYTE) {
                                    case 0:
                        FAIL;

                                        // fprintf(log, "G(%d)", INT);
                                        break;
                                    case 1:
                        FAIL;
                                        // fprintf(log, "L(%d)", INT);
                                        break;
                                    case 2:
                        FAIL;
                                        // fprintf(log, "A(%d)", INT);
                                        break;
                                    case 3:
                        FAIL;
                                        // fprintf(log, "C(%d)", INT);
                                        break;
                                    default:
                                        FAIL;
                                }
                            }
                        };
                        break;

                    case 5:
                        FAIL;

                        // fprintf(log, "CALLC\t%d", INT);
                        break;

                    case 6:
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

                    case 7: // TAG
                        value = make_hash(STRING);
                        n = INT;
                        res = check_tag(pop(), value, n);
                        push(res);
                        break;

                    case 8:
                                            FAIL;
                        //fprintf(log, "ARRAY\t%d", INT);
                        break;

                    case 9:
                        FAIL;
                        // fprintf(log, "FAIL\t%d", INT);
                        // fprintf(log, "%d", INT);
                        break;

                    case 10:
                        INT;
                        break;

                    default:
                        FAIL;
                }
                break;

            case 6:
                switch (l) {
                    FAIL;
                }
                // fprintf(log, "PATT\t%s", pats[l]);
                break;

            case 7: { // Builtin
                switch (l) {
                    case 0:
                        res = Lread();
                        // fprintf(log, "Reading %d\n", res);
                        break;
                    case 1:
                        res = Lwrite(pop());
                        // fprintf(log, "wrote %d\n", res);
                        break;

                    case 2:
                        // fprintf(f, "Length %d\n", res);
                        res = Llength(pop());
                        break;

                    case 3:
                        res = Lstring(pop());
                        break;

                    case 4: // ARRAY
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
