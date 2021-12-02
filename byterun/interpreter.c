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

typedef struct {
    int *variables;
    int *args;
    int *c; // whatever this is
} Function;

typedef struct {
    int *mem;
    int *sp;
} Stack;

Stack stack;
Function function;

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
    function.args = 0;
    function.variables = 0;
    function.c = 0;
}

void destruct() {
    free(stack.mem);
}


/* Disassembles the bytecode pool */
void interpret(FILE *f, bytefile *bf) {
    init();

# define INT    (ip += sizeof (int), *(int*)(ip - sizeof (int)))
# define BYTE   *ip++
# define STRING get_string (bf, INT)
# define FAIL   failure ("ERROR: invalid opcode %d-%d\n", h, l)

    char *ops[] = {"+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "!!"};
    char *pats[] = {"=str", "#string", "#array", "#sexp", "#ref", "#val", "#fun"};
    char *lds[] = {"LD", "LDA", "ST"};
    char *ip = bf->code_ptr;
    do {
        char x = BYTE,
                h = (x & 0xF0) >> 4,
                l = x & 0x0F;
        int a, b, value, res, n;
        int *p;

        switch (h) {
            case 15:
                goto stop;

            case 0: /* BINOP */
                // fprintf(f, "binop\n");
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
                } else if (strcmp(op, "\%") == 0) {
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
                // fprintf(f, "binop %d %s %d == %d\n", a, op, b, res);
                break;

            case 1:
                switch (l) {
                    case 0: // CONST
                        push(make_box(INT));
                        break;

                    case 1: // STRING
                        push(STRING);
                        break;

                    case 2: // SEXP
                        // TODO how?
                        break;

                    case 3: // STI
                        // TODO how?
                        // fprintf(f, "STI");
                        a = pop();
                        b = pop();
                        push(a);
                        break;

                    case 4: // STA
                        value = pop();
                        int i = pop();
                        int mem = pop();
                        push(Bsta(value, i, mem));
                        break;

                    case 5: // JMP
                        ip = bf->code_ptr + INT;
                        break;

                    case 6: // END
                        // TODO how?
                        return;
                        // fprintf(f, "END");
                        break;

                    case 7: // RET
                        // TODO how?
                        return;
                        // fprintf(f, "RET");
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
                        a = pop();
                        b = pop();
                        push(Belem(b, a));
                        break;

                    default:
                        FAIL;
                }
                break;

            case 2: // LD
            case 3: // LDA
            case 4: // ST
                // fprintf(f, "%d %d\n", h, l);
                p = 0;
                switch (l) {
                    case 0: // G
                        p = bf->global_ptr + INT;
                        break;
                    case 1: // L
                        p = function.variables + INT;
                        break;
                    case 2: // A
                        p = function.args + INT;
                        break;
                    case 3: // C
                        p = function.c + INT;
                        break;
                    default:
                        FAIL;
                }
                if (h == 2) { //LD
                    push(*p);
                } else if (h == 3) {
                    push(p);
                    push(p);
                } else if (h == 4) {//ST
                    res = pop();
                    // fprintf(f, "store %d\n", res);
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

                    case 2:; // BEGIN
                    Function fun;
                    fun.args = stack.sp;
                    stack.sp += INT + 1;
                    fun.variables = stack.sp;
                    stack.sp += INT;
                    function = fun;
                    break;

                    case 3:
                        // fprintf(f, "CBEGIN\t%d ", INT);
                        // fprintf(f, "%d", INT);
                        break;

                    case 4:
                        // fprintf(f, "CLOSURE\t0x%.8x", INT);
                        {
                            int n = INT;
                            for (int i = 0; i < n; i++) {
                                switch (BYTE) {
                                    case 0:
                                        // fprintf(f, "G(%d)", INT);
                                        break;
                                    case 1:
                                        // fprintf(f, "L(%d)", INT);
                                        break;
                                    case 2:
                                        // fprintf(f, "A(%d)", INT);
                                        break;
                                    case 3:
                                        // fprintf(f, "C(%d)", INT);
                                        break;
                                    default:
                                        FAIL;
                                }
                            }
                        };
                        break;

                    case 5:
                        // fprintf(f, "CALLC\t%d", INT);
                        break;

                    case 6:
                        // fprintf(f, "CALL\t0x%.8x ", INT);
                        // fprintf(f, "%d", INT);
                        break;

                    case 7:
                        // fprintf(f, "TAG\t%s ", STRING);
                        // fprintf(f, "%d", INT);
                        break;

                    case 8:
                        // fprintf(f, "ARRAY\t%d", INT);
                        break;

                    case 9:
                        // fprintf(f, "FAIL\t%d", INT);
                        // fprintf(f, "%d", INT);
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

                }
                // fprintf(f, "PATT\t%s", pats[l]);
                break;

            case 7: {
                switch (l) {
                    case 0:
                        res = Lread();
                        // printf("Reading %d\n", res);
                        push(res);
                        break;
                    case 1:
                        Lwrite(pop());
                        break;

                    case 2:
                        Llength(pop());
                        break;

                    case 3:
                        Lstring(pop());
                        break;

                    case 4:
                        n = pop();
                        // TODO where to get values to array
//                        Barray(pop());
                        break;

                    default:
                        FAIL;
                }
            }
                break;

            default:
                FAIL;
        }

        // fprintf(f, "\n");
    } while (1);
    stop:
        destruct();
    // fprintf(f, "<end>\n");
}

int main(int argc, char *argv[]) {
    bytefile *f = read_file(argv[1]);
    interpret(stdout, f);
    return 0;
}
