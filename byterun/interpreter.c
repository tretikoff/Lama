/* Lama SM Bytecode interpreter */

# include <string.h>
# include <stdio.h>
# include <errno.h>
# include <stdlib.h>
# include "../runtime/runtime.h"

# define make_box(x) ((((int) (x)) << 1) | 0x0001)

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

struct Stack {
    int *mem;
    int *sp;
};

struct Stack stack;

int pop() {
    return *(stack.sp--);
}

void push(int v) {
    *stack.sp = v;
    stack.sp++;
}

void init() {
    stack.sp = 0;
    stack.mem = malloc(1000000 * sizeof(int));
}


/* Disassembles the bytecode pool */
void interpret(FILE *f, bytefile *bf) {
    init();

# define INT    (ip += sizeof (int), *(int*)(ip - sizeof (int)))
# define BYTE   *ip++
# define STRING get_string (bf, INT)
# define FAIL   failure ("ERROR: invalid opcode %d-%d\n", h, l)

    char *ip = bf->code_ptr;
    char *ops[] = {"+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "!!"};
    char *pats[] = {"=str", "#string", "#array", "#sexp", "#ref", "#val", "#fun"};
    char *lds[] = {"LD", "LDA", "ST"};
    do {
        char x = BYTE,
                h = (x & 0xF0) >> 4,
                l = x & 0x0F;
        int a, b, value, res;
        int* p;

//        fprintf(f, "0x%.8x:\t", ip - bf->code_ptr - 1);

        switch (h) {
            case 15:
                goto stop;

                /* BINOP */
            case 0:
                b = pop();
                a = pop();
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
                break;

            case 1:
                switch (l) {
                    case 0:
                        push(make_box(INT));
                        break;

                    case 1:
                        push(STRING);
                        break;

                    case 2:
                        // TODO how?
                        break;

                    case 3:
                        // TODO how?
                        fprintf(f, "STI");
                        int z = pop();
                        int r = pop();
                        push(z);
                        break;

                    case 4: // STA
                        value = pop();
                        int i = pop();
                        int mem = pop();
                        push(Bsta(value, i, mem));
                        break;

                    case 5:
                        // TODO
                        fprintf(f, "JMP\t0x%.8x", INT);
                        break;

                    case 6:
                        // TODO how?
                        fprintf(f, "END");
                        break;

                    case 7:
                        // TODO how?
                        fprintf(f, "RET");
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
                p = 0;
                switch (l) {
                    case 0: // G
                        p = bf->global_ptr + INT;
                        break;
                    case 1: // L
//                        p = bf->local_var + INT;
                        break;
                    case 2: // A
//                        p = bf->args + INT;
                        break;
                    case 3: // C
//                        p = bf->c + INT; // TODO
                        break;
                    default:
                        FAIL;
                }
                if (h == 2) {
                    push(p);
                } else if (h == 3) {
                    push(p);
                    push(p);
                } else if (h == 4) {
                    res = pop();
                    *p = res;
                    push(res);
                }
                break;

            case 5:
                switch (l) {
                    case 0:
                        fprintf(f, "CJMPz\t0x%.8x", INT);
                        break;

                    case 1:
                        fprintf(f, "CJMPnz\t0x%.8x", INT);
                        break;

                    case 2:
                        fprintf(f, "BEGIN\t%d ", INT);
                        fprintf(f, "%d", INT);
                        break;

                    case 3:
                        fprintf(f, "CBEGIN\t%d ", INT);
                        fprintf(f, "%d", INT);
                        break;

                    case 4:
                        fprintf(f, "CLOSURE\t0x%.8x", INT);
                        {
                            int n = INT;
                            for (int i = 0; i < n; i++) {
                                switch (BYTE) {
                                    case 0:
                                        fprintf(f, "G(%d)", INT);
                                        break;
                                    case 1:
                                        fprintf(f, "L(%d)", INT);
                                        break;
                                    case 2:
                                        fprintf(f, "A(%d)", INT);
                                        break;
                                    case 3:
                                        fprintf(f, "C(%d)", INT);
                                        break;
                                    default:
                                        FAIL;
                                }
                            }
                        };
                        break;

                    case 5:
                        fprintf(f, "CALLC\t%d", INT);
                        break;

                    case 6:
                        fprintf(f, "CALL\t0x%.8x ", INT);
                        fprintf(f, "%d", INT);
                        break;

                    case 7:
                        fprintf(f, "TAG\t%s ", STRING);
                        fprintf(f, "%d", INT);
                        break;

                    case 8:
                        fprintf(f, "ARRAY\t%d", INT);
                        break;

                    case 9:
                        fprintf(f, "FAIL\t%d", INT);
                        fprintf(f, "%d", INT);
                        break;

                    case 10:
                        fprintf(f, "LINE\t%d", INT);
                        break;

                    default:
                        FAIL;
                }
                break;

            case 6:
                fprintf(f, "PATT\t%s", pats[l]);
                break;

            case 7: {
                switch (l) {
                    case 0:
                        fprintf(f, "CALL\tLread");
                        break;

                    case 1:
                        fprintf(f, "CALL\tLwrite");
                        break;

                    case 2:
                        fprintf(f, "CALL\tLlength");
                        break;

                    case 3:
                        fprintf(f, "CALL\tLstring");
                        break;

                    case 4:
                        fprintf(f, "CALL\tBarray\t%d", INT);
                        break;

                    default:
                        FAIL;
                }
            }
                break;

            default:
                FAIL;
        }

        fprintf(f, "\n");
    } while (1);
    stop:
    fprintf(f, "<end>\n");
}

int main(int argc, char *argv[]) {
    bytefile *f = read_file(argv[1]);
    interpret(stdout, f);
    return 0;
}
