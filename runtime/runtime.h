# ifndef __LAMA_RUNTIME__
# define __LAMA_RUNTIME__

# include <stdio.h>
# include <stdio.h>
# include <string.h>
# include <stdarg.h>
# include <stdlib.h>
# include <sys/mman.h>
# include <assert.h>
# include <errno.h>
# include <regex.h>
# include <time.h>
# include <limits.h>
# include <ctype.h>

# define WORD_SIZE (CHAR_BIT * sizeof(int))
void* Bsta (void *v, int i, void *x);
void failure (char *s, ...);
void* Belem (void *p, int i);

# endif
