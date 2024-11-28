/*
** $Id: hkscfile.h
** Building file paths and loading files
** See Copyright Notice in lua.h
*/

#include <stdio.h>

#ifndef hkscfile_h
#define hkscfile_h

extern FILE *xopen (const char *name, const char *mode);
extern int fcmp (const char *a, const char *b, int istext);

#if (defined(_WIN32) && !defined(__CYGWIN__)) || defined(__MSDOS__) || \
  defined(__DJGPP__) || defined(__OS2__)
# define HAVE_DOS_BASED_FILE_SYSTEM
# define IS_DIR_SEP(c)  ((c) == '/' || (c) == '\\')
#else
# undef HAVE_DOS_BASED_FILE_SYSTEM
# define IS_DIR_SEP(c)  ((c) == '/') 
#endif

extern const char *basename (const char *name);

typedef struct Buffer {
  char *buffer;
  size_t n, size;
  size_t oldn;
} Buffer;

extern char *replace_ext (Buffer *b, const char *name, const char *newext);

extern void buff_space (Buffer *b, size_t n);
extern void buff_concat (Buffer *b, const char *str);
extern void buff_concatn (Buffer *b, const char *str, size_t len);
extern void buff_revert (Buffer *b);

#define buff_get(b)  ((b)->buffer)
#define buff_len(b)  ((b)->n)
#define buff_size(b)  ((b)->size)

#endif /* hkscfile_h */
