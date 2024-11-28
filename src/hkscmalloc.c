/*
** $Id: hkscmalloc.c $
** xmalloc and xstrdup
** See Copyright Notice in lua.h
*/

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define hkscmalloc_c

#include "hkscaux.h"

extern const char *progname;

void xerror (const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "%s: ", progname);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  abort();
}

void *xmalloc (size_t n) {
  return xrealloc(NULL, n);
}

void *xrealloc (void *mem, size_t n) {
  mem = realloc(mem, n);
  if (mem == NULL) {
    xerror("%s: cannot allocate %lu: (%s)\n", progname,
           (unsigned long)n, strerror(errno));
  }
  return mem;
}
