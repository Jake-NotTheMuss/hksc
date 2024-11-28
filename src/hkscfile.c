/*
** $Id: hkscfile.c
** Building file paths and loading files
** See Copyright Notice in lua.h
*/

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define hkscfile_c

#include "hkscaux.h"

FILE *xopen (const char *name, const char *mode) {
  FILE *f = fopen(name, mode);
  if (f == NULL)
    xerror("cannot open file '%s': %s\n", name, strerror(errno));
  return f;
}

int fcmp (const char *a, const char *b, int istext) {
  FILE *f1, *f2;
  int result = 0;
  f1 = xopen(a, istext ? "r" : "rb");
  f2 = xopen(b, istext ? "r" : "rb");
  for (;;) {
    int c1 = fgetc(f1), c2 = fgetc(f2);
    if (c1 != c2) {
      result = 1;
      break;
    }
    if (c1 == EOF)
      break;
  }
  fclose(f1);
  fclose(f2);
  return result;
}

const char *basename (const char *name) {
  const char *base;
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
  /* skip over disk name in MSDOS pathnames */
  if (isalpha(name[0]) && name[1] == ':')
    name += 2;
#endif /* HAVE_DOS_BASED_FILE_SYSTEM */
  for (base = name; *name; name++) {
    if (IS_DIR_SEP(*name))
      base = name + 1;
  }
  return base;
}

char *replace_ext (Buffer *b, const char *name, const char *newext) {
  size_t totalsize, n, extlen = strlen(newext);
  const char *ext = NULL;
  for (n = 0; name[n]; n++) {
    if (name[n] == '.') ext = name + n;
    else if (IS_DIR_SEP(name[n])) ext = NULL;
  }
  if (ext)
    n = ext - name;
  totalsize = n + extlen + 1;
  if (totalsize > b->size) {
    b->size = totalsize;
    b->buffer = xrealloc(b->buffer, totalsize);
  }
  memcpy(b->buffer, name, n);
  memcpy(b->buffer + n, newext, extlen);
  n += extlen;
  b->buffer[n] = 0;
  b->n = n;
  return b->buffer;
}

void buff_space (Buffer *b, size_t n) {
  if (b->n + n > b->size) {
    b->size += n;
    b->buffer = xrealloc(b->buffer, b->size);
  }
}

void buff_concat (Buffer *b, const char *str) {
  buff_concatn(b, str, strlen(str));
}

void buff_concatn (Buffer *b, const char *str, size_t len) {
  buff_space(b, len + 1);
  memcpy(b->buffer + b->n, str, len);
  b->oldn = b->n;
  b->n += len;
  b->buffer[b->n] = 0;
}

void buff_revert (Buffer *b) {
  b->n = b->oldn;
}
