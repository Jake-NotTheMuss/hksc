/*
** $Id: lprintf.c $
** formatted print using a custom write function
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define lprintf_c
#define LUA_CORE

#include "hksclua.h"

#include "llimits.h"
#include "lobject.h"

static int getfarg (va_list *pap, const char **p, int *arg) {
  const char *s = *p;
  *arg = 0;
  if (*s == '*') {
    *arg = va_arg(*pap, int);
    *p = s+1;
    return 1;
  }
  else if (isdigit(*s)) {
    do {
      int d = *s++ - '0';
      if (*arg < 999) *arg = *arg * 10 + d;
    } while (isdigit(*s));
    *p = s;
    return 1;
  }
  return 0;
}

#define HANDLE_H(T) if (qual == 'h') { \
  short T x = cast(short T, va_arg(ap, T)); \
  n = sprintf(buffer, spec.s, x); \
  break; \
}
#define DONT_HANDLE_H(T)  /* empty */

#define HANDLE_CODE(T, Lqual, h) do { \
  h(T); \
  if (qual == Lqual) { \
    long T x = va_arg(ap, long T); \
    n = sprintf(buffer, spec.s, x); \
  } \
  else { \
    T x = va_arg(ap, T); \
    n = sprintf(buffer, spec.s, x); \
  } \
} while (0)

static const char spaces [] = "                                ";

#define MAX_PAD cast_int(sizeof (spaces) - 1)
#define PAD(s, n) do { if (0 < (n)) {int i, j = (n); \
  for (; 0 < j; j -= i) \
    {i = MAX_PAD < j ? MAX_PAD : j; PUT(s, i); } } \
} while (0)
#define PUT(s, n) do { \
  if (0 < (n)) {(*pfn)(s, cast(size_t, n), ud); nchar += (n); } \
} while (0)

enum { FSP = 1, FPL = 2, FMI = 4, FNO = 8, FZE = 16 };

static int putstr (l_PFN pfn, void *ud,
                   const char *s, int width, int prec, int flags) {
  size_t n;
  int nchar = 0;
  if (s == NULL) s = "(null)";
  n = strlen(s);
  if (prec >= 0 && cast(size_t, prec) < n)
    n = cast(size_t, prec);
  if (width < 0) {
    width = -width;
    flags |= FMI;
  }
  if (cast(size_t, width) > n && !(flags & FMI))
    PAD(spaces, cast(size_t, width) - n);
  PUT(s, n);
  if (cast(size_t, width) > n && (flags & FMI))
    PAD(spaces, cast(size_t, width) - n);
  return nchar;
}

int luaO_vprintf (l_PFN pfn, void *ud, const char *fmt, va_list ap) {
  char buffer [1024];  /* enough space to convert a number */
  int nchar = 0;
  struct {
    char s [128];
    int n;
  } spec;
  for (;;) {
    int n = 0, flags, width = 0, prec = -1;
    char code, qual = 0;
    const char *s;
    spec.n = 0;
    spec.s[spec.n++] = '%';  /* begin format string */
    for (s = fmt; *s && *s != '%'; s++)
      ;
    PUT(fmt, s - fmt);
    if (*s == 0)
      break;
    fmt = ++s;
    {  /* parse a conversion specifier */
      const char *t;
      int i;
      static const char fchar [] = " +-#0";
      for (flags = 0; (t = strchr(fchar, *s)) != NULL; ++s)
        flags |= 1 << (t - fchar);
      for (i = 0; i < cast_int(sizeof fchar) - 1; i++)
        if (flags & (1 << i)) spec.s[spec.n++] = fchar[i];
    }
    if (getfarg(&ap, &s, &width))
      spec.n += sprintf(spec.s + spec.n, "%d", width);
    if (*s == '.') {
      spec.s[spec.n++] = *s++;
      if (getfarg(&ap, &s, &prec))
        spec.n += sprintf(spec.s + spec.n, "%d", prec);
    }
    /* qualifier */
    if (strchr("hlL", *s) != NULL)
      spec.s[spec.n++] = qual = *s++;
    code = *s;
    spec.s[spec.n++] = code;
    spec.s[spec.n] = '\0';  /* end of format string */
    switch (code) {
      case 'c': case '%': {
        int c = code == 'c' ? va_arg(ap, int) : '%';
        spec.s[spec.n-1] = 'c';
        n = sprintf(buffer, spec.s, c);
        break;
      }
      case 'd': case 'i':
        HANDLE_CODE(int, 'l', HANDLE_H);
        break;
      case 'o': case 'u': case 'x': case 'X':
        HANDLE_CODE(unsigned, 'l', HANDLE_H);
        break;
      case 'e': case 'E': case 'f': case 'g': case 'G':
        HANDLE_CODE(double, 'L', DONT_HANDLE_H);
        break;
      case 'n':
        if (qual == 'h')
          *va_arg(ap, short *) = nchar;
        else if (qual == 'l')
          *va_arg(ap, long *) = nchar;
        else
          *va_arg(ap, int *) = nchar;
        goto advance;
      case 'p': {
        void *p = va_arg(ap, void *);
        n = sprintf(buffer, spec.s, p);
        break;
      }
      case 's':
        n = putstr(pfn, ud, va_arg(ap, const char *), width, prec, flags);
        goto advance;
      default:  /* undefined specifier */
        n = sprintf(buffer, "%c", code);
    }
    PUT(buffer, n);
    advance: fmt = s+1;
    nchar += n;
  }
  return nchar;
}


int luaO_printf (l_PFN pfn, void *ud, const char *fmt, ...) {
  va_list ap;
  int nchar;
  va_start(ap, fmt);
  nchar = luaO_vprintf(pfn, ud, fmt, ap);
  va_end(ap);
  return nchar;
}
