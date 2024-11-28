/*
** $Id: lcmp.c $
** Comparing function headers
** See Copyright Notice in lua.h
*/

#include <ctype.h>

#define lcmp_c
#define LUA_CORE

#include "hksclua.h"

#include "lobject.h"
#include "lundump.h"

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

typedef struct {
  int (*printer)(void *ud, const char *fmt, ...);
  void *ud;
  const char *a, *b;
  int strip;
  int needheader;
  int quote;
} CmpState;

/*#ifdef LUA_CODT6
#define needprofinfo(S) ((S)->strip == BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION)
#define needdebuginfo(S) ((S)->strip == BYTECODE_STRIPPING_DEBUG_ONLY)
#else
#define needprofinfo(S)
#define needdebuginfo(S)
#endif*/

#define cmpfield(m,f,a,b) \
  if (p1->m != p2->m) \
  (*S->printer)(S->ud, "  %s\n    a: " f "\n    b: " f "\n", #m, a, b)

#define cmpfieldint(m) cmpfield(m, "%-2d", p1->m, p2->m)
#define cmpfieldts(m)  cmpfield(m, "\"%s\"", getstr(p1->m), getstr(p2->m))

#define field_min(m) MIN(p1->m, p2->m)
#define field_max(m) MAX(p1->m, p2->m)

#define PUTHEADER(s) do { \
  if (S->needheader) { \
    (*S->printer)(S->ud, "  %s which differ:\n", s); \
    S->needheader = 0; \
  } \
} while (0)

/* return a readable `is_vararg' value */
static const char *vararg_2str (unsigned int x) {
  static const char *const tab [] = {
    "0",
    "VARARG_HASARG",
    "VARARG_ISVARARG",
    "VARARG_HASARG | VARARG_ISVARARG",
    "VARARG_NEEDSARG",
    "VARARG_HASARG | VARARG_NEEDSARG",
    "VARARG_HASARG | VARARG_ISVARARG",
    "VARARG_HASARG | VARARG_ISVARARG | VARARG_NEEDSARG",
  };
  if (x > 7) x = 0;
  return tab[x];
}

static char *str2lower (const char *s, char *outstr) {
  char *const str = outstr;
  for (; *s; s++)
    *outstr++ = tolower(*s);
  *outstr = 0;
  return str;
}

static int print_type (CmpState *S, int t) {
  char buff[64];
  static const char *const types [] = {
#define DEFTYPE(t) #t,
#include "ltype.def"
#undef DEFTYPE
    NULL
  };
  return (*S->printer)(S->ud, "%*s: ", cast_int(sizeof("TBOOLEAN")),
                       str2lower(types[t]+1, buff));
}

static void printk_f (const char *s, size_t l, void *ud) {
  CmpState *S = ud;
  (*S->printer)(S->ud, "%.*s", cast_int(l), s);
}

static int cmpfunc (CmpState *S, const Proto *p1, const Proto *p2) {
  int i;
  if (p1 == p2)
    return 0;
  /*(*S->printer)(S->ud, )*/
  cmpfieldint(numparams);
  cmpfield(is_vararg, "%s",
           vararg_2str(p1->is_vararg), vararg_2str(p2->is_vararg));
  cmpfieldint(maxstacksize);
  cmpfieldint(nups);
#ifdef LUA_CODT6
  cmpfield(hash, "0x%" LUA_INT_FRMLEN "x", p1->hash, p2->hash);
#endif
  if (S->strip >= BYTECODE_STRIPPING_PROFILING) {
    cmpfieldts(source);
    cmpfieldts(name);
  }
  /* compare code */
  cmpfieldint(sizecode);
  S->needheader = 1;
  /* print differing instructions */
  for (i = 0; i < field_min(sizecode); i++) {
    if (p1->code[i] != p2->code[i]) {
      PUTHEADER("Instructions");
      (*S->printer)(S->ud, "    a: ");
      luaU_printcode(p1, i, S->printer, S->ud, S->quote);
      (*S->printer)(S->ud, "    b: ");
      luaU_printcode(p2, i, S->printer, S->ud, S->quote);
    }
  }
  /* print extra instructions */
  if (p1->sizecode != p2->sizecode) {
    const Proto *p = p1->sizecode > p2->sizecode ? p1 : p2;
    (*S->printer)(S->ud, "  \"%s\" has %d more instructions:\n",
          p == p1 ? S->a : S->b, field_max(sizecode) - field_min(sizecode));
    for (i = field_min(sizecode); i < field_max(sizecode); i++) {
      (*S->printer)(S->ud, "    + ");
      luaU_printcode(p, i, S->printer, S->ud, S->quote);
    }
  }
  /* compare constants */
  cmpfieldint(sizek);
  S->needheader = 1;
  /* print differing constants */
  for (i = 0; i < field_min(sizek); i++) {
    const TValue *o1 = &p1->k[i], *o2 = &p2->k[i];
    if (!luaO_rawequalObj(o1, o2)) {
      PUTHEADER("Constants");
      (*S->printer)(S->ud, "    [%d]\n"
                             "      a: ", i);
      print_type(S, ttype(o1));
      luaO_printk(o1, printk_f, S, S->quote);
      (*S->printer)(S->ud, "\n      b: ");
      print_type(S, ttype(o2));
      luaO_printk(o2, printk_f, S, S->quote);
      (*S->printer)(S->ud, "\n");
    }
  }
  /* print extra constants */
  if (p1->sizek != p2->sizek) {
    const Proto *p = p1->sizek > p2->sizek ? p1 : p2;
    (*S->printer)(S->ud, "  \"%s\" has %d more constants:",
                  p == p1 ? S->a : S->b, field_max(sizek) - field_min(sizek));
    for (i = field_min(sizek); i < field_max(sizek); i++) {
      (*S->printer)(S->ud, "\n    + [%d] ", i);
      print_type(S, ttype(&p->k[i]));
      luaO_printk(&p->k[i], printk_f, S, S->quote);
    }
    (*S->printer)(S->ud, "\n");
  }
  if (S->strip >= BYTECODE_STRIPPING_PROFILING) {
    cmpfieldint(linedefined);
    cmpfieldint(lastlinedefined);
  }
  return 0;
}

int luaO_cmp (const Proto *p1, const Proto *p2, const char *name1,
              const char *name2,
              int strip, int (*printer) (void *ud, const char *fmt, ...),
              void *ud) {
  CmpState S;
  S.printer = printer;
  S.ud = ud;
#ifdef LUA_CODT6
  if (strip == BYTECODE_STRIPPING_CALLSTACK_RECONSTRUCTION)
    strip = BYTECODE_STRIPPING_PROFILING;
  else if (strip == BYTECODE_STRIPPING_DEBUG_ONLY)
    strip = BYTECODE_STRIPPING_NONE;
#endif
  S.strip = strip;
  S.quote = '"';
  S.a = name1; S.b = name2;
  (*printer)(ud, "a \"%s\"\n", name1 ? name1 : "input 1");
  (*printer)(ud, "b \"%s\"\n", name2 ? name2 : "input 2");
  (*printer)(ud, "The following fields differ:\n");
  return cmpfunc(&S, p1, p2);
}
