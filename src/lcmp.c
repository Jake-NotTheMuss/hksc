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

#ifdef HKSC_TESTING

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

typedef struct {
  hksc_State *H;
  int (*printer)(void *ud, const char *fmt, ...);
  void *ud;
  const char *a, *b;
  int strip;
  int needheader;
  int quote;
  int level;
} CmpState;

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

#define printdiff(m, what, T, printnl, eq_func, print_func) \
do { \
  char buffer [64]; \
  cmpfieldint(size##m); \
  S->needheader = 1; \
  for (i = 0; i < field_min(size##m); i++) { \
    T *t1 = &p1->m[i]; T *t2 = &p2->m[i]; \
    if (!eq_func(t1, t2)) { \
      PUTHEADER(what "s"); \
      (*S->printer)(S->ud, "    [%d]\n      a: ", i+1); \
      print_func(S, p1, t1); \
      if (printnl) \
        (*S->printer)(S->ud, "\n"); \
      (*S->printer)(S->ud, "      b: "); \
      print_func(S, p2, t2); \
      if (printnl) \
        (*S->printer)(S->ud, "\n"); \
    } \
  } \
  if (p1->size##m != p2->size##m) { \
    const Proto *p = p1->size##m > p2->size##m ? p1 : p2; \
    int difference = field_max(size##m) - field_min(size##m); \
    (*S->printer)(S->ud, "  \"%s\" has %d more %s%s:\n", \
                  p == p1 ? S->a : S->b, difference, \
                  str2lower(what, buffer), difference == 1 ? "" : "s"); \
    for (i = field_min(size##m); i < field_max(size##m); i++) { \
      (*S->printer)(S->ud, "    + [%d] ", i+1); \
      print_func(S, p, p->m + i); \
      if (printnl) \
        (*S->printer)(S->ud, "\n"); \
    } \
  } \
} while (0)

#define print_object(S, p, o) do { \
  print_type(S, ttype(o)); \
  luaO_printk(o, printk_f, S, S->quote); \
} while (0)

#define print_instruction(S, p, _) \
  luaU_printcode(S->H, p, i, S->printer, S->ud, S->quote)

#define print_linemapping(S, p, l) (S->printer)(S->ud, "%d", *(l))

#define default_eq(o1, o2) (*(o1) == *(o2))
#define object_eq(o1, o2) luaO_rawequalObj(o1, o2)

static int locvar_eq (const LocVar *v1, const LocVar *v2) {
  if (v1->startpc != v2->startpc)
    return 0;
  if (v1->endpc != v2->endpc)
    return 0;
  return (v1->varname == v2->varname);
}

static void print_locvar (CmpState *S, const Proto *p, const LocVar *var) {
  (void)p;
  (*S->printer)(S->ud, "[%d-%d] %s",
                var->startpc+1, var->endpc+1, getstr(var->varname));
}

#define print_upval(S,p,pts) ((*S->printer)(S->ud, "%s", getstr(*(pts))))

static void cmpfunc (CmpState *S, const Proto *p1, const Proto *p2) {
  int i;
  if (p1 == p2)
    return;
  S->level++;
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
  printdiff(code, "Instruction", Instruction, 0, default_eq,
            print_instruction);
  /* compare constants */
  printdiff(k, "Constant", TValue, 1, object_eq, print_object);
  if (S->strip >= BYTECODE_STRIPPING_PROFILING) {
    cmpfieldint(linedefined);
    cmpfieldint(lastlinedefined);
  }
  /* compare line info */
  if (S->strip >= BYTECODE_STRIPPING_NONE) {
    printdiff(lineinfo, "Line mapping", int,
              1, default_eq, print_linemapping);
    printdiff(locvars, "Local variable", LocVar, 1, locvar_eq, print_locvar);
    printdiff(upvalues, "Upvalue", TString *, 1, default_eq, print_upval);
  }
  /* compare child functions */
  cmpfieldint(sizep);
  if (p1->sizep == p2->sizep) {
    for (i = 0; i < p1->sizep; i++) {
      (*S->printer)(S->ud, "\nComparison of child function (level %d) %s:\n",
                    S->level, p1->name ? getstr(p1->name) : "(anonymous)");
      cmpfunc(S, p1->p[i], p2->p[i]);
    }
  }
  else
    (*S->printer)(S->ud, "\nIgnoring child functions due to count mismatch\n");
  S->level--;
}

void luaO_cmp (hksc_State *H, const Proto *p1, const Proto *p2,
               const char *name1, const char *name2,
              int strip, int (*printer) (void *ud, const char *fmt, ...),
              void *ud) {
  CmpState S;
  S.H = H;
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
  S.level = 0;
  (*printer)(ud, "a \"%s\"\n", name1 ? name1 : "input 1");
  (*printer)(ud, "b \"%s\"\n", name2 ? name2 : "input 2");
  (*printer)(ud, "The following fields differ:\n");
  cmpfunc(&S, p1, p2);
}

#endif /* HKSC_TESTING */
