/*
** $Id: ldecomp.c $
** decompile precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h> /* memset */

#define ldecomp_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"

/* for formatting decimal integers in generated variable names */
#define INT_CHAR_MAX_DEC (3 * sizeof(int) * (CHAR_BIT/8))

#define nodebug_p(H) (Settings(H).ignore_debug)

struct DFuncState;

typedef lu_int32 InstructionFlags;

typedef struct {
  hksc_State *H;
  lua_Writer writer;
  void *data;
  int status;
  /* data for the current function's decompilation */
  int indentlevel; /* the initial indentation level of this function */
  int funcidx; /* n for the nth function that is being decompiled */
  int needspace;
} DecompState;

#define expliteral(e,l) ((e)->str = "" l, (e)->len = sizeof(l)-1, (e))
#define expts(e,ts) ((e)->str = getstr(ts), (e)->len = (ts)->tsv.len, (e))

#define detype(e) ((e)->flags)
#define dereg(e) ((e)->u.reg)
#define detok(e) ((e)->u.token)
#define delocidx(e) ((e)->u.locidx)

/* decompiler expression flags */
enum DEXPTYPE {
  DK = (1 << 0), /* a constant */
  DID = (1 << 1), /* an identifier */
  DTOKEN = (1 << 2), /* a reserved Lua token */
  DLHS = (1 << 3), /* left-hand-side of an assignment */
  DDECL = (1 << 4), /* an identifier in a local variable declaration */
  DRET = (1 << 5), /* expression is returned */
  DLIST = (1 << 6), /* part of an expression list */
  DSTARTFUNC = (1 << 17), /* first node in a function */
  DENDLINE = (1 << 18), /* the last node in a line */
  DENDSTMT = (1 << 19), /* the last node in a statement */
  DENDBLOCK = (1 << 20), /* the last node in a block */
  DCOMMENT = (1 << 21), /* a Lua comment */
  DLONGCOMMENT = (1 << 22) /* a long Lua comment */
};

#define deisk(e)              ((detype(e) & DK) != 0)
#define deisid(e)             ((detype(e) & DID) != 0)
#define deistoken(e)          ((detype(e) & DTOKEN) != 0)
#define deislhs(e)            ((detype(e) & DLHS) != 0)
#define deisdecl(e)           ((detype(e) & DDECL) != 0)
#define deisret(e)            ((detype(e) & DRET) != 0)
#define deislist(e)           ((detype(e) & DLIST) != 0)
#define deisfuncstart(e)      ((detype(e) & DSTARTFUNC) != 0)
#define deislineend(e)        ((detype(e) & DENDLINE) != 0)
#define deisstmtend(e)        ((detype(e) & DENDSTMT) != 0)
#define deisblockend(e)       ((detype(e) & DENDBLOCK) != 0)
#define deiscomment(e)        ((detype(e) & DCOMMENT) != 0)
#define deislongcomment(e)    ((detype(e) & DLONGCOMMENT) != 0)

#define desettype(e,t) ((detype(e) |= (t)))
#define deunsettype(e,t) ((detype(e) &= ~(t)))
#define deresettype(e,t) ((detype(e) = (t)))

#define ID_LOCAL 0
#define ID_UPVALUE 1
#define ID_GLOBAL 2

/* a decompiled expression */
typedef struct DExp {
  struct DExp *prev, *next;
  struct DExp *nextinlist;
  const char *str; /* the expressions's printable form */
  size_t len; /* length of str */
  int flags;
  int pc; /* pc at which this expression was encountered */
  lu_byte istemp; /* true if this expression is stored in a temp register */
  int lines_needed; /* number of line feeds needed after this expression */
  int idtype;
  union {
    int locidx; /* local variable index for identifiers */
    int reg; /* the register that stores this expression */
    int token; /* the token id if this is a reserved token */
  } u;
} DExp;

/* a decompiled function */
typedef struct DFuncState {
  struct DFuncState *prev;
  const Proto *f;
  InstructionFlags *ins_properties;
  struct {
    DExp *first, *last;
  } explist; /* current chain of pending expressions */
  int nexps; /* number of expressions in current chain */
  int idx; /* the nth function */
  hksc_State *H;
  struct LocVar *locvars;  /* information about local variables */
  int locvaridx;
  int nlocvars; /* number of local variables declared so far */
  int ownlocvars; /* true if the locvars array was allocated for this struct */
  int sizelocvars;
  int pc; /* current pc */
  int line; /* actual current line number */
  int linepending; /* line + number of pending new lines */
  int numdecls; /* number of initial implicit nil local variables */
} DFuncState;

static DExp *newexp(DFuncState *fs, int type) {
  DExp *e = luaM_new(fs->H, DExp);
  memset(e, 0, sizeof(DExp));
  desettype(e, type);
  e->pc = fs->pc;
  return e;
}

static void freeexp(DFuncState *fs, DExp *exp) {
  luaM_free(fs->H, exp);
}

static void open_func(DFuncState *fs, const Proto *f, DFuncState *prev,
                      DecompState *D) {
  fs->prev = prev;
  fs->f = f;
  fs->explist.first = fs->explist.last = NULL;
  fs->nexps = 0;
  fs->H = D->H;
  fs->idx = D->funcidx++;
  fs->nlocvars = 0;
  if (!nodebug_p(fs->H) && f->sizelineinfo > 0) { /* have debug info */
    printf("using debug info for function '%s'\n", f->name ? getstr(f->name) : "(anonymous)");
    fs->ownlocvars = 0;
    fs->sizelocvars = f->sizelocvars;
    fs->locvars = f->locvars;
  }
  else {
    fs->ownlocvars = 1;
    fs->sizelocvars = f->maxstacksize;
    fs->locvars = luaM_newvector(fs->H, fs->sizelocvars, struct LocVar);
  }
  fs->locvaridx = fs->sizelocvars - 1;
  fs->line = (prev != NULL) ? prev->linepending : 1;
  fs->linepending = fs->line;
  fs->ins_properties = luaM_newvector(fs->H, f->sizecode, InstructionFlags);
  memset(fs->ins_properties, 0, f->sizecode * sizeof(InstructionFlags));
}

static void append_func(DFuncState *fs1, DFuncState *fs2);

static void close_func(DFuncState *fs, DecompState *D) {
  lua_assert(fs->explist.first == NULL);
  lua_assert(fs->explist.last == NULL);
  lua_assert(fs->nexps == 0);
  lua_assert(fs->line == fs->linepending);
  D->funcidx--;
  if (fs->ownlocvars)
    luaM_freearray(fs->H, fs->locvars, fs->sizelocvars, struct LocVar);
  UNUSED(fs->locvars);
  UNUSED(fs->sizelocvars);
  UNUSED(fs->ownlocvars);
  if (fs->prev != NULL) { /* update line number for parent */
    DFuncState *prev = fs->prev;
    int pending = prev->linepending - prev->line;
    prev->line = fs->line;
    prev->linepending = prev->line + pending;
    append_func(prev, fs);
  }
  luaM_freearray(fs->H, fs->ins_properties, fs->f->sizecode, InstructionFlags);
}

#define match_lines_p(D) \
  (Settings((D)->H).match_line_info && !Settings((D)->H).ignore_debug)

#define DumpLiteral(s,D) DumpBlock("" s, sizeof(s)-1, D)
#define DumpString(s,D) DumpBlock(s, strlen(s), D)

static void DumpBlock(const void *b, size_t size, DecompState *D)
{
  if (D->status==0)
  {
    lua_unlock(D->H);
    D->status=(*D->writer)(D->H,b,size,D->data);
    lua_lock(D->H);
  }
}

static void DumpExp(DExp *exp, DecompState *D)
{
  lua_assert(exp->str != NULL);
  lua_assert(exp->len > 0);
  DumpBlock(exp->str, exp->len, D);
}

static void DumpStringf(DecompState *D, const char *fmt, ...)
{
  va_list argp;
  const char *s;
  va_start(argp, fmt);
  s = luaO_pushvfstring(D->H, fmt, argp);
  DumpString(s,D);
  va_end(argp);
}

#define createvarname(type) \
  char buff[sizeof("f_" type) + (2 * INT_CHAR_MAX_DEC)]; \
  lua_assert(i >= 0 && i < LUAI_MAXVARS); \
  lua_assert(fs->idx >= 0); \
  sprintf(buff, "f%d_" type "%d", fs->idx, i); \
  return luaS_new(fs->H, buff)

/* when debug info is not present/used, generate a local variable name */
static TString *createlocvarname(DFuncState *fs, int i) {
  createvarname("local");
}

/* when debug info is not present/used, generate a argument variable name */
static TString *createargname(DFuncState *fs, int i) {
  createvarname("arg");
}

#undef createvarname

#define DumpAugVar(fs,n,D,s) DumpStringf((D), "f%d_" s "%d", (fs)->idx, (n))
#define DumpAugLocVar(fs,n,D) DumpAugVar(fs,n,D,"local")
#define DumpAugArgVar(fs,n,D) DumpAugVar(fs,n,D,"arg")

#define NextLine(fs,D) AddLines(fs,1,D)
static void AddLines(DFuncState *fs, int n, DecompState *D)
{
  int i;
  for (i=0; i<n; i++)
    DumpLiteral("\n",D);
  fs->line+=n;
}

/* begin a new statement */
static void begin_stmt(DFuncState *fs) {
  DExp *curr = fs->explist.last;
  if (curr != NULL)
    desettype(curr, DENDSTMT);
}

/* begin a new line */
static void begin_line(DFuncState *fs, int n, DecompState *D) {
  DExp *curr = fs->explist.last;
  lua_assert(n >= 0);
  fs->linepending += n;
  if (curr != NULL && n > 0) {
    desettype(curr, DENDLINE);
    curr->lines_needed = n;
  }
  else {
    AddLines(fs, n, D);
    lua_assert(fs->line == fs->linepending);
  }
}

/* when line info is not present/used, check if a new line is needed */
static void maybe_begin_line(DFuncState *fs, DecompState *D) {
  DExp *curr = fs->explist.last;
  if (curr != NULL && (deisstmtend(curr) || deisblockend(curr))) {
    printf("Adding a new line before the next node\n");
    begin_line(fs, 1, D);
  }
}

static void append_exp(DFuncState *fs, DExp *exp) {
  DExp *last = fs->explist.last;
  fs->explist.last = exp;
  exp->prev = last;
  exp->next = NULL;
  if (last != NULL)
    last->next = exp;
  fs->nexps++;
  if (fs->explist.first == NULL)
    fs->explist.first = fs->explist.last;
}

static void prepend_exp(DFuncState *fs, DExp *exp) {
  DExp *first = fs->explist.first;
  fs->explist.first = exp;
  exp->prev = NULL;
  exp->next = first;
  if (first)
    first->prev = exp;
  fs->nexps++;
  if (fs->explist.last == NULL)
    fs->explist.last = fs->explist.first;
}

static void insert_exp_before(DFuncState *fs, DExp *pos, DExp *exp) {
  DExp *prev;
  lua_assert(pos != NULL);
  if (pos == fs->explist.first) {
    prepend_exp(fs, exp);
    return;
  }
  prev = pos->prev;
  if (prev != NULL)
    prev->next = exp;
  exp->prev = prev;
  exp->next = pos;
  pos->prev = exp;
  fs->nexps++;
}

static void insert_exp_after(DFuncState *fs, DExp *pos, DExp *exp) {
  DExp *next;
  lua_assert(pos != NULL);
  if (pos == fs->explist.last) {
    append_exp(fs, exp);
    return;
  }
  next = pos->next;
  if (next != NULL)
    next->prev = exp;
  exp->prev = pos;
  exp->next = next;
  pos->next = exp;
  fs->nexps++;
}

static void remove_exp(DFuncState *fs, DExp *exp) {
  DExp *prev, *next;
  prev = exp->prev;
  next = exp->next;
  if (prev != NULL) 
    prev->next = next;
  else
    fs->explist.first = next;
  if (next != NULL)
    next->prev = prev;
  else
    fs->explist.last = prev;
  freeexp(fs, exp);
  fs->nexps--;
}

static void append_func(DFuncState *fs1, DFuncState *fs2) {
  DExp *first, *last;
  first = fs2->explist.first;
  last = fs2->explist.last;
  lua_assert(deisfuncstart(first));
  lua_assert(deisblockend(last));
  append_exp(fs1, first);
  fs1->explist.last = last;
}

static int empty_chain(DFuncState *fs) {
  if (fs->explist.first == NULL) {
    lua_assert(fs->explist.last == NULL);
    lua_assert(fs->nexps == 0);
    return 1;
  }
  else {
    lua_assert(fs->explist.last != NULL);
    lua_assert(fs->nexps > 0);
    return 0;
  }
}

static void setlhs(DFuncState *fs, DExp *exp) {
#if 0
  if (fs->nexps >= 2) {
    DExp *second = fs->explist.first->next;
    lua_assert(deislhs(second)); /* LHS cannot already be set */
  }
#endif
  desettype(exp, DLHS);
  prepend_exp(fs, exp);
}

/* access a local variable; all local variable operations go through here */
static DExp *locvar2exp(DFuncState *fs, int i) {
  struct LocVar *var;
  DExp *exp;
  lua_assert(i < fs->sizelocvars);
  var = &fs->locvars[i];
  exp = newexp(fs, DID);
  exp->idtype = ID_LOCAL;
  delocidx(exp) = i;
  lua_assert(var->varname != NULL);
  expts(exp, var->varname);
  return exp;
}

/* prepend a local variable declaration as an LHS in an assignment */
static DExp *makedecl(DFuncState *fs, DExp *exp) {
  DExp *decl;
  int r = dereg(exp);
  struct LocVar *var;
  int idx = fs->nlocvars++;
  lua_assert(r < fs->f->maxstacksize);
  lua_assert(idx < fs->sizelocvars);
  var = &fs->locvars[idx];
  if (fs->ownlocvars) {
    var->startpc = fs->pc; /* todo: is this always the case? */
    var->endpc = fs->f->sizecode;
    var->varname = createlocvarname(fs, idx);
  }
  decl = locvar2exp(fs, idx);
  delocidx(decl) = idx;
  desettype(decl, DLHS | DDECL);
  if (!fs->ownlocvars) {
    /*lua_assert(var->startpc == fs->pc);*/ /* todo: this isn't always true */
  }
  return decl;
}

/* prenend a declaration node to an expression, making it an assignment RHS */
static void adddecl(DFuncState *fs, DExp *exp) {
  insert_exp_before(fs, exp, makedecl(fs, exp));
}

/* true if EXP follows a node of type DDECL */
static int isdecl(DExp *exp) {
  DExp *prev = exp->prev;
  return (prev != NULL && deisdecl(prev));
}

/* remove a preceding declaration node from an expression if present; this is
   needed when the decompiler encounters an instruction that would augment the
   previous declaration(s), such as a forloop or certain complex arithmetic
   expressions */
static void removedecl(DFuncState *fs, DExp *exp) {
  if (isdecl(exp)) {
    remove_exp(fs, exp->prev);
  }
}

static void DumpExpList(DFuncState *fs, DExp *root, DecompState *D)
{
  DExp *exp = root;
  if (exp->nextinlist != NULL)
    lua_assert(deislist(exp));
  DumpExp(exp,D);
  exp = exp->nextinlist;
  while (exp != NULL) {
    DExp *next;
    lua_assert(deislist(exp));
    DumpLiteral(", ",D);
    DumpExp(exp,D);
    next = exp->nextinlist;
    freeexp(fs, exp);
    exp = next;
  }
  root->nextinlist = NULL;
}

static void DumpNode(DFuncState *fs, DExp *exp, DecompState *D)
{
  lua_assert(exp != NULL);
  while (D->needspace > 0) {
    DumpLiteral(" ", D);
    D->needspace--;
  }
  if (deisdecl(exp)) { /* new local variable */
    printf("   Dumping declaration node\n");
    DumpLiteral("local ",D);
    DumpExpList(fs, exp, D);
  }
  else if (deisret(exp)) { /* return statement */
    DumpLiteral("return",D);
    if (exp->str != NULL) { /* returns a value */
      printf("   Dumping returned expression '%s'\n", exp->str);
      DumpLiteral(" ",D);
      DumpExp(exp,D);
    }
    else printf("   Dumping return statement\n");
  }
  else {
    DumpExp(exp,D);
  }
  if (deislhs(exp)) DumpLiteral(" = ",D); /* assignment follows */
  if (deisstmtend(exp)) {
    DumpLiteral(";",D); /* end of statement */
    if (!deislineend(exp))
      D->needspace = 1;
  }
  if (deislineend(exp)) {
    lua_assert(exp->lines_needed > 0);
    AddLines(fs,exp->lines_needed,D);
  }
}

/* discharge the pending expression chain */
static void discharge(DFuncState *fs, DecompState *D) {
  DExp *exp = fs->explist.first;
  printf("--> DISCHARGING %d expression nodes:\n", fs->nexps);
  while (exp != NULL) {
    DExp *next;
    DumpNode(fs,exp,D);
    next = exp->next;
    freeexp(fs, exp);
    exp = next;
    fs->nexps--;
  }
  lua_assert(fs->nexps == 0);
  fs->explist.first = fs->explist.last = NULL;
}

static void addtolist(DFuncState *fs, DExp *e1, DExp *e2) {
  UNUSED(fs);
  e1->nextinlist = e2;
  desettype(e1, DLIST);
  desettype(e2, DLIST);
}

static DExp *nil2exp(DFuncState *fs, int i, DecompState *D)
{
  DExp *exp = newexp(fs, DK);
  UNUSED(D);
  expliteral(exp, "nil");
  dereg(exp) = i;
  return exp;
}

/* dump N nil variable declarations before dumping a function's code */
static void dump_initial_decl(DFuncState *fs, int i, int n, DecompState *D)
{
  int line, lines_avail;
  int declline; /* what line is the current declaration on */
  /* assertions: this only happens at the start of function */
  lua_assert(empty_chain(fs));
  lua_assert(fs->line == fs->linepending);
  line = getline(fs->f,0);
  declline = fs->linepending;
  if (line > 0 && match_lines_p(D)) {
    lines_avail = line - fs->line; /* keep in sync with line info */
    if (lines_avail > n)
      declline += lines_avail - n;
  }
  else {
    lines_avail = n; /* not matching line info, one line for each decl */
  }
  printf("line info line = %d\n", line);
  printf("declline = %d, fs->linepending = %d\n", declline, fs->linepending);
  begin_line(fs, declline - fs->linepending, D);
  for (; i < n; i++) {
    DExp *nil, *decl;
    if (lines_avail == 1) break;
    nil = nil2exp(fs, i, D); /* nil value */
    decl = makedecl(fs, nil); /* the declaration*/
    append_exp(fs, decl);
    append_exp(fs, nil);
    begin_stmt(fs);
    if (lines_avail > 0) {
      int nlines = ++declline - fs->linepending;
      lines_avail-=nlines;
      begin_line(fs, nlines, D);
    }
  }
  if (i < n && lines_avail == 1) {
    DExp *nil = nil2exp(fs, i, D);
    DExp *lastdecl = NULL;
    DExp *rootdecl;
    for (; i < n; i++) {
      DExp *decl;
      dereg(nil) = i;
      decl = makedecl(fs, nil);
      if (lastdecl != NULL)
        addtolist(fs, lastdecl, decl);
      else
        rootdecl = decl;
      lastdecl = decl;
    }
    append_exp(fs, rootdecl);
    append_exp(fs, nil);
    begin_stmt(fs);
    begin_line(fs, 1, D);
  }
  discharge(fs,D);
  if (match_lines_p(D)) lua_assert(fs->line == line);
}

/* check for implicit nil variable declarations at the start of the function */
static void implicit_decl(DFuncState *fs, int n, DecompState *D)
{
  int pc;
  int numparams = fs->f->numparams;
  /* find the first instruction that usesA as a register, check if it is > 0 */
  for (pc=0; pc<n; pc++)
  {
    Instruction i=fs->f->code[pc];
    OpCode o=GET_OPCODE(i);
    int a=GETARG_A(i);
    if (testAMode(o)) {
      if ((a - numparams) > 0) {
        printf("Should load nil into register %d-%d\n", numparams, a-1);
        dump_initial_decl(fs, numparams, a, D);
      }
      break;
    }
  }
}

/* retrieve a global variable name from the given constant table index */
static DExp *glob2exp(DFuncState *fs, int i, DecompState *D)
{
  DExp *exp = newexp(fs, DID);
  TString *name = rawtsvalue(&fs->f->k[i]); /* global name */
  UNUSED(D);
  return expts(exp, name);
}

/* constant handler: convert a TValue to a printable string */
static DExp *k2exp(DFuncState *fs, int i, DecompState *D)
{
  const Proto *f = fs->f;
  DExp *exp = newexp(fs, DK);
  const TValue *o=&f->k[i];
  TString *result;
  switch (ttype(o))
  {
    case LUA_TNIL:
      return expliteral(exp, "nil");
    case LUA_TBOOLEAN:
      if (bvalue(o)) return expliteral(exp, "true");
      else return expliteral(exp, "false");
    case LUA_TLIGHTUSERDATA: {
      char s[LUAI_MAXUI642STR+sizeof("0xhi")-1];
      sprintf(s, "0x%zxhi", cast(size_t, pvalue(o)));
      result = luaS_new(D->H, s);
      break;
    }
    case LUA_TNUMBER: {
      char s[LUAI_MAXNUMBER2STR];
      sprintf(s, "%g", nvalue(o));
      result = luaS_new(D->H, s);
      break;
    }
    case LUA_TSTRING:
      result = luaO_kstring2print(D->H, rawtsvalue(o));
      break;
    case LUA_TUI64: {
      char s[LUAI_MAXUI642STR+sizeof("0xhl")-1];
      lua_ui642str(s+2, ui64value(o));
      s[0] = '0'; s[1] = 'x';
      strcat(s, "hl");
      result = luaS_new(D->H, s);
      break;
    }
    default: {
      char s[10];
      lua_assert(0);
      sprintf(s, "? type=%d", ttype(o));
      result = luaS_new(D->H, s);
      break;
    }
  }
  return expts(exp, result);
}

enum INS_FLAG {
  INS_FJT = 0,
  INS_BJT,
  INS_PRECALL, /* first pc that sets up a function call */
  INS_PRERETURN, /* first pc that evaluates a returned expression */
  INS_BRANCHFAIL, /* false-jump out of an if-statement condition evaluation */
  INS_BRANCHPASS, /* true-jump out of an if-statement condition evaluation */
  INS_LOOPFAIL, /* false-jump out of a loop condition evaluation */
  INS_LOOPPASS, /* true-jump out of a loop condition evaluation */
  INS_OPTLOOPFAILEXIT, /* optimized jump target of a loop fail */
  INS_REPEATSTAT, /* first pc in a repeat-loop */
  INS_WHILESTAT, /* first pc in a while-loop */
  INS_WHILEEXIT, /* a jump instruction in a while-loop condition */
  INS_IFSTAT, /* first pc in an if-branch */
  INS_ELSEIFSTAT, /* first pc in an elseif-branch */
  INS_ELSESTAT, /* first pc in an else-branch */
  INS_IFSTATEND, /* exit target of an if-statement */
  INS_FORLIST, /* first pc in a list for-loop */
  INS_PREFORLIST, /* first pc to evaluate for-list control variables */
  INS_FORNUM, /* first pc in a numeric for-loop */
  INS_PREFORNUM, /* first pc to evluate for-num control variables */
  INS_BLOCKEND, /* last pc in a block */
  INS_LOOPEND, /* last pc in a loop */
  INS_BREAKSTAT, /* pc is a break instruction */
  INS_WHILESTATEND, /* last pc in a while-loop, to help detect if-statements */
  INS_REPEATSTATEND, /* last pc in a reoeat-loop */
  INS_NEWREG, /* first pc to use a given register */
  /* end of instruction flags */
  MAX_INS
};

static const char *const ins_flag_names[MAX_INS+1] = {
  "INS_FJT",
  "INS_BJT",
  "INS_PRECALL",
  "INS_PRERETURN",
  "INS_BRANCHFAIL",
  "INS_BRANCHPASS",
  "INS_LOOPFAIL",
  "INS_LOOPPASS",
  "INS_OPTLOOPFAILEXIT",
  "INS_REPEATSTAT",
  "INS_WHILESTAT",
  "INS_WHILEEXIT",
  "INS_IFSTAT",
  "INS_ELSEIFSTAT",
  "INS_ELSESTAT",
  "INS_IFSTATEND",
  "INS_FORLIST",
  "INS_PREFORLIST",
  "INS_FORNUM",
  "INS_PREFORNUM",
  "INS_BLOCKEND",
  "INS_LOOPEND",
  "INS_BREAKSTAT",
  "INS_WHILESTATEND",
  "INS_REPEATSTATEND",
  "INS_NEWREG",
  /* end of instruction flags */
  "MAX_INS"
};

#define test_ins_property(fs,pc,val) \
  (((fs)->ins_properties[(pc)] & (1 << (val))) != 0)

#define set_ins_property(fs,pc,val) do { \
  printf("  marking pc (%d) as %s\n", (pc)+1, ins_flag_names[(val)]); \
  printf("  previous flags for pc (%d):", (pc)+1); \
  { \
    int flag_index_; \
    for (flag_index_ = 0; flag_index_ < MAX_INS; flag_index_++) { \
      if ((fs)->ins_properties[(pc)] & (1 << flag_index_)) \
        printf(" %s", ins_flag_names[(flag_index_)]); \
    } \
    printf("\n"); \
  } \
  (fs)->ins_properties[(pc)] |= (1 << (val)); \
} while (0)

#define unset_ins_property(fs,pc,val) do { \
  (fs)->ins_properties[(pc)] &= ~(1 << (val)) \
} while (0)

#define init_ins_property(fs,pc,val) do { \
  lua_assert(!test_ins_property(fs,pc,val)); \
  set_ins_property(fs,pc,val); \
} while (0)

#define CODE_LOOP_DECL(arrayvar, pcvar) \
  Instruction i=(arrayvar)[(pcvar)]; \
  OpCode o=GET_OPCODE(i); \
  int a=GETARG_A(i); \
  int b=GETARG_B(i); \
  int c=GETARG_C(i); \
  int bx=GETARG_Bx(i); \
  int sbx=GETARG_sBx(i)

/* create parameter names for a function if debug info does not provide them */
static void addargs(const Proto *f, DFuncState *fs) {
  struct LocVar *var;
  int i;
  for (i = 0; i < f->numparams; i++) {
    var = &fs->locvars[i];
    if (fs->ownlocvars) {
      var->startpc = 0;
      var->endpc = f->sizecode-1;
      var->varname = createargname(fs, i);
    }
    else {
      lua_assert(var->startpc == 0);
      lua_assert(var->endpc == f->sizecode - 1);
    }
    lua_assert(var->varname != NULL && getstr(var->varname) != NULL);
  }
  fs->nlocvars = i;
}

/* create a new local variable for a function; call this when the analyzer is
   certain of the existence and startpc of the variable */
static int addlocalvar(DFuncState *fs, int pc) {
  struct LocVar *var;
  int startpc;
  int idx = fs->nlocvars++;
  lua_assert(idx < fs->sizelocvars);
  var = &fs->locvars[idx];
  if (fs->ownlocvars) {
    var->startpc = pc;
    var->varname = createlocvarname(fs, idx - fs->f->numparams);
  }
  else {
    lua_assert(var->startpc == pc); /* analyzer must be certain */
  }
  startpc = var->startpc;
  lua_assert(var->varname != NULL && getstr(var->varname) != NULL);
  return startpc;
}

/* create a new local variable for a function; call this when the analyzer is
   NOT certain of the existence and startpc of the variable */
static int maybe_addlocvar(DFuncState *fs, int pc) {
  struct LocVar *var;
  int idx = fs->nlocvars;
  /* debug info will tell whether this is actually a local variable */
  if (!fs->ownlocvars && idx < fs->sizelocvars) {
    var = &fs->locvars[idx];
    if (var->startpc == pc) /* yes */
      return addlocalvar(fs, pc);
    /* fallthrough */
  }
  /* either debug info tells you the analyzer was wrong, or there is no debug
     info, in which case the analyzer may have been correct, but still assume
     it was wrong to avoid cluttering the decompilation with extra generated
     variable names (the decomp will be semantically equivalent in any case) */
  return -1;
}

#if 0
/* pc is what the decompiler thinks is the startpc */
static int pass1_addlocalvar(DFuncState *fs, int pc) {
  struct LocVar *var;
  int startpc;
  int idx = fs->nlocvars;
  if (idx == fs->sizelocvars && !fs->ownlocvars)
    return -1; /* debug info tells you it is a temporary register */
  fs->nlocvars++;
  lua_assert(idx < fs->sizelocvars); /* otherwise this is true */
  var = &fs->locvars[idx];
  if (fs->ownlocvars) {
    var->startpc = pc;
    var->varname = createlocvarname(fs, idx - fs->f->numparams);
    startpc = pc;
  }
  else { /* have debug info */
    startpc = var->startpc;
  }
  lua_assert(var->varname != NULL && getstr(var->varname) != NULL);
  return startpc;
}
#endif

static int pass2_endlocalvar(DFuncState *fs, int idx, int pc) {
  struct LocVar *var;
  int endpc;
  lua_assert(idx < fs->nlocvars);
  var = &fs->locvars[idx];
  if (fs->ownlocvars) {
    var->endpc = pc;
    endpc = pc;
  }
  else { /* have debug info */
    endpc = var->endpc;
  }
  return endpc;
}

static void dumploopinfo(const Proto *f, DFuncState *fs, DecompState *D)
{
  int pc;
  const Instruction *code = f->code;
  int sizecode = f->sizecode;
  for (pc = 0; pc < sizecode - 1; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    /*CODE_LOOP_DECL(code,pc);*/
    if (test_ins_property(fs, pc, INS_REPEATSTAT))
      DumpLiteral("BEGIN REPEAT\n", D);
    if (test_ins_property(fs, pc, INS_WHILESTAT))
      DumpLiteral("BEGIN WHILE\n", D);
    if (test_ins_property(fs, pc, INS_IFSTAT))
      DumpLiteral("BEGIN IF\n", D);
    if (test_ins_property(fs, pc, INS_ELSEIFSTAT))
      DumpLiteral("BEGIN ELSEIF\n", D);
    if (test_ins_property(fs, pc, INS_ELSESTAT))
      DumpLiteral("BEGIN ELSE\n", D);
    if (test_ins_property(fs, pc, INS_PREFORLIST))
      DumpLiteral("PRE FORLIST\n", D);
    if (test_ins_property(fs, pc, INS_FORLIST))
      DumpLiteral("BEGIN FORLIST\n", D);
    if (test_ins_property(fs, pc, INS_PREFORNUM))
      DumpLiteral("PRE FORNUM\n", D);
    if (test_ins_property(fs, pc, INS_FORNUM))
      DumpLiteral("BEGIN FORNUM\n", D);
    if (test_ins_property(fs, pc, INS_BRANCHFAIL))
      DumpLiteral("BRANCH FAIL\n", D);
    else if (test_ins_property(fs, pc, INS_BRANCHPASS))
      DumpLiteral("BRANCH PASS\n", D);
    if (test_ins_property(fs, pc, INS_LOOPFAIL))
      DumpLiteral("LOOP FAIL\n", D);
    else if (test_ins_property(fs, pc, INS_LOOPPASS))
      DumpLiteral("LOOP PASS\n", D);
    if (test_ins_property(fs, pc, INS_PRECALL))
      DumpLiteral("PRE FUNCTION CALL\n", D);
    if (test_ins_property(fs, pc, INS_PRERETURN))
      DumpLiteral("PRE RETURN\n", D);
    if (test_ins_property(fs, pc, INS_BREAKSTAT))
      DumpLiteral("BREAK\n", D);
    if (test_ins_property(fs, pc, INS_BLOCKEND))
      DumpLiteral("END BLOCK\n", D);
    if (test_ins_property(fs, pc, INS_WHILESTATEND))
      DumpLiteral("END WHILE\n", D);
    else if (test_ins_property(fs, pc, INS_REPEATSTATEND))
      DumpLiteral("END REPEAT\n", D);
    else if (test_ins_property(fs, pc, INS_LOOPEND))
      DumpLiteral("END LOOP\n", D);
    DumpStringf(D, "\t%d\t%s\n", pc+1, luaP_opnames[o]);
  }
}


/*
** Check if the given operation O clobbers register A without depending on what
** was previously in register A. if CHECKDEP is false, the check will not take
** into account dependencies on 
*/
static int beginseval(OpCode o, int a, int b, int c, int checkdep) {
  switch (o) {
    /* these operations never depend on what's in A */
    case OP_GETGLOBAL:
    case OP_LOADBOOL:
    case OP_LOADK:
    case OP_LOADNIL:
    case OP_GETUPVAL:
    case OP_NEWTABLE:
    case OP_CLOSURE:
    case OP_VARARG:
      return 1;
    /* these operations depend on what's in B and B may equal A */
    case OP_GETFIELD: case OP_GETFIELD_R1:
    case OP_MOVE:
    case OP_UNM:
    case OP_NOT: case OP_NOT_R1:
    case OP_LEN:
    case OP_TESTSET:
      return !checkdep || a != b;
    /* these operations depend on what's in B and C and B or C may equal A */
    case OP_SELF:
    case OP_GETTABLE_S:
    case OP_GETTABLE_N:
    case OP_GETTABLE:
    case OP_CONCAT:
    /* arithmetic operations */
    case OP_ADD: case OP_ADD_BK:
    case OP_SUB: case OP_SUB_BK:
    case OP_MUL: case OP_MUL_BK:
    case OP_DIV: case OP_DIV_BK:
    case OP_MOD: case OP_MOD_BK:
    case OP_POW: case OP_POW_BK:
#ifdef LUA_CODT7
    case OP_LEFT_SHIFT: case OP_LEFT_SHIFT_BK:
    case OP_RIGHT_SHIFT: case OP_RIGHT_SHIFT_BK:
    case OP_BIT_AND: case OP_BIT_AND_BK:
    case OP_BIT_OR: case OP_BIT_OR_BK:
#endif /* LUA_CODT7 */
      return !checkdep || (a != b && a != c);
    /* the remaining operations do not clobber A or do depend on A */
    default:
      return 0;
  }
}

typedef struct CodeAnalyzer {
  int whilestatstart; /* start pc of current while loop */
  int whilestatend; /* end pc of current while loop */
  int inforloop; /* number of forloops currently active */
  int inwhileloop; /* number of while loops currently active */
  int pc; /* current pc */
  lu_byte prevTMode; /* previous pc is a test instruction */
  int ret_pending; /* a returned register, which may be temporary */
  int lastexp; /* pc of omst recently encountered beginning of an expression */
  int datacount; /* number of OP_DATA instructions after OP_CLOSURE */
  lu_byte *reg_properties;
  const Instruction *code;
} CodeAnalyzer;

/* 
** Check if the given jump at PC with TARGET is after the start of a loop.
** TARGET-1 must be marked as the end of a loop.
*/
static int pcinloop(CodeAnalyzer *ca, DFuncState *fs, int pc, int target) {
  int loopstart;
  int sbx = GETARG_sBx(ca->code[target-1]);
  lua_assert(test_ins_property(fs, target - 1, INS_LOOPEND));
  lua_assert(GET_OPCODE(ca->code[pc]) == OP_JMP);
  lua_assert(sbx < 0);
  lua_assert(pc < target);
  loopstart = target + sbx;
  /* avoid a long assertion string */
  if (!test_ins_property(fs, loopstart, INS_WHILESTAT) &&
      !test_ins_property(fs, loopstart, INS_FORLIST) &&
      !test_ins_property(fs, loopstart, INS_FORNUM) &&
      !test_ins_property(fs, loopstart, INS_REPEATSTAT)) {
    lua_assert(0); /* cannot happen */
  }
  return loopstart <= pc;
}

/*
** Ask the debug info if the next local variable starts at PC
*/
static int varstartshere1(CodeAnalyzer *ca, DFuncState *fs, int pc) {
  int n = 0;
  struct LocVar *var;
  int idx = fs->locvaridx;
  UNUSED(ca);
  lua_assert(idx < fs->sizelocvars);
  if (!fs->ownlocvars) { /* have local variable info */
    var = &fs->locvars[idx];
    while (var->startpc == pc) {
      idx = --fs->locvaridx;
      var = &fs->locvars[idx];
      n++;
    }
  }
  if (n) {
          printf("\t%d variable%s start%s at %d\n", n,
                 (n>1)?"s":"",(n==1)?"s":"", pc+1);
  }
  return n;
}
#if 0
/* create a new local variable for a function; call this when the analyzer is
   certain of the existence and startpc of the variable */
static int addlocalvar(DFuncState *fs, int pc) {
  struct LocVar *var;
  int startpc;
  int idx = fs->nlocvars++;
  lua_assert(idx < fs->sizelocvars);
  var = &fs->locvars[idx];
  if (fs->ownlocvars) {
    var->startpc = pc;
    var->varname = createlocvarname(fs, idx - fs->f->numparams);
  }
  else {
    lua_assert(var->startpc == pc); /* analyzer must be certain */
  }
  startpc = var->startpc;
  lua_assert(var->varname != NULL && getstr(var->varname) != NULL);
  return startpc;
}

/* create a new local variable for a function; call this when the analyzer is
   NOT certain of the existence and startpc of the variable */
static int maybe_addlocvar(DFuncState *fs, int pc) {
  struct LocVar *var;
  int idx = fs->nlocvars;
  /* debug info will tell whether this is actually a local variable */
  if (!fs->ownlocvars && idx < fs->sizelocvars) {
    var = &fs->locvars[idx];
    if (var->startpc == pc) /* yes */
      return addlocalvar(fs, pc);
    /* fallthrough */
  }
  /* either debug info tells you the analyzer was wrong, or there is no debug
     info, in which case the analyzer may have been correct, but still assume
     it was wrong to avoid cluttering the decompilation with extra generated
     variable names (the decomp will be semantically equivalent in any case) */
  return -1;
}

#define REG_ISVAR      0 /* register is a local variable at the current pc */
#define REG_MAYBETEMP  1 /* register is assumed to be temporary */
#define REG_ISTEMP     2 /* register is temporary */

static int locvar1(CodeAnalyzer *ca, DFuncState *fs, int reg)
{
  const Proto *f = fs->f;
  struct LocVar *var;
  int startpc;
  int idx = fs->locvaridx;
  var = &fs->locvars[idx];
  lua_assert(reg >= 0 && reg < f->maxstacksize);
  /* debug info will tell whether this is actually a local variable */
  if (!fs->ownlocvars) {
    if (var->startpc == ca->pc) {
      /* yes */
    }
    else {
      lua_assert(ca->reg_properties[reg] != REG_ISVAR);
    }
  }
  if (ca->reg_properties[reg] == REG_ISTEMP) {
    ;
  }
}
#endif
/*
** Check if the given instruction I begins a temporary expression in register
** FISRTREG. PC is the pc of I. JUMPLIMIT is a pc used to check if a jump
** instruction jumps to somewhere within the temporary expression evaluation,
** meaning it is part of the temporary expression. CODE is the instruction array
** which contains I, used for look-behinds.
*/
static int beginstempexpr(const Instruction *code, Instruction i, int pc,
                           int firstreg, int jumplimit)
{
  OpCode o = GET_OPCODE(i);
  int a = GETARG_A(i);
  int b = GETARG_B(i);
  int c = GETARG_C(i);
  switch (o) {
    case OP_JMP: {
      int sbx = GETARG_sBx(i);
      lua_assert(sbx >= 0);
      lua_assert(pc + 1 + sbx < jumplimit);
      return 0;
    }
    case OP_LOADBOOL:
      /* OP_LOADBOOL may begin an expression, unless it is preceded by another
         OP_LOADBOOL with argC == 1 */
      if ((pc-1) >= 0) {
        Instruction prev = code[pc-1];
        if (GET_OPCODE(prev) == OP_LOADBOOL && GETARG_C(prev))
          return 0;
      }
      /* fallthrough */
    default:
      /* OP_TESTSET is handled in `beginseval' */
      if (testTMode(o) && o != OP_TESTSET) { /* conditional expression */
        if ((pc-1) >= 0 && GET_OPCODE(code[pc-1]) == OP_JMP) {
          checkjumptarget:
          {
            int jumpoffs = GETARG_sBx(code[pc-1]);
            int target = pc + jumpoffs;
            if (target > jumplimit || jumpoffs < 0)
              return 1; /* the previous jump is not part of this expression */
            else
              return 0;
          }
        }
        if (testAMode(o)) { /* test */
          if (a < firstreg)
            return 1;
        }
        else { /* comparison */
          if ((ISK(b) || b < firstreg) && (ISK(c) || c < firstreg))
            return 1;
        }
      }
      else if (testAMode(o)) {
        if (a == firstreg && beginseval(o, a, b, c, 1)) {
          if ((pc-1) >= 0 && GET_OPCODE(code[pc-1]) == OP_JMP)
            goto checkjumptarget;
          return 1;
        }
      }
      /* fallthrough */
      return 0;
  }
}


/*
** Find and mark the beginning of a call expression
*/
static void callstat1(CodeAnalyzer *ca, DFuncState *fs)
{
  int endpc; /* pc of call */
  int nargs; /* number of arguments */
  int nret; /* number of return values */
  int firstreg; /* first register used for the call */
  int lastreg; /* last register used for the call */
  int istailcall; /* whether this is a tailcall */
  const Instruction *code = ca->code;
  { /* handle the call instruction */
    Instruction call;
    int a, b, c;
    lua_assert(ca->pc >= 0);
    call = code[ca->pc];
    /* avoid long assertion string */
    if (!isOpCall(GET_OPCODE(call))) lua_assert(0);
    a = GETARG_A(call);
    b = GETARG_B(call);
    c = GETARG_C(call);
    nret = c - 1;
    nargs = b - 1;
    firstreg = a;
    lastreg = firstreg + nargs;
    istailcall = isOpTailCall(GET_OPCODE(call));
    endpc = ca->pc--;
  }
  for (; ca->pc >= 0; ca->pc--) {
    const int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int c = GETARG_C(i);
    switch (o) {
      case OP_SELF:
        lua_assert(a == firstreg);
        /* fallthrough */
      default: {
        if (isOpCall(o)) { /* a nested call expression */
          callstat1(ca, fs);
          /* when a function return value is used to evaluate another function
             expression, e.g. `f()()', mark the call op as a pre-call (a call
             operation itself will never be the beginning of a call expression,
             a fact that is taken advantage of here; when a call instruction is
             marked as such, it will clue the decompiler in that the most recent
             non-call `precall' instruction is actually the start of multiple
             calls that each call the return value of the previous call. */
          if (a == firstreg && c > 1) {
            init_ins_property(fs, pc, INS_PRECALL);
            return;
          }
        }
        else if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          init_ins_property(fs, pc, INS_PRECALL);
          if (istailcall)
            /*init_ins_property(fs, pc, INS_PRERETURN)*/;
          printf("returning from callstat1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return;
        }
        break;
      }
    }
  }
}

/*
** walk through a complete table construction (this prevents the analyzer from
** executing unnecessary logic inside a set-list sequence)
*/
static void setlist1(CodeAnalyzer *ca, DFuncState *fs)
{
  const Instruction *code = ca->code;
  int firstreg; /* register that holds the table */
  int narray; /* number of array-part items to set */
  int flushlevel;
  int endpc;
  {
    Instruction setlist;
    lua_assert(ca->pc >= 0);
    setlist = code[ca->pc];
    lua_assert(GET_OPCODE(setlist) == OP_SETLIST);
    firstreg = GETARG_A(setlist);
    narray = GETARG_B(setlist);
    flushlevel = GETARG_C(setlist);
    if (flushlevel == 0) {
      Instruction data = code[ca->pc+1];
      flushlevel = GETARG_sBx(data);
    }
    lua_assert(flushlevel > 0);
    endpc = ca->pc--;
  }
  for (; ca->pc >= 0; ca->pc--) {
    const int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      case OP_SETLIST:
        if (a == firstreg) {
          flushlevel--;
          lua_assert(flushlevel > 0);
        }
        else {
          setlist1(ca, fs);
        }
        break;
      CASE_OP_CALL:
        callstat1(ca, fs);
        lua_assert(firstreg != a);
        break;
      case OP_NEWTABLE:
        if (a == firstreg) {
          lua_assert(flushlevel == 1);
          return;
        }
      default: break;
    }
  }
  lua_assert(0);
}

/* returns 0 if the start of the return statement was successfully detected */
static int retstat1(CodeAnalyzer *ca, DFuncState *fs)
{
  const Instruction *code = ca->code;
  int firstreg; /* first register of returned expression list */
  int nret; /* number of returned values */
 /* int startpc;*/
  int endpc;
  { /* get start register and number of returned values */
    Instruction ret;
    lua_assert(ca->pc >= 0);
    ret = code[ca->pc];
    lua_assert(GET_OPCODE(ret) == OP_RETURN);
    firstreg = GETARG_A(ret);
    nret = GETARG_B(ret) - 1;
    endpc = ca->pc;
    if (nret == 0) { /* no values to return */
      init_ins_property(fs, endpc, INS_PRERETURN);
      return 0;
    }
    else if (nret == 1) {
      /* OP_RETURN is usually not the beginning of its own statement, but in
         this case, when there is only 1 return value, if argA is a local
         variable register, it does begin the statement. It is impossible right
         now to say whether it is a local variable or a temporary value */
      return 1;
    }
    ca->pc--;
  }
  for (; ca->pc >= 0; ca->pc--) {
    const int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      case OP_RETURN:
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (firstreg == a) {
          init_ins_property(fs, ca->pc, INS_PRERETURN);
          return 0;
        }
        break;
      default:
        if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          init_ins_property(fs, pc, INS_PRERETURN);
          printf("returning from retstat1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return 0;
        }
        break;
    }
  }
  lua_assert(0); /* cannot happen - void-return should already be handled */
  return 1;
}

static void fornumprep1(CodeAnalyzer *ca, DFuncState *fs, int endpc)
{
  const Instruction *code = ca->code;
  int firstreg; /* first register of loop control variables */
  int startpc; /* start pc of loop, specifically the jump op into the loop */
  lua_assert(ca->inforloop);
  ca->inforloop--;
  { /* get the start-register of the loop control variables */
    Instruction entry; /* OP_FORPREP */
    Instruction test; /* OP_FORLOOP */
    lua_assert(ca->pc >= 0);
    entry = code[ca->pc];
    lua_assert(GET_OPCODE(entry) == OP_FORPREP);
    lua_assert(GETARG_sBx(entry) >= 0); /* can be 0 in an empty loop */
    lua_assert(ca->pc + 1 + GETARG_sBx(entry) == endpc);
    test = code[endpc];
    lua_assert(GET_OPCODE(test) == OP_FORLOOP);
    firstreg = GETARG_A(entry);
    startpc = ca->pc--;
  }
  varstartshere1(ca, fs, startpc+1);
  varstartshere1(ca, fs, startpc);
  for (; ca->pc >= 0; ca->pc--) {
    const int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (firstreg == a) {
          init_ins_property(fs, ca->pc, INS_PREFORNUM);
          return;
        }
        break;
      default:
        if (beginstempexpr(code, i, pc, firstreg, startpc)) {
          init_ins_property(fs, pc, INS_PREFORNUM);
          printf("returning from fornumprep1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return;
        }
        break;
    }
  }
  /* it is possible for a for-loop to have no preparation code, for example:
      for x = nil, nil, nil do
          ...
      end
  */
  lua_assert(ca->pc < 0);
  init_ins_property(fs, 0, INS_PREFORNUM);
}

static void forlistprep1(CodeAnalyzer *ca, DFuncState *fs, int endpc)
{
  const Instruction *code = ca->code;
  int firstreg; /* first register of loop control variables */
  int nvars; /* number of loop variables */
  int startpc; /* start of loop, specifically the jump op into the loop */
  lua_assert(ca->inforloop);
  ca->inforloop--;
  { /* get the start-register of the loop control variables */
    Instruction entry; /* OP_JMP into the for-loop */
    Instruction test; /* OP_TFORLOOP at the end of the for-loop */
    lua_assert(ca->pc >= 0);
    entry = code[ca->pc];
    lua_assert(GET_OPCODE(entry) == OP_JMP);
    lua_assert(GETARG_sBx(entry) >= 0); /* can be 0 in an empty loop */
    lua_assert(ca->pc + 1 + GETARG_sBx(entry) == endpc);
    test = code[endpc];
    lua_assert(GET_OPCODE(test) == OP_TFORLOOP);
    nvars = GETARG_C(test);
    firstreg = GETARG_A(test);
    startpc = ca->pc--;
  }
  varstartshere1(ca, fs, startpc+1);
  varstartshere1(ca, fs, startpc);
  for (; ca->pc >= 0; ca->pc--) {
    const int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (firstreg == a) {
          init_ins_property(fs, ca->pc, INS_PREFORLIST);
          return;
        }
        break;
      default:
        if (beginstempexpr(code, i, pc, firstreg, startpc)) {
          init_ins_property(fs, pc, INS_PREFORLIST);
          printf("returning from forlistprep1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return;
        }
        break;
    }
  }
  /* it is possible for a for-loop to have no preparation code, for example:
      for x, y in nil do
          ...
      end
  */
  lua_assert(ca->pc < 0);
  init_ins_property(fs, 0, INS_PREFORLIST);
}


/*
** First pass while-loop analyzer
*/
static void whilestat1(CodeAnalyzer *ca, DFuncState *fs, const int startpc)
{
  int outerwhilestat, outerwhilestatend;
  int forlistreg = -1;
  const Instruction *code = ca->code;
  const int endpc = ca->pc--;
  ca->inwhileloop++;
  outerwhilestat = ca->whilestatstart;
  outerwhilestatend = ca->whilestatend;
  ca->whilestatstart = startpc;
  ca->whilestatend = endpc;
  for (; ca->pc >= 0; ca->pc--) {
    int pc = ca->pc;
    int nextpc = pc + 1;
    int prevpc = pc - 1;
    CODE_LOOP_DECL(code,pc);
    printf("pc %d: OP_%s\n", nextpc, luaP_opnames[o]);
    switch (o) {
      case OP_JMP: {
        int target = nextpc + sbx;
        OpCode prevop;
        /* jump into a for-loop */
        if (test_ins_property(fs, nextpc, INS_FORLIST)) {
          forlistprep1(ca, fs, target);
          break;
        }
        /* excluding the above case, this cannot be the first instruction */
        lua_assert(pc > 0);
        prevop = GET_OPCODE(code[prevpc]);
        ca->prevTMode = testTMode(prevop);
        /* there are 4 logical cetegories of jumps:
           (1) conditional backward jump (prevTMode && sbx < 0)
           (2) unconditional backward jump (!prevTMode && sbx < 0)
           (3) conditional forward jump (prevTMode && sbx >= 0)
           (4) unconditional forward jump (!prevTMode && sbx >= 0) */
        if (sbx < 0) { /* backward jump */
          set_ins_property(fs, target, INS_BJT);
          if (ca->prevTMode) { /* conditional backward jump */
            if (prevop == OP_TFORLOOP) { /* for-list-loop */
              printf("  encountered a for-list loop starting at %d\n",target+1);
              init_ins_property(fs, target, INS_FORLIST);
              init_ins_property(fs, pc, INS_LOOPEND);
              ca->inforloop++;
            }
            else { /* possibly a repeat-loop */
              /* When there are branches at the end of a while-loop, they will
                 exit by jumping to the start of the loop, saving an unnecessary
                 jump. Sometimes this will be preceded with a test, such as the
                 following case:
                    while a do
                        ...
                        if b then
                            ...
                            if c then
                                ...
                            end
                        end
                    end
                 When `c' is tested, the fail-case will jump back to the
                 beginning of the loop, which looks the same as the end of a
                 repeat-loop. This is differentiated by checking if the jump
                 target has already been marked as the start of a while-loop */
              if (!test_ins_property(fs, target, INS_WHILESTAT) &&
                  !test_ins_property(fs, target, INS_REPEATSTAT)) {
                printf("  encountered a repeat loop starting at %d\n",target+1);
                init_ins_property(fs, target, INS_REPEATSTAT);
                init_ins_property(fs, pc, INS_LOOPEND);
                init_ins_property(fs, pc, INS_REPEATSTATEND);
              }
              else if (test_ins_property(fs, target, INS_WHILESTAT)) {
                /* when while-loops fail-jump backward, it is because they are
                   at the end of an enclosing while-loop */
                if (target == ca->whilestatstart) {
                  init_ins_property(fs, pc, INS_BRANCHFAIL);
                  /* there is a branch at the end of the loop, which means the
                     end of the loop is also the end of a block */
                  set_ins_property(fs, endpc, INS_BLOCKEND);
                }
                else
                  init_ins_property(fs, pc, INS_LOOPFAIL);
              }
              else { /* jumps to repeat-loop */
                init_ins_property(fs, pc, INS_LOOPPASS);
              }
            }
          }
          else { /* unconditional backward jump */
            /* The end of if/elseif blocks within a while-loop will also jump to
               the beginning if there is no more code after the branch, such as
               the following example:
                  while a do
                      ...
                      if a == 1 then
                          ...
                      elseif a == 2 then
                          ...
                      else
                          ...
                      end
                  end
               Because the analyzer walks through the code backwards, it
               encounters the end-of-loop backward jump before it encounters any
               branch-exit backward jumps. Therefore, if the target has already
               been marked as the start of the while-loop, then this is an exit
               jump from a branch */
            if (test_ins_property(fs, target, INS_WHILESTAT)) {
              /* a break statement in an inner block/loop at the end of a
                 while-loop will jump backward to the start of the enclosing
                 while-loop */
              if (target == outerwhilestat) {
                /* an outer while loop with at least 1 inner for-loop or inner
                   while-loop */
                printf("num current while-loops: %d\n", ca->inwhileloop);
                lua_assert(ca->inwhileloop &&
                           (ca->inforloop + ca->inwhileloop) >= 2);
                init_ins_property(fs, pc, INS_BREAKSTAT);
              }
              else
                init_ins_property(fs, pc, INS_BLOCKEND); /* end of if/elseif */
            }
            else { /* mark beginning of while-loop */
              printf("  encountered a while loop starting at %d\n",target+1);
              init_ins_property(fs, target, INS_WHILESTAT);
              /* actual end of loop */
              init_ins_property(fs, pc, INS_LOOPEND);
              init_ins_property(fs, pc, INS_WHILESTATEND);
              if (test_ins_property(fs, nextpc, INS_BLOCKEND)) {
                Instruction nextins = code[nextpc];
                int opttarget;
                lua_assert(GET_OPCODE(nextins) == OP_JMP);
                opttarget = nextpc + 1 + GETARG_sBx(nextins);
                if (opttarget > nextpc) /* forward jump */
                  /* This is the result of an optimized jump from a failed loop
                     test to the end of the enclosing if-else branches. It
                     happens when there is no more code after the while-loop and
                     before the end of the enclosing branch. It is to be used
                     internally by the analyzer to determine if a failed
                     condition is part of a loop test or a branch test */
                  init_ins_property(fs, opttarget, INS_OPTLOOPFAILEXIT);
              }
              whilestat1(ca, fs, target);
              /* make sure everything was updated correctly */
              lua_assert(ca->inwhileloop >= 0);
              lua_assert(ca->pc == target);
              lua_assert(ca->whilestatstart == startpc);
              lua_assert(ca->whilestatend == endpc);
              continue;
            }
          }
        }
        else { /* forward jump */
          lua_assert(sbx >= 0);
          set_ins_property(fs, target, INS_FJT);
          if (ca->prevTMode) { /* conditional forward jump */
            if (forlistreg != -1)
              break; /* part of for-list variable evaluation */
            if (test_ins_property(fs, target-1, INS_REPEATSTATEND) &&
                pcinloop(ca, fs, pc, target)) {
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else if (test_ins_property(fs,target-1, INS_LOOPFAIL))
              init_ins_property(fs, pc, INS_LOOPPASS);
            else if (test_ins_property(fs,target-1, INS_LOOPPASS))
              init_ins_property(fs, pc, INS_LOOPFAIL);
            /* There is ambiguity when an if-statement and while-loop have the
               same exit target. This jump could be for the loop condition or it
               could be for the branch condition. If the current pc is after the
               start of the loop, this is part of the loop condition */
            else if (ca->inwhileloop &&
                     (test_ins_property(fs, target-1, INS_WHILESTATEND) ||
                     test_ins_property(fs, target, INS_OPTLOOPFAILEXIT))) {
              init_ins_property(fs, pc, INS_LOOPFAIL);
              break;
            }
            else {
              if (test_ins_property(fs, target-1, INS_BRANCHFAIL))
                init_ins_property(fs, pc, INS_BRANCHPASS);
              else {
                /* jump target out of if-statement */
                /* this will be set already if there are elseif-branches or an
                   else-branch */
                init_ins_property(fs, pc, INS_BRANCHFAIL);
                if (test_ins_property(fs, target-1, INS_BLOCKEND) &&
                    GET_OPCODE(code[target-1]) == OP_JMP) {
                  int jtarget = target + GETARG_sBx(code[target-1]);
                  if (jtarget - 1 > pc)
                    set_ins_property(fs, jtarget-1, INS_BLOCKEND);
                }
                else
                  set_ins_property(fs, target-1, INS_BLOCKEND);
              }
            }
          }
          else { /* unconditional forward jump (possibly a break statement) */
            if (test_ins_property(fs, target-1, INS_LOOPEND) &&
                pcinloop(ca, fs, pc, target)) { /* break statement */
              printf("  encoutered break-statement\n");
              init_ins_property(fs, pc, INS_BREAKSTAT); /* break out of loop */
            }
            else { /* exiting an if-else branch */
              printf("  encountered exit-jump from a branch\n");
              /* mark this jump as the end of the current branch */
              init_ins_property(fs, pc, INS_BLOCKEND);
              /*set_ins_property(fs, target, INS_BLOCKEND);*/
            }
          }
        }
        break;
      }
      case OP_TFORLOOP: /* handled in case OP_JMP */
        varstartshere1(ca, fs, pc);
        lua_assert(test_ins_property(fs, nextpc, INS_LOOPEND));
        break;
      case OP_FORLOOP: { /* identify a for-num loop */
        int target = nextpc + sbx;
        lua_assert(GET_OPCODE(code[target-1]) == OP_FORPREP);
        varstartshere1(ca, fs, pc);
        init_ins_property(fs, pc, INS_LOOPEND);
        init_ins_property(fs, target, INS_FORNUM);
        ca->inforloop++;
        break;
      }
      case OP_FORPREP: { /* mark the start of for-num loop control variables */
        int target = nextpc + sbx;
        lua_assert(test_ins_property(fs, target, INS_LOOPEND));
        fornumprep1(ca, fs, target);
        break;
      }
      CASE_OP_CALL: /* mark a call expression */
        callstat1(ca, fs);
        break;
      case OP_RETURN:
        if (ca->ret_pending != -1) { /* back-to-back returns */
          /* uncomment when everything is in place */
          /*lua_assert(GET_OPCODE(code[nextpc]) == OP_RETURN);*/
          init_ins_property(fs, nextpc, INS_PRERETURN);
        }
        ca->ret_pending = retstat1(ca, fs) ? a : -1;
        break;
      case OP_SETLIST:
        /* the analyzer will get confused inside a setlist operation and think
           new local variables are being created, so handle it separately in its
           own context */
        setlist1(ca, fs);
        lua_assert(ca->pc >= 0);
        /* still more work to do - update everything */
        pc = ca->pc;
        nextpc = pc + 1;
        prevpc = pc - 1;
        i = code[pc];
        o = GET_OPCODE(i);
        a = GETARG_A(i);
        b = GETARG_B(i);
        c = GETARG_C(i);
        lua_assert(o == OP_NEWTABLE);
        printf("pc %d: OP_%s\n", pc + 1, luaP_opnames[o]);
        /* fallthrough */
      default: {
        int n;
        /* make sure calls that change the pc actually break */
        lua_assert(pc == ca->pc);
        if (beginseval(o, a, b, c, 0))
          printf("\tbegins an expression\n");
        if ((n = varstartshere1(ca, fs, pc))) {
          /*printf("\t%d variable%s start%s here\n", n,
                 (n>1)?"s":"",(n==1)?"s":"");*/
        }
        (void)varstartshere1;
        UNUSED(b), UNUSED(c), UNUSED(bx);
      }
    }
    if (test_ins_property(fs, ca->pc, INS_WHILESTAT)) {
      lua_assert(ca->inwhileloop >= 1);
      break;
    }
  }
  ca->inwhileloop--;
  ca->whilestatstart = outerwhilestat;
  ca->whilestatend = outerwhilestatend;
}

/*
** The code analyzer works in the first pass and detects the beginnings and ends
** of loops (and determines the type of each loop), the ends of blocks, branch
** jumps, break jumps, and the beginnings of for-loop control variable
** evaluations. It walks through the code backwards, which makes differentiating
** branch tests from loop tests a simpler task.
*/
static void pass1(const Proto *f, DFuncState *fs, DecompState *D)
{
  CodeAnalyzer ca;
  ca.whilestatstart = ca.whilestatend = -1;
  ca.inforloop = 0;
  ca.inwhileloop = -1; /* incremented in whilestat1 */
  ca.pc = f->sizecode - 1; /* will be decremented again to skip final return */
  ca.prevTMode = 0;
  ca.ret_pending = -1;
  ca.lastexp = ca.pc;
  ca.datacount = 0;
  ca.code = f->code;
  ca.reg_properties = luaM_newvector(fs->H, f->maxstacksize, lu_byte);
  UNUSED(D);
  init_ins_property(fs, ca.pc, INS_BLOCKEND); /* mark end of function */
  varstartshere1(&ca, fs, ca.pc);
  whilestat1(&ca, fs, -1);
  luaM_freearray(fs->H, ca.reg_properties, f->maxstacksize, lu_byte);
  lua_assert(ca.whilestatstart == -1);
  lua_assert(ca.whilestatend == -1);
  lua_assert(ca.inforloop == 0);
  lua_assert(ca.inwhileloop == -1);
  lua_assert(ca.pc < 0);
  printf("fs->locvaridx == %d\n", fs->locvaridx);
  /*lua_assert(ca.ret_pending == -1);*/
}

static void DecompileFunction(const Proto *f, DFuncState *prev, DecompState *D);

static void DecompileCode(const Proto *f, DFuncState *fs, DecompState *D)
{
  const Instruction *code=f->code;
  int n=f->sizecode;
  int deltaAStreak=0; /* how many times in a row arg A increments */
  DExp *lastreg=NULL;
  lua_assert(n > 0); /* there is always at least OP_RETURN */
  dumploopinfo(f, fs, D);
  return;
  implicit_decl(fs, n, D);
  for (fs->pc=0; fs->pc<n; fs->pc++)
  {
    const int pc = fs->pc;
    Instruction i=code[pc];
    OpCode o=GET_OPCODE(i);
    int a=GETARG_A(i);
    int b=GETARG_B(i);
    int c=GETARG_C(i);
    int bx=GETARG_Bx(i);
    int sbx=GETARG_sBx(i);
    int deltaA=-1; /* change in register A index between instructions */
    int line=getline(f,pc);
    /* todo: line shouldn't be updated for initial implicit LOADNIL */
    if (line > 0 && match_lines_p(D)) {
      int lines_needed = line - fs->linepending;
      begin_line(fs, lines_needed, D);
    }
    else {
      maybe_begin_line(fs,D);
    }
    if (testAMode(o)) { /* a is a register */
      printf("%s uses reg A (%d)", luaP_opnames[o], a);
      if (lastreg != NULL) {
        printf(" last pending uses (%d)", dereg(lastreg));
        deltaA = a - dereg(lastreg);
      }
      else
        deltaA = a; /* handles initial omitted LOADNIL */
      printf("\n");
      if (lastreg != NULL && dereg(lastreg) < a) {
        /*if (deisid(last)) {
          adddecl(fs, last);
        }*/
        /*if (deisid(last))
          adddecl(fs, last);
        discharge(fs, D);
        NextLine(fs,D);*/
      }
      deltaAStreak = deltaA > 0 ? deltaAStreak + 1 : 0;
      printf("updated deltaAStreak to %d\n", deltaAStreak);
    }
    switch (o)
    {
#if 1
      case OP_LOADK: {
        DExp *exp = k2exp(fs, bx, D);
        dereg(exp) = a;
        append_exp(fs, exp);
        lastreg = exp;
        break;
      }
      case OP_RETURN: {
        if (pc == n-1) { /* final return */
          lua_assert(a == 0 && b == 1);
        }
#if 0
        else {
          DExp *exp;
          if (b == 1) { /* empty return */
            exp = newexp(fs, DRET);
            append_exp(fs, exp);
          }
          else {
            exp = fs->explist.first;
            lua_assert(fs->explist.first != NULL);
            desetret(exp);
            desettype(exp, DRET);
            deunsettype(exp, DLHS | DDECL);
          }
        }
#endif
        break;
      }
      case OP_LOADNIL: {
        /* todo: can use multiple registers */
        DExp *exp = newexp(fs, DK);
        dereg(exp) = a;
        exp = expliteral(exp, "nil");
        append_exp(fs, exp);
        lastreg = exp;
        break;
      }
#endif
#if 0
      /*case OP_NEWTABLE:*/
      case OP_LOADK:
      {
        DExp *exp = k2exp(fs, bx, D);
        dereg(exp) = a;
        /*lua_assert(a >= currlhs)*/
        append_exp(fs, exp);
        break;
      }
      case OP_GETGLOBAL: {
        DExp *exp = glob2exp(fs, bx, D);
        dereg(exp) = a;
        append_exp(fs, exp);
        if (exp == fs->explist.first) {
          printf("add decl expression for OP_GETGLOBAL\n");
          adddecl(fs, exp); /*  */
        }
        {

          /*const Instruction next = f->code[pc+1];
          OpCode nextop = GET_OPCODE(next);
          int nexta = GETARG_A(next);
          if (nextop == OP_RETURN)*/
        }
        break;
      }
      case OP_SETGLOBAL: {
        DExp *exp = glob2exp(fs, bx, D);
        setlhs(fs, exp);
        break;
      }
#endif
      default: {
        DumpStringf(D, "%s\t\t",luaP_opnames[o]);
        switch (getOpMode(o))
        {
          case iABC:
            DumpStringf(D, "%d",a);
            if (getBMode(o)!=OpArgN) DumpStringf(D, " %d",b);
            if (getCMode(o)!=OpArgN) DumpStringf(D, " %d",c);
            break;
          case iABx:
            DumpStringf(D, "%d %d",a,bx);
            break;
          case iAsBx:
            if (o==OP_JMP) DumpStringf(D, "%d",sbx); else DumpStringf(D, "%d %d",a,sbx);
            break;
        }
        DumpLiteral("\n",D);
      }
    }
  }
}

static void DecompileFunction(const Proto *f, DFuncState *prev, DecompState *D)
{
  DFuncState fs;
  open_func(&fs, f, prev, D);
  if (f->name && getstr(f->name))
    printf("-- Decompiling function named '%s'\n", getstr(f->name));
  else
    printf("-- Decompiling anonymous function (%d)\n", D->funcidx);
  pass1(f,&fs,D); /* detect loops and local variables */
  DecompileCode(f,&fs,D);
  /*discharge(&fs, D);*/ /* todo: should the chain always be empty by this point? */
  close_func(&fs, D);
}

/*
** dump Lua function as decompiled chunk
*/
int luaU_decompile (hksc_State *H, const Proto *f, lua_Writer w, void *data)
{
  DecompState D;
  D.H=H;
  D.writer=w;
  D.data=data;
  D.status=0;
  D.indentlevel=0;
  D.funcidx=0;
  D.needspace=0;
  DecompileFunction(f,NULL,&D);
  (void)f;
  (void)addargs;
  (void)insert_exp_before;
  (void)insert_exp_after;
  (void)setlhs;
  (void)adddecl;
  (void)removedecl;
  (void)glob2exp;
  (void)addlocalvar;
  (void)pass2_endlocalvar;
  (void)maybe_addlocvar;
  return D.status;
}
