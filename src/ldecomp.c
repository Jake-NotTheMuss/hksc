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
  INS_PRECONCAT, /* first pc that sets up a concat operation */
  INS_PRECALL, /* first pc that sets up a function call */
  INS_PRERETURN, /* first pc that evaluates a returned expression */
  INS_PRERETURN1, /* possible first pc that evaluates a single return value */
  INS_PREBRANCHTEST, /* first pc that evaluates a branch condition */
  INS_PREBRANCHTEST1, /* possible first pc that evaluates a branch condition */
  INS_BRANCHFAIL, /* false-jump out of an if-statement condition evaluation */
  INS_BRANCHPASS, /* true-jump out of an if-statement condition evaluation */
  INS_PRELOOPTEST, /* first pc that evaluates a loop condition */
  INS_PRELOOPTEST1, /* possible first pc that evaluates a loop condition */
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
  /* end of instruction flags */
  MAX_INS
};

static const char *const ins_flag_names[MAX_INS+1] = {
  "INS_FJT",
  "INS_BJT",
  "INS_PRECONCAT",
  "INS_PRECALL",
  "INS_PRERETURN",
  "INS_PRERETURN1",
  "INS_PREBRANCHTEST",
  "INS_PREBRANCHTEST1",
  "INS_BRANCHFAIL",
  "INS_BRANCHPASS",
  "INS_PRELOOPTEST",
  "INS_PRELOOPTEST1",
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
  (fs)->ins_properties[(pc)] &= ~(1 << (val)); \
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
    if (test_ins_property(fs, pc, INS_PRECONCAT))
      DumpLiteral("PRE CONCAT\n", D);
    if (test_ins_property(fs, pc, INS_PRERETURN))
      DumpLiteral("PRE RETURN\n", D);
    else if (test_ins_property(fs, pc, INS_PRERETURN1))
      DumpLiteral("PRE RETURN 1\n", D);
    if (test_ins_property(fs, pc, INS_PREBRANCHTEST))
      DumpLiteral("PRE BRANCH TEST\n", D);
    else if (test_ins_property(fs, pc, INS_PREBRANCHTEST1))
      DumpLiteral("PRE BRANCH TEST 1\n", D);
    else if (test_ins_property(fs, pc, INS_PRELOOPTEST))
      DumpLiteral("PRE LOOP TEST\n", D);
    else if (test_ins_property(fs, pc, INS_PRELOOPTEST1))
      DumpLiteral("PRE LOOP TEST 1\n", D);
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
** into account dependencies related to registers that both read and clobbered.
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

/*
** basic block types used in the first pass
*/
#define BBL1_FUNCTION 0
#define BBL1_WHILE 1
#define BBL1_REPEAT 2
#define BBL1_FOR 3
/* the other basic block types are not needed for the first pass */
#define BBL1_MAX 4

/*
** `Code Analyzer' - the main structure used in the first pass. It populates the
** `ins_properties' array of its corresponding `DFuncState'.
*/
typedef struct CodeAnalyzer {
  /* a record of all encountered loops is saved implicitly on the callstack;
     these values hold the bounds of the current loop */
  struct {
    int start, end;
  } bbl_bounds[BBL1_MAX+1];
  int in_bbl[BBL1_MAX]; /* number currently active for each loop type */
  int pc; /* current pc */
  /* most of these values could be local to `whilestat1', but are kept in this
     structure to avoid allocating new copies on every recursive call */
  lu_byte prevTMode; /* previous pc is a test instruction */
  lu_byte buildingretval; /* whether evaluating a single return value */
  int retpending; /* a returned register, which may be temporary */
  int retpc; /* pc of OP_RETURN in pending return statement */
  struct {
    int r1, r2; /* compared registers */
    int pc; /* pc of last test instruction */
    int flag; /* flag for the type of condition (loop or branch) */
  } condpending; /* a tested register or pair of registers */
  lu_byte *reg_properties;
  const Instruction *code; /* f->code */
} CodeAnalyzer;

#if 0
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
#endif
#if 0
static void locvar1(CodeAnalyzer *ca, DFuncState *fs, int reg);

static int updatelastexp(CodeAnalyzer *ca, DFuncState *fs, int pc, int reg) {
  int n;
  int lastexp = ca->lastexp;
  ca->lastexp = pc;
  printf("\tbegins an expression\n");
  n = varstartshere1(ca, fs, pc);
  (void)varstartshere2;
  if (n) {
    int ends = fs->locvars[fs->locvaridx+1].endpc == pc;
    printf("\tlocvar1(ca, fs, %d);\n", ends?reg:reg-n);
    locvar1(ca, fs, ends?reg:reg-n);
  }
  return n;
}
#endif
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
** Returns true if the instruction at (PC-1) is OP_JMP.
*/
static int previsjump(const Instruction *code, int pc) {
  if (pc > 0) {
    Instruction i = code[pc-1];
    return GET_OPCODE(i) == OP_JMP;
  }
  return 0;
}

/*
** Check if the given instruction I begins a temporary expression in register
** FISRTREG. PC is the pc of I. JUMPLIMIT is a pc used to check if a preceding
** jump instruction jumps to somewhere within the temporary expression
** evaluation, meaning it is part of the temporary expression. CODE is the
** instruction array which contains I, used for single look-behinds.
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
      /* This jump must be part of the temporary expression. Otherwise, the
         caller has made a mistake by calling after they have already found the
         beginning of the expression. */
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
          return 0; /* one result of a boolean expression */
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
        if (testAMode(o)) { /* OP_TEST */
          if (a < firstreg) {
            /* This instruction tests a local variable and is not preceded by a
               jump. Check if the preceding instruction uses a free register */
            checkprevreg:
            if ((pc-1) >= 0) {
              Instruction prev = code[pc-1];
              OpCode prevop = GET_OPCODE(prev);
              int preva = GETARG_A(prev);
              return !(testAMode(prevop) && preva >= firstreg);
            }
            else return 1; /* this is the first instruction */
          }
        }
        else { /* comparison op */
          if ((ISK(b) || b < firstreg) && (ISK(c) || c < firstreg))
            /* Same as above. */
            goto checkprevreg;
        }
      }
      else if (testAMode(o)) { /* A is a register */
        if (a == firstreg && beginseval(o, a, b, c, 1)) {
          if ((pc-1) >= 0 && GET_OPCODE(code[pc-1]) == OP_JMP)
            goto checkjumptarget;
          /* Clobbers the first free register without using what was previously
             stored inside it, and there is no jump to precede this instruction.
             This is the start of a temporary expression. */
          return 1;
        }
      }
      return 0;
  }
}


static void callstat1(CodeAnalyzer *ca, DFuncState *fs);

/*
** concat -> ... OP_CONCAT
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: No
** New Blocks: No
** =============================================================================
** Finds and marks the beginning of a concatenated expression.
*/
static void concat1(CodeAnalyzer *ca, DFuncState *fs)
{
  int endpc; /* pc of OP_CONCAT */
  int firstreg; /* first register used in concat operation */
  const Instruction *code = ca->code;
  {
    Instruction concat;
    lua_assert(ca->pc >= 0);
    concat = code[ca->pc];
    lua_assert(GET_OPCODE(concat) == OP_CONCAT);
    firstreg = GETARG_B(concat);
    endpc = ca->pc--;
  }
  for (; ca->pc >= 0; ca->pc--) {
    const int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (firstreg == a) {
          init_ins_property(fs, ca->pc, INS_PRECONCAT);
          return;
        }
        break;
      case OP_CONCAT:
      case OP_RETURN:
        lua_assert(0);
      default:
        if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          init_ins_property(fs, pc, INS_PRECONCAT);
          printf("returning from concat1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return;
        }
        break;
    }
  }
}


/*
** callstat -> ... [callstat] ... OP_CALL [callstat]
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: No
** New Blocks: No
** =============================================================================
** Finds and marks the beginning of a call expression.
*/
static void callstat1(CodeAnalyzer *ca, DFuncState *fs)
{
  int endpc; /* pc of call */
  int firstreg; /* first register used for the call */
  const Instruction *code = ca->code;
  { /* handle the call instruction */
    Instruction call;
    lua_assert(ca->pc >= 0);
    call = code[ca->pc];
    /* avoid long assertion string */
    if (!isOpCall(GET_OPCODE(call))) lua_assert(0);
    firstreg = GETARG_A(call);
    endpc = ca->pc--;
  }
  for (; ca->pc >= 0; ca->pc--) {
    int pc;
    Instruction i;
    OpCode o;
    int a, c;
    recheck:
    pc = ca->pc;
    i = code[pc];
    o = GET_OPCODE(i);
    a = GETARG_A(i);
    c = GETARG_C(i);
    switch (o) {
      case OP_CONCAT:
        concat1(ca, fs);
        goto recheck;
      case OP_SELF:
        lua_assert(a == firstreg);
        /* fallthrough */
      default: {
        if (isOpCall(o)) { /* a nested call expression */
          callstat1(ca, fs);
          /* When a function return value is used to evaluate another function
             expression, e.g. `f()()', mark the first call op as a pre-call (a
             call operation itself will never be the beginning of a call
             expression, a fact that is taken advantage of here; when a call
             instruction is marked as such, it will clue the decompiler in that
             the most recent non-call `precall' instruction is actually the
             start of multiple calls that each call the return value of the
             previous call). */
          if (a == firstreg && c > 1) {
            init_ins_property(fs, pc, INS_PRECALL);
            return;
          }
        }
        else if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          init_ins_property(fs, pc, INS_PRECALL);
          printf("returning from callstat1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return;
        }
        break;
      }
    }
  }
}

#if 0
/*
** setlist -> OP_NEWTABLE ... [setlist] ... OP_SETLIST
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: No
** New Blocks: No
** =============================================================================
** Walks through a complete table construction (this prevents the analyzer from
** executing unnecessary logic inside a set-list sequence).
*/
static void setlist1(CodeAnalyzer *ca, DFuncState *fs)
{
  const Instruction *code = ca->code;
  int firstreg; /* register that holds the table */
  int flushlevel; /* value of argC from most recent OP_SETLIST */
  int endpc; /* pc of final OP_SETLIST */
  {
    Instruction setlist;
    lua_assert(ca->pc >= 0);
    setlist = code[ca->pc];
    lua_assert(GET_OPCODE(setlist) == OP_SETLIST);
    firstreg = GETARG_A(setlist);
    flushlevel = GETARG_C(setlist);
    if (flushlevel == 0) /* next instruction holds real C */
      flushlevel = GETARG_Bx(code[ca->pc+1]);
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
        else { /* nested table constructor */
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
#endif

#if 0
/*
** locvar -> reserve free register R ... write to R ... begin new statement
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: No
** New Blocks: No
** =============================================================================
** Marks the beginning of the evluation of a new variable assignment.
*/
static void locvar1(CodeAnalyzer *ca, DFuncState *fs, int reg)
{
  const Instruction *code = ca->code;
  int startpc = ca->pc--;
  for (; ca->pc >= 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (reg == a) {
          init_ins_property(fs, ca->pc, INS_PREVARSTART);
          return;
        }
        break;
      case OP_SETLIST:
        setlist1(ca, fs);
        pc = ca->pc;
        i = code[pc];
        o = GET_OPCODE(i);
        a = GETARG_A(i);
        /* fallthrough */
      default:
        if (beginstempexpr(code, i, pc, reg, startpc)) {
          init_ins_property(fs, pc, INS_PREVARSTART);
          printf("returning from locvar1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return;
        }
        break;
    }
  }
}
#endif

/*
** retstat -> ... OP_RETURN
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: No
** New Blocks: No
** =============================================================================
** If the beginning of the return statement cannot be determined from the
** information currently at hand, the (single) register used in the return
** statement is returned, otherwise -1.
*/
static int retstat1(CodeAnalyzer *ca, DFuncState *fs)
{
  const Instruction *code = ca->code;
  int firstreg; /* first register of returned expression list */
  int nret; /* number of returned values */
  int endpc; /* pc of OP_RETURN */
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
      return -1;
    }
    else if (nret == 1) {
      /* OP_RETURN is usually not the beginning of its own statement, but in
         this case, when there is only 1 return value, if argA is a local
         variable register, it does begin the statement. It is impossible right
         now to say whether it is a local variable or a temporary value */
      return firstreg;
    }
    ca->pc--;
  }
  for (; ca->pc >= 0; ca->pc--) {
    int pc;
    Instruction i;
    OpCode o;
    int a, c;
    recheck:
    pc = ca->pc;
    i = code[pc];
    o = GET_OPCODE(i);
    a = GETARG_A(i);
    c = GETARG_C(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (firstreg == a) {
          init_ins_property(fs, ca->pc, INS_PRERETURN);
          return -1;
        }
        break;
      case OP_CONCAT:
        concat1(ca, fs);
        goto recheck;
      default:
        lua_assert(o != OP_RETURN); /* cannot have nested return statements */
        if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          init_ins_property(fs, pc, INS_PRERETURN);
          printf("returning from retstat1 at pc %d, OP_%s\n",
                 pc+1, luaP_opnames[o]);
          return -1;
        }
        break;
    }
  }
  lua_assert(0); /* cannot happen - void-return should already be handled */
  return -1;
}

/*
** fornumprep -> ... OP_FORPREP
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: Only loop-control variables
** New Blocks: No
** =============================================================================
** Marks the start of for-num-loop preparation code.
*/
static void fornumprep1(CodeAnalyzer *ca, DFuncState *fs, int endpc)
{
  const Instruction *code = ca->code;
  int firstreg; /* first register of loop control variables */
  int startpc; /* start pc of loop, specifically the jump op into the loop */
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
      case OP_CONCAT:
        concat1(ca, fs);
        /* fallthrough */
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

/*
** forlistprep -> ... OP_JMP (to OP_TFORLOOP)
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: No
** New Variables: Only loop-control variables
** New Blocks: No
** =============================================================================
** Marks the start of for-list-loop preparation code.
*/
static void forlistprep1(CodeAnalyzer *ca, DFuncState *fs, int endpc)
{
  const Instruction *code = ca->code;
  int firstreg; /* first register of loop control variables */
  int nvars; /* number of loop variables */
  int startpc; /* start of loop, specifically the jump op into the loop */
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
  for (; ca->pc >= 0; ca->pc--) {
    int pc;
    Instruction i;
    OpCode o;
    int a, c;
    recheck:
    pc = ca->pc;
    i = code[pc];
    o = GET_OPCODE(i);
    a = GETARG_A(i);
    c = GETARG_C(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        if (firstreg == a) {
          init_ins_property(fs, ca->pc, INS_PREFORLIST);
          return;
        }
        break;
      case OP_CONCAT:
        concat1(ca, fs);
        goto recheck;
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
** Check if there is a valid pending return register, and discharge it. This is
** used for special instructions that can never be a part of a return statement.
** The instruction after the current is expected to be an OP_RETURN.
*/
#define pendingreturn1ex(extraassert) do { \
  if (ca->retpending != -1) { \
    /* store in a variable to avoid long assertion string */ \
    int extraassertval_ = (extraassert); \
    lua_assert(GET_OPCODE(code[nextpc]) == OP_RETURN); \
    lua_assert(ca->retpc == nextpc); \
    lua_assert(extraassertval_); UNUSED(extraassertval_); \
    init_ins_property(fs, nextpc, INS_PRERETURN); \
    ca->retpending = -1; /* discharge */ \
  } \
} while (0)

#define pendingreturn1() pendingreturn1ex(1)

#define pendingcond1() do { \
  if (ca->condpending.r1 != -1 || ca->condpending.r2 != -1) { \
    lua_assert(testTMode(GET_OPCODE(code[nextpc]))); \
    lua_assert(ca->condpending.pc == nextpc); \
    init_ins_property(fs, nextpc, ca->condpending.flag); \
    ca->condpending.r1 = ca->condpending.r2 = -1; /* discharge */ \
  } \
} while (0)

/* mark the end of a block or loop with INS_BLOCKEND or INS_LOOPEND */
#define markend1(pc,flag,init) do { \
  if (init) \
    init_ins_property(fs, (pc), flag); \
  else \
    set_ins_property(fs, (pc), flag); \
  { \
    int pendingflag_; \
    int pc_ = (pc)+1; \
    if (test_ins_property(fs, (pc), INS_PREBRANCHTEST) || \
        test_ins_property(fs, (pc), INS_PREBRANCHTEST1)) \
      pendingflag_ = INS_PREBRANCHTEST; \
    else if (test_ins_property(fs, (pc), INS_PRELOOPTEST) || \
             test_ins_property(fs, (pc), INS_PRELOOPTEST1)) \
      pendingflag_ = INS_PRELOOPTEST; \
    else \
      break; \
    while (test_ins_property(fs, pc_, INS_BLOCKEND) || \
           test_ins_property(fs, pc_, INS_LOOPEND)) \
      pc_++; \
    unset_ins_property(fs, (pc), pendingflag_); \
    unset_ins_property(fs, (pc), pendingflag_+1); \
    pendingflag_+=!testTMode(o); \
    lua_assert(!test_ins_property(fs, pc_, INS_WHILESTAT)); \
    init_ins_property(fs, pc_, pendingflag_); \
  } \
} while (0)


/*
** whilestat -> { test OP_JMP } (to EXIT) ... OP_JMP (to test) EXIT
** ============================ NESTED CONSTRUCTS ==============================
** Call Statements: Yes
** Return Statements: Yes
** New Variables: Yes
** New Blocks: Yes
** =============================================================================
** Classifies/contextualizes jumps and invokes handlers for other constructs.
*/
static void bbl1(CodeAnalyzer *ca, DFuncState *fs, const int startpc, int type)
{
  /* `seenloopcond' is used to tell if the analyzer incorrectly detected a
     while-loop instead of a repeat-until-false loop such as the following:
      repeat
          ...
      until false
     Both cases generate a loop with no tail-condition, which creates ambiguity
     for the analyzer. The analyzer assumes it is a while-loop, but if it gets
     to the start of the loop and `seenloopcond' is still false, then it
     re-marks the current loop as a repeat-loop with a constant false
     tail-condition. */
  int seenloopcond = 0; /* whether a loop condition has been encountered */
  int outerstatstart, outerstatend; /* enclosing loop of any type */
  int outertypestatstart, outertypestatend; /* enclosing loop of type `type' */
  const Instruction *code = ca->code;
  const int endpc = ca->pc--;
  ca->in_bbl[type]++;
  /* push old values */
  outerstatstart = ca->bbl_bounds[BBL1_MAX].start;
  outerstatend = ca->bbl_bounds[BBL1_MAX].end;
  outertypestatstart = ca->bbl_bounds[type].start;
  outertypestatend = ca->bbl_bounds[type].end;
  /* update to new values */
  ca->bbl_bounds[BBL1_MAX].start = startpc;
  ca->bbl_bounds[BBL1_MAX].end = endpc;
  ca->bbl_bounds[type].start = startpc;
  ca->bbl_bounds[type].end = endpc;
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
          /* check for a return statement at the start of the for-loop */
          pendingreturn1();
          forlistprep1(ca, fs, target);
          break;
        }
        if (pc == 0) {
          init_ins_property(fs, pc, INS_BREAKSTAT);
          break;
        }
        /* check for pending return register */
        if (ca->retpending != -1 && sbx >= 0 && target <= ca->retpc)
          break; /* this jump is part of the return expression */
        else
          pendingreturn1(); /* discarge pending return if needed */
        /* check for pending condition registers */
        if (ca->condpending.r1 != -1 || ca->condpending.r2 != -1) {
          init_ins_property(fs, nextpc,
                            ca->condpending.flag + ca->buildingretval);
          ca->condpending.r1 = ca->condpending.r2 = -1;
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
              /*init_ins_property(fs, pc, INS_LOOPEND);*/
              markend1(pc, INS_LOOPEND, 1);
              bbl1(ca, fs, target, BBL1_FOR);
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
                /*init_ins_property(fs, pc, INS_LOOPEND);*/
                markend1(pc, INS_LOOPEND, 1);
                init_ins_property(fs, pc, INS_REPEATSTATEND);
                init_ins_property(fs, pc, INS_LOOPPASS);
                bbl1(ca, fs, target, BBL1_REPEAT);
              }
              else if (test_ins_property(fs, target, INS_WHILESTAT)) {
                /* when while-loops fail-jump backward, it is because they are
                   at the end of an enclosing while-loop */
                if (target == startpc)
                /*if (target == ca->bbl_bounds[BBL1_WHILE].start)*/ {
                  init_ins_property(fs, pc, INS_BRANCHFAIL);
                  /* there is a branch at the end of the loop, which means the
                     end of the loop is also the end of a block */
                  /*set_ins_property(fs, endpc, INS_BLOCKEND);*/
                  markend1(endpc, INS_BLOCKEND, 0);
                }
                else {
                  seenloopcond = (type == BBL1_WHILE);
                  init_ins_property(fs, pc, INS_LOOPFAIL);
                }
              }
              else { /* jumps to repeat-loop */
                lua_assert(type == BBL1_REPEAT);
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
              if (target == outerstatstart) {
                /* an outer while loop with at least 1 inner for-loop or inner
                   while-loop */
                printf("num current while-loops: %d\n", ca->in_bbl[BBL1_WHILE]);
                lua_assert(ca->in_bbl[BBL1_WHILE] &&
                          (ca->in_bbl[BBL1_FOR] + ca->in_bbl[BBL1_WHILE]) >= 2);
                init_ins_property(fs, pc, INS_BREAKSTAT);
              }
              else
                /*init_ins_property(fs, pc, INS_BLOCKEND);*/ /* end of if/elseif */
                markend1(pc, INS_BLOCKEND, 1);
            }
            else { /* mark beginning of while-loop */
              printf("  encountered a while loop starting at %d\n",target+1);
              init_ins_property(fs, target, INS_WHILESTAT);
              /* actual end of loop */
              /*init_ins_property(fs, pc, INS_LOOPEND);*/
              markend1(pc, INS_LOOPEND, 1);
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
              bbl1(ca, fs, target, BBL1_WHILE);
            }
          }
        }
        else { /* forward jump */
          lua_assert(sbx >= 0);
          set_ins_property(fs, target, INS_FJT);
          if (ca->prevTMode) { /* conditional forward jump */
            /* check for a repeat-until condition exit jump */
            if (type == BBL1_REPEAT && target-1 == endpc) {
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            /* if the jump skips over a false-jump, this is a true-jump */
            else if (test_ins_property(fs,target-1, INS_LOOPFAIL)) {
              lua_assert(seenloopcond);
              init_ins_property(fs, pc, INS_LOOPPASS);
            }
            /* if the jump skips over a true-jump, this is a false-jump  */
            else if (test_ins_property(fs,target-1, INS_LOOPPASS)) {
              lua_assert(seenloopcond);
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            /* check for a while-loop condition exit jump */
            else if (type == BBL1_WHILE &&
                     /* a break statement follows the while-loop, which means
                        the jump is optimized to target the end of the enclosing
                        loop */
                     ((target-1 == outerstatend &&
                       test_ins_property(fs, endpc+1, INS_BREAKSTAT)) ||
                     /* jumps to the end of the current loop */
                      (target-1 == endpc) ||
                     /* an optimized jump out of the loop condition */
                      (test_ins_property(fs, target, INS_OPTLOOPFAILEXIT)))) {
              lua_assert(outerstatend != -1);
              seenloopcond = 1;
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else { /* jumping in a branch-condition */
              /* if the jump skips over a false-jump, this is a true-jump  */
              /*if (ca->condpending.r1 != -1 || ca->condpending.r2 != -1) {
                init_ins_property(fs, nextpc, INS_PREBRANCHTEST);
                ca->condpending.r1 = ca->condpending.r2 = -1;
              }*/
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
                    /*set_ins_property(fs, jtarget-1, INS_BLOCKEND);*/
                    markend1(jtarget-1, INS_BLOCKEND, 0);
                }
                else
                  /*set_ins_property(fs, target-1, INS_BLOCKEND);*/
                  markend1(target-1, INS_BLOCKEND, 0);
              }
            }
          }
          else { /* unconditional forward jump (possibly a break statement) */
            if (test_ins_property(fs, target-1, INS_LOOPEND) &&
                target-1 == endpc) { /* break statement */
              printf("  encoutered break-statement\n");
              init_ins_property(fs, pc, INS_BREAKSTAT); /* break out of loop */
            }
            else { /* exiting an if-else branch */
              printf("  encountered exit-jump from a branch\n");
              /* mark this jump as the end of the current branch */
              /*init_ins_property(fs, pc, INS_BLOCKEND);*/
              markend1(pc, INS_BLOCKEND, 1);
              /*set_ins_property(fs, target, INS_BLOCKEND);*/
            }
          }
        }
        /*if (test_ins_property(fs, pc, INS_BLOCKEND)) {
          init_ins_property(fs, nextpc, INS_PREBRANCHTEST);
          ca->condpending.r1 = ca->condpending.r2 = -1;
        }*/
        break;
      }
      case OP_TFORLOOP: /* handled in case OP_JMP */
        lua_assert(test_ins_property(fs, nextpc, INS_LOOPEND));
        lua_assert(ca->retpending == -1);
        lua_assert(ca->condpending.r1 == -1 && ca->condpending.r2 == -1);
        break;
      case OP_FORLOOP: { /* identify a for-num loop */
        int target = nextpc + sbx;
        lua_assert(GET_OPCODE(code[target-1]) == OP_FORPREP);
        /*init_ins_property(fs, pc, INS_LOOPEND);*/
        markend1(pc, INS_LOOPEND, 1);
        init_ins_property(fs, target, INS_FORNUM);
        /* check for a return statement after the end of the for-loop */
        pendingreturn1();
        pendingcond1();
        bbl1(ca, fs, target, BBL1_FOR);
        break;
      }
      case OP_FORPREP: { /* mark the start of preparation code */
        int target = nextpc + sbx;
        lua_assert(test_ins_property(fs, target, INS_LOOPEND));
        lua_assert(type == BBL1_FOR);
        lua_assert(pc == startpc);
        /* check for a return statement at the start of the for-loop */
        pendingreturn1();
        pendingcond1();
        fornumprep1(ca, fs, target);
        goto exitblock;
      }
      CASE_OP_CALL: /* mark a call expression */
        /* if retpending is a valid register, this call cannot be part of the
           return expression, as tailcalls are handled in `retstat1'. Mark the
           start of the following return statement and discharge retpending */
        pendingreturn1ex(!isOpTailCall(o));
        callstat1(ca, fs);
        goto postcallstat; /* check on pending condition registers */
      case OP_CONCAT:
        concat1(ca, fs);
        goto postcallstat;
      case OP_RETURN: /* mark a return statement */
        pendingreturn1();
        pendingcond1();
        ca->retpending = retstat1(ca, fs);
        if (ca->retpending != -1) {
          lua_assert(ca->condpending.r1 == -1 && ca->condpending.r2 == -1);
          ca->retpc = pc;
          ca->buildingretval = 0;
        }
        break;
      default: {
        int pendingflag;
        /* record a pending branch condition expression if needed */
        if (test_ins_property(fs, nextpc, INS_BRANCHFAIL) ||
             test_ins_property(fs, nextpc, INS_BRANCHPASS)) {
          pendingflag = INS_PREBRANCHTEST;
          pendingloopcond:
          lua_assert(ca->retpending == -1);
          lua_assert(testTMode(o));
          lua_assert(ca->condpending.r1 == -1);
          if (testAMode(o)) { /* OP_TEST, OP_TESTSET */
            ca->condpending.r1 = a;
            ca->condpending.r2 = -1;
          }
          else { /* comparison */
            ca->condpending.r1 = ISK(b) ? -1 : b;
            ca->condpending.r2 = ISK(c) ? -1 : c;
          }
          lua_assert(ca->condpending.r1 != -1 || ca->condpending.r2 != -1);
          ca->condpending.pc = pc;
          ca->condpending.flag = pendingflag;
          printf("Recording new pending condition at pc (%d)\n"
                 "\tflag == %s\n"
                 "\tr1 == (%d)\n"
                 "\tr2 == (%d)\n", pc+1,ins_flag_names[pendingflag],
                 ca->condpending.r1, ca->condpending.r2);
          ca->buildingretval = 0;
        }
        /* record a pending loop condition expression if needed */
        else if (test_ins_property(fs, nextpc, INS_LOOPFAIL) ||
                 test_ins_property(fs, nextpc, INS_LOOPPASS)) {
          pendingflag = INS_PRELOOPTEST;
          goto pendingloopcond;
        }
        else if (ca->retpending != -1) {
          if (testAMode(o)) {
            if (!ca->buildingretval && a != ca->retpending) {
              init_ins_property(fs, ca->retpc, INS_PRERETURN);
              ca->retpending = -1;
            }
            else {
              ca->buildingretval = 1; /* currently evluating retpending */
              if (beginstempexpr(code, i, pc, ca->retpending,
                                    ca->retpc)) {
                /* this may be the start of the return statement */
                init_ins_property(fs, pc, INS_PRERETURN1);
                ca->retpending = -1;
              }
            }
          }
        }
        else {
          int callstat = 0;
          goto checkpendingcond;
          /* Jump to here after calling `callstat1'. This will do 2 checks, one
             for the pc of the call instruction, and one for the current pc */
          postcallstat:
          callstat = 1;
          checkpendingcond:
          if (ca->condpending.r1 != -1 || ca->condpending.r2 != -1) {
            if (testAMode(o)) {
              int *condreg = ca->condpending.r2 != -1 ? &ca->condpending.r2 :
                                                        &ca->condpending.r1;
              lua_assert(*condreg != -1);
              if (!ca->buildingretval && a != *condreg) {
                init_ins_property(fs, ca->condpending.pc, ca->condpending.flag);
                goto checkcondreg;
              }
              else {
                ca->buildingretval = 1;
                if (beginstempexpr(code, i, pc, *condreg, ca->condpending.pc)) {
                  int wasjump = previsjump(code, pc);
                  int flagvariant = !(wasjump ||
                                      test_ins_property(fs, pc, INS_WHILESTAT));
                  /* if the previous instruction is a jump, this instruction
                     begins the condition expression, hence the addition of
                     `!previsjump()', turning the `certain' flag into the
                     `uncertain' flag if there is no preceding jump */
                  init_ins_property(fs, pc,
                    ca->condpending.flag + flagvariant);
                  checkcondreg:
                  printf(
                         "\tca->condpending.r1 == (%d)\n"
                         "\tca->condpending.r2 == (%d)\n",
                         ca->condpending.r1, ca->condpending.r2);
                  *condreg = -1;
                  printf("Marked pc (%d) as %s\n"
                         "\tca->condpending.r1 == (%d)\n"
                         "\tca->condpending.r2 == (%d)\n", pc+1,
                         ins_flag_names[ca->condpending.flag + flagvariant],
                         ca->condpending.r1, ca->condpending.r2);
                  ca->buildingretval = 0;
                  /* see if there is still 1 more register pending */
                  if (ca->condpending.r1 != -1) {
                    /* a preceding jump is caught here, where the pending
                       condition can be completed before moving on */
                    if (wasjump)
                      ca->condpending.r1 = -1;
                    else
                      /* 1 more pending register from the last test; update the
                         pending end-pc for the remaining expression */
                      ca->condpending.pc = prevpc;
                  }
                }
              }
            }
          }
          if (callstat) {
            callstat = 0;
            /* update variables to current pc for the second check */
            pc = ca->pc;
            i = code[pc];
            o = GET_OPCODE(i);
            a = GETARG_A(i);
            goto checkpendingcond;
          }
        }
        UNUSED(a), UNUSED(b), UNUSED(c), UNUSED(bx);
        break;
      }
    }
    pc = ca->pc; /* update current pc */
    if (pc == startpc) {
      if (type == BBL1_WHILE) {
        lua_assert(test_ins_property(fs, pc, INS_WHILESTAT));
        /* repeat-until-false-loops will be marked as while-loops */
        printf("ca->in_bbl[BBL1_WHILE] = %d\n"
               "ca->in_bbl[BBL1_REPEAT] = %d\n",
               ca->in_bbl[BBL1_WHILE],
               ca->in_bbl[BBL1_REPEAT]);
        lua_assert(ca->in_bbl[BBL1_WHILE] || ca->in_bbl[BBL1_REPEAT]);
        lua_assert(ca->retpending == -1);
        /* Discharge pending while-loop explicitly condition, this is needed in
           cases such as the following example:
            local a;
            [begin block]
                ...
                a = a * 23;
            end
            while a < 0 and a == i * 20 do
                ...
           The analyzer will assume that an expression is being built across the
           start of the loop (as if the while-loop condition contained
           `a * 23 < 0'). This is caught here because the analyzer does not both
           record a new pending condition and discharge one in the same switch
           statement.
          */
        if (ca->condpending.r1 != -1 || ca->condpending.r2 != -1) {
          int flag = seenloopcond ? INS_PRELOOPTEST : INS_PREBRANCHTEST;
          ca->condpending.r1 = ca->condpending.r2 = -1;
          init_ins_property(fs, pc, flag);
        }
        if (!seenloopcond) { /* this may not be a while-loop */
          if (test_ins_property(fs, pc, INS_BREAKSTAT)) {
            /* A while-loop with `false' as the condition will have an
               unconditional jump at the beginning of the loop, which will be
               marked as a break-statement. It is harmless, but should not be
               listed when debugging pass1 results. */
#ifdef LUA_DEBUG
            unset_ins_property(fs, pc, INS_BREAKSTAT);
#endif /* LUA_DEBUG */
          }
#if 0
          else { /* change to repeat-loop with `false' condition */
            lua_assert(test_ins_property(fs, endpc, INS_WHILESTATEND));
            lua_assert(test_ins_property(fs, endpc, INS_LOOPEND));
            /* remove while-loop markers */
            unset_ins_property(fs, pc, INS_WHILESTAT);
            unset_ins_property(fs, endpc, INS_WHILESTATEND);
            /* add repeat-loop markers */
            init_ins_property(fs, pc, INS_REPEATSTAT);
            init_ins_property(fs, endpc, INS_REPEATSTATEND);
            lua_assert(!test_ins_property(fs, endpc, INS_LOOPPASS) &&
                       !test_ins_property(fs, endpc, INS_LOOPFAIL));
            /* do not change the repeat/while counters; while-count will be
               decremented at the end as needed, and repeat-count will not
               change which is correct because it was never incremented to begin
               with */
          }
#endif
        }
        else {
          lua_assert(!test_ins_property(fs, pc, INS_BREAKSTAT));
        }
      }
      else if (type == BBL1_REPEAT) {
        lua_assert(test_ins_property(fs, pc, INS_REPEATSTAT));
        /* Like with while-loops, pending retusn and condition expressions need
           to be caught here before they go across the start of a repeat-loop.
           This is needed in a case such as the following example:
            local a = i + 1;
            repeat
                if a > 2 then
                    ...
                end
            until ...
           The analyzer will otherwise think that the condition inside the
           repeat-loop is `i + 1 > 2' instead of `a > 2'. */
        if (ca->retpending != -1) {
          init_ins_property(fs, pc, INS_PRERETURN + ca->buildingretval);
          ca->retpending = -1;
        }
        else if (ca->condpending.r1 != -1 || ca->condpending.r2 != -1) {
          ca->condpending.r1 = ca->condpending.r2 = -1;
          init_ins_property(fs, pc, ca->condpending.flag + ca->buildingretval);
        }
      }
      lua_assert(ca->condpending.r1 == -1 && ca->condpending.r2 == -1);
      break;
    }
  }
  exitblock:
  ca->in_bbl[type]--;
  /* pop old values */
  ca->bbl_bounds[BBL1_MAX].start = outerstatstart;
  ca->bbl_bounds[BBL1_MAX].end = outerstatend;
  ca->bbl_bounds[type].start = outertypestatstart;
  ca->bbl_bounds[type].end = outertypestatend;
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
  int i;
  for (i = 0; i < BBL1_MAX; i++) {
    ca.bbl_bounds[i].start = ca.bbl_bounds[i].end = -1;
    ca.in_bbl[i] = 0;
  }
  /* this slot holds the enclosing loop, regardless of its type */
  ca.bbl_bounds[BBL1_MAX].start = ca.bbl_bounds[BBL1_MAX].end = -1;
  ca.pc = f->sizecode - 1; /* will be decremented again to skip final return */
  ca.prevTMode = 0;
  ca.retpending = -1;
  ca.condpending.r1 = ca.condpending.r2 = -1;
  ca.code = f->code;
  ca.reg_properties = luaM_newvector(fs->H, f->maxstacksize, lu_byte);
  UNUSED(D);
  init_ins_property(fs, ca.pc, INS_BLOCKEND); /* mark end of function */
  bbl1(&ca, fs, 0, BBL1_FUNCTION);
  luaM_freearray(fs->H, ca.reg_properties, f->maxstacksize, lu_byte);
  /* check bbl-stack */
  for (i = 0; i < BBL1_MAX; i++) {
    static const char *const bbl1_names[BBL1_MAX] = {
      "BBL1_FUNCTION", "BBL1_WHILE", "BBL1_REPEAT", "BBL1_FOR"
    };
    lua_assert(ca.bbl_bounds[i].start == -1 && ca.bbl_bounds[i].end == -1);
    printf("ca.in_bbl[%s] = (%d)\n", bbl1_names[i], ca.in_bbl[i]);
    lua_assert(ca.in_bbl[i] == 0);
  }
  lua_assert(ca.bbl_bounds[BBL1_MAX].start == -1 &&
             ca.bbl_bounds[BBL1_MAX].end == -1);
  printf("ca.pc == (%d)\n", ca.pc);
  lua_assert(ca.pc <= 0);
  lua_assert(ca.retpending == -1);
  lua_assert(ca.condpending.r1 == -1);
}

static void DecompileFunction(const Proto *f, DFuncState *prev, DecompState *D);

static void DecompileCode(const Proto *f, DFuncState *fs, DecompState *D)
{
  const Instruction *code=f->code;
  int n=f->sizecode;
  int deltaAStreak=0; /* how many times in a row arg A increments */
  DExp *lastreg=NULL;
  lua_assert(n > 0); /* there is always at least OP_RETURN */
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
    printf("-- Decompiling function (%d) named '%s'\n", D->funcidx, getstr(f->name));
  else
    printf("-- Decompiling anonymous function (%d)\n", D->funcidx);
  pass1(f,&fs,D);
  dumploopinfo(f, &fs, D);
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
