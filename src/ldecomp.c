/*
** $Id: ldecomp.c $
** decompile precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define ldecomp_c
#define LUA_CORE

#include "hksclua.h"

#include "lanalyzer.h"
#include "ldebug.h"
#include "ldo.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"

#ifdef HKSC_DECOMPILER

/*
** Check whether an op loads a constant into register A. Helps determine if a
** tested expression starts evaluating at an instruction. Ops that load
** constants warrant their own statements, as otherwise the constant would be
** indexed as an operand directly in the test-instruction.
*/
#define opLoadsK(o) ((o) == OP_LOADK || (o) == OP_LOADBOOL || (o) == OP_LOADNIL)


#define DEFBBLTYPE(e)  #e,
static const char *const bbltypenames [] = {
  BBLTYPE_TABLE
  "MAX_BBLTYPE"
};
#undef DEFBBLTYPE

#define DEFINSFLAG(e)  "INS_" #e,
static const char *const insflagnames [] = {
  INSFLAG_TABLE
  "MAX_INSFLAG"
};
#undef DEFINSFLAG

#define DEFREGFLAG(e)  "REG_" #e,
static const char *const regflagnames [] = {
  REGFLAG_TABLE
  "MAX_REGFLAG"
};
#undef DEFREGFLAG

#define bbltypename(v) (bbltypenames[v])
#define insflagname(v) (insflagnames[v])
#define regflagname(v) (regflagnames[v])

#ifdef LUA_DEBUG
#define info_setprop(fs,pc,val) \
  printf("  marking pc (%d) as %s\n", (pc)+1, insflagname(val)); \
  printf("  previous flags for pc (%d):", (pc)+1); \
  { \
    int flag_index_; \
    for (flag_index_ = 0; flag_index_ < MAX_INSFLAG; flag_index_++) { \
      if ((fs)->a->insproperties[(pc)] & (1 << flag_index_)) \
        printf(" %s", insflagname(flag_index_)); \
    } \
    printf("\n"); \
  } (void)0
#else /* !LUA_DEBUG */
#define info_setprop(fs,pc,val) (void)0
#endif /* LUA_DEBUG */

#define test_ins_property(fs,pc,val) \
  (((fs)->a->insproperties[(pc)] & (1 << (val))) != 0)

#define set_ins_property(fs,pc,val) do { \
  info_setprop(fs,pc,val); \
  (fs)->a->insproperties[(pc)] |= (1 << (val)); \
} while (0)

#define unset_ins_property(fs,pc,val) do { \
  (fs)->a->insproperties[(pc)] &= ~(1 << (val)); \
} while (0)

#define init_ins_property(fs,pc,val) do { \
  lua_assert(!test_ins_property(fs,pc,val)); \
  set_ins_property(fs,pc,val); \
} while (0)

/* for formatting decimal integers in generated variable names */
#define INT_CHAR_MAX_DEC (3 * sizeof(int) * (CHAR_BIT/8))

struct DFuncState;

typedef int (*CheckVarStartsCB)(struct DFuncState *fs, int pc);

typedef struct {
  hksc_State *H;
  struct DFuncState *fs;  /* current function state */
  CheckVarStartsCB varstarts;
  lua_Writer writer;
  void *data;
  int status;
  int usedebuginfo;  /* true if using debug info */
  int matchlineinfo;  /* true if matching statements to line info */
  /* data for the current function's decompilation */
  int indentlevel;  /* the initial indentation level of this function */
  int funcidx;  /* n for the nth function that is being decompiled */
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
  struct DFuncState *prev;  /* enclosing function */
  DecompState *D;  /* decompiler state */
  Analyzer *a; /* function analyzer data */
  const Proto *f;  /* current function header */
  hksc_State *H;  /* copy of the Lua state */
  struct {
    DExp *first, *last;
  } explist;  /* current chain of pending expressions */
  int nexps;  /* number of expressions in current chain */
  int idx;  /* the nth function */
  int nbbl; /* number of basic blocks */
  struct LocVar *locvars;  /* information about local variables */
  int locvaridx;
  int nlocvars;  /* number of local variables declared so far */
  int sizelocvars;
  int pc;  /* current pc */
  int line;  /* actual current line number */
  int linepending;  /* line + number of pending new lines */
  int numdecls;  /* number of initial implicit nil local variables */
  int firstclob;  /* first pc that clobbers register A */
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


static BasicBlock *newbbl(hksc_State *H, int startpc, int endpc, int type) {
  BasicBlock *bbl = luaM_new(H, BasicBlock);
  bbl->next = NULL;
  bbl->nextsibling = NULL;
  bbl->firstchild = NULL;
  bbl->startpc = startpc;
  bbl->endpc = endpc;
  bbl->type = type;
  bbl->isempty = (endpc < startpc);
  return bbl;
}


static void open_func(DFuncState *fs, DecompState *D, const Proto *f) {
  hksc_State *H = D->H;
  Analyzer *a = luaA_newanalyzer(H);
  fs->a = a;
  fs->prev = D->fs;  /* linked list of funcstates */
  fs->D = D;
  fs->H = H;
  D->fs = fs;
  fs->f = f;
  fs->explist.first = fs->explist.last = NULL;
  fs->nexps = 0;
  fs->idx = D->funcidx++;
  fs->nlocvars = 0;
  fs->nbbl = 0;
  if (D->usedebuginfo) { /* have debug info */
    printf("using debug info for function '%s'\n", f->name ? getstr(f->name) : "(anonymous)");
    fs->sizelocvars = f->sizelocvars;
    fs->locvars = f->locvars;
  }
  else {
    fs->sizelocvars = f->maxstacksize;
    a->sizelocvars = fs->sizelocvars;
    a->locvars = luaM_newvector(H, a->sizelocvars, struct LocVar);
    fs->locvars = a->locvars;
  }
  fs->locvaridx = fs->sizelocvars - 1;
  fs->line = (fs->prev != NULL) ? fs->prev->linepending : 1;
  fs->linepending = fs->line;
  fs->firstclob = -1;
  /* allocate vectors for instruction and register properties */
  a->sizeinsproperties = f->sizecode; /* flags for each instruction */
  a->insproperties = luaM_newvector(H, a->sizeinsproperties, InstructionFlags);
  memset(a->insproperties, 0, f->sizecode * sizeof(InstructionFlags));
  a->sizeregproperties = f->maxstacksize; /* flags for each register */
  a->regproperties = luaM_newvector(H, f->maxstacksize, RegisterFlags);
  memset(a->regproperties, 0, a->sizeregproperties * sizeof(RegisterFlags));
}

static void append_func(DFuncState *fs1, DFuncState *fs2);

static void close_func(DecompState *D) {
  DFuncState *fs = D->fs;
  lua_assert(fs->explist.first == NULL);
  lua_assert(fs->explist.last == NULL);
  lua_assert(fs->nexps == 0);
  lua_assert(fs->line == fs->linepending);
  D->funcidx--;
  UNUSED(fs->locvars);
  UNUSED(fs->sizelocvars);
  UNUSED(fs->D->usedebuginfo);
  if (fs->prev != NULL) { /* update line number for parent */
    DFuncState *prev = fs->prev;
    int pending = prev->linepending - prev->line;
    prev->line = fs->line;
    prev->linepending = prev->line + pending;
    append_func(prev, fs);
  }
}

static void addsibling1(BasicBlock *bbl1, BasicBlock *bbl2) {
  lua_assert(bbl1 != NULL);
  bbl1->nextsibling = bbl2;
}

static void addbbl1(DFuncState *fs, int startpc, int endpc, int type) {
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  BasicBlock *curr, *new;
  curr = a->bbllist.first;
  new = newbbl(H, startpc, endpc, type);
  new->next = curr;
  a->bbllist.first = new;
  if (curr == NULL)
    a->bbllist.last = new;
  printf("recording new basic block of type %s (%d-%d)\n",
         bbltypename(type),startpc+1,endpc+1);
}


static void printbblmsg(const char *msg, BasicBlock *block) {
  fputs(msg, stdout);
  fputs(" ", stdout);
  if (block != NULL)
    printf("(%s) (%d-%d)\n", bbltypename(block->type), block->startpc+1,
           block->endpc+1);
  else
    printf("(NULL)\n");
}


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


#ifdef LUA_DEBUG

static void debugbbl(DFuncState *fs, BasicBlock *bbl, int indent) {
  BasicBlock *child = bbl->firstchild;
  BasicBlock *nextsibling = bbl->nextsibling;
  int i;
  for (i = 0; i < indent; i++) {
    printf("  ");
#ifdef HKSC_DECOMP_DEBUG_PASS1
    DumpLiteral("\t",fs->D);
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
  }
  if (indent) printf("- ");
  printf("(%d-%d) %s (%s sibling)\n", bbl->startpc+1, bbl->endpc+1,
         bbltypename(bbl->type), (nextsibling != NULL)?"YES":"NO");
#ifdef HKSC_DECOMP_DEBUG_PASS1
  DumpStringf(fs->D, "%s\n", bbltypename(bbl->type));
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
  while (child != NULL) {
    debugbbl(fs, child, indent+1);
    child = child->nextsibling;
  }
#ifdef HKSC_DECOMP_DEBUG_PASS1
  if (bbl->type != BBL_IF &&
      (bbl->nextsibling == NULL ||
       bbl->nextsibling->type != BBL_ELSE)) {  /* if-statement has no else */
    for (i = 0; i < indent; i++)
      DumpLiteral("\t",fs->D);
    DumpLiteral("END\n",fs->D);
  }
#endif
}

static void debugbblsummary(DFuncState *fs)
{
  Analyzer *a = fs->a;
  fputs("BASIC BLOCK SUMMARY\n"
        "-------------------\n", stdout);
  debugbbl(fs, a->bbllist.first, 0);
  fputs("-------------------\n", stdout);
}

#else

#define debugbblsummary(fs)  ((void)(fs))

#endif /* LUA_DEBUG */


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
  if (!fs->D->usedebuginfo) {
    var->startpc = fs->pc; /* todo: is this always the case? */
    var->endpc = fs->f->sizecode;
    var->varname = createlocvarname(fs, idx);
  }
  decl = locvar2exp(fs, idx);
  delocidx(decl) = idx;
  desettype(decl, DLHS | DDECL);
  if (fs->D->usedebuginfo) {
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
  if (line > 0 && D->matchlineinfo) {
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
  if (D->matchlineinfo) lua_assert(fs->line == line);
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


/* create parameter names for a function if debug info does not provide them */
static void addargs(const Proto *f, DFuncState *fs) {
  struct LocVar *var;
  int i;
  for (i = 0; i < f->numparams; i++) {
    var = &fs->locvars[i];
    if (!fs->D->usedebuginfo) {
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
  if (!fs->D->usedebuginfo) {
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
  if (fs->D->usedebuginfo && idx < fs->sizelocvars) {
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
  if (idx == fs->sizelocvars && fs->D->usedebuginfo)
    return -1; /* debug info tells you it is a temporary register */
  fs->nlocvars++;
  lua_assert(idx < fs->sizelocvars); /* otherwise this is true */
  var = &fs->locvars[idx];
  if (!fs->D->usedebuginfo) {
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
  if (!fs->D->usedebuginfo) {
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
  for (pc = 0; pc < sizecode; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
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
    if (test_ins_property(fs, pc, INS_BRANCHFAIL)) {
      lua_assert(!test_ins_property(fs, pc, INS_BLOCKEND));
      lua_assert(!test_ins_property(fs, pc, INS_BRANCHPASS));
      DumpLiteral("BRANCH FAIL\n", D);
    }
    else if (test_ins_property(fs, pc, INS_BRANCHPASS)) {
      lua_assert(!test_ins_property(fs, pc, INS_BLOCKEND));
      DumpLiteral("BRANCH PASS\n", D);
    }
    if (test_ins_property(fs, pc, INS_BRANCHBEGIN))
      DumpLiteral("BEGIN BRANCH\n", D);
    if (test_ins_property(fs, pc, INS_LOOPFAIL)) {
      lua_assert(!test_ins_property(fs, pc, INS_LOOPPASS));
      lua_assert(!test_ins_property(fs, pc, INS_LOOPEND));
      DumpLiteral("LOOP FAIL\n", D);
    }
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
    if (test_ins_property(fs, pc, INS_DOSTAT))
      DumpLiteral("BEGIN DO\n", D);
    if (test_ins_property(fs, pc, INS_BLOCKEND)) {
      lua_assert(!test_ins_property(fs, pc, INS_BRANCHFAIL));
      lua_assert(!test_ins_property(fs, pc, INS_BRANCHPASS));
      DumpLiteral("END BLOCK\n", D);
    }
    else if (test_ins_property(fs, pc, INS_TESTSETEND))
      DumpLiteral("END TESTSET\n", D);
    if (test_ins_property(fs, pc, INS_EMPTYBLOCK))
      DumpLiteral("EMPTY BLOCK\n", D);
    if (test_ins_property(fs, pc, INS_LOOPEND))
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
      lua_assert(pc + 1 + sbx <= jumplimit);
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
              int prevA = GETARG_A(prev);
              return !(testAMode(prevop) && prevA >= firstreg);
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


/* a loop in the first pass, described by its start and end pc and its type */
struct stat1 {
  int start, end, type;
};


/*
** `Code Analyzer' - the main structure used in the first pass. It populates the
** `insproperties' array of its corresponding `DFuncState'.
*/
typedef struct CodeAnalyzer {
  /* a record of all encountered loops is saved implicitly on the callstack;
     these values hold the bounds of the current loop */
  struct stat1 curr; /* the start and end of the current basic block */
  int pc;  /* current pc */
  /* most of these values could be local to `whilestat1', but are kept in this
     structure to avoid allocating new copies on every recursive call */
  lu_byte prevTMode;  /* previous pc is a test instruction */
  struct {
    int endpc, reg;
  } testset;  /* the current OP_TESTSET expression */
  const Instruction *code;  /* f->code */
} CodeAnalyzer;


/*
** prototypes for recursive non-terminal functions
*/
static void callstat1(CodeAnalyzer *ca, DFuncState *fs);
static void concat1(CodeAnalyzer *ca, DFuncState *fs);


#define STAT1_CASE_CALL(label) \
  CASE_OP_CALL: \
    callstat1(ca, fs); \
    pc = ca->pc; \
    o = GET_OPCODE(code[pc]); \
    if (firstreg == a && !isOpCall(o)) \
      goto label; \
    break;

#define encounteredstat1(what) \
  printf("  encountered " what " ending at (%d)\n", endpc+1)

#define leavingstat1(what) \
printf("  leaving " what " ending at (%d), (%d-%d)\n", endpc+1,ca->pc+1,endpc+1)

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
  encounteredstat1("concat");
  for (; ca->pc > 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        pc = ca->pc;
        o = GET_OPCODE(code[pc]);
        if (firstreg == a && !isOpCall(o))
          goto markconcat;
        break;
      case OP_CONCAT:
      case OP_RETURN:
        lua_assert(0);
      default:
        if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          markconcat:
          init_ins_property(fs, pc, INS_PRECONCAT);
          leavingstat1("concat");
          return;
        }
        break;
    }
  }
  init_ins_property(fs, 0, INS_PRECONCAT);
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
  encounteredstat1("call");
  for (; ca->pc > 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int c = GETARG_C(i);
    switch (o) {
      /*case OP_SELF:
        lua_assert(a == firstreg);
        break;*/
      case OP_CONCAT:
        concat1(ca, fs);
        /* The beginnig of the concat expression may already have been marked
           as a pre-call in a nested `callstat1' call, for example in the
           following case:
            ((function () end)() .. a)();
           In this case, mark the pc of OP_CONCAT with INS_PRECALL. Like OP_CALL
           and its variants, OP_CONCAT cannot start a call expression, so this
           is safe to use as a marker for this special case. */
/*        if (a == firstreg && test_ins_property(fs, ca->pc, INS_PRECALL)) {
          init_ins_property(fs, pc, INS_PRECALL);
          leavingstat1("call");
          return;
        }*/
        if (a == firstreg) {
          if (!test_ins_property(fs, ca->pc, INS_PRECALL))
            pc = ca->pc; /* mark actual startpc */
          goto markcall;
        }
        pc = ca->pc;
        i = code[pc];
        o = GET_OPCODE(i);
        a = GETARG_A(i);
        c = GETARG_C(i);
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
            goto markcall;
          }
        }
        else if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          markcall:
          init_ins_property(fs, pc, INS_PRECALL);
          leavingstat1("call");
          return;
        }
        break;
      }
    }
  }
  init_ins_property(fs, 0, INS_PRECALL);
}

/* only put this before the default case */
#define STAT1_CASE_CONCAT \
  case OP_CONCAT: \
    concat1(ca, fs); \
    pc = ca->pc; \
    i = code[pc]; \
    o = GET_OPCODE(i); \
    a = GETARG_A(i); \
    /* fallthrough */

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
static void retstat1(CodeAnalyzer *ca, DFuncState *fs)
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
      return;
    }
    else if (nret == 1) {
      /* OP_RETURN is usually not the beginning of its own statement, but in
         this case, when there is only 1 return value, if argA is a local
         variable register, it does begin the statement. It is impossible right
         now to say whether it is a local variable or a temporary value */
      return;
    }
    ca->pc--;
  }
  encounteredstat1("return");
  for (; ca->pc > 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      STAT1_CASE_CALL(markret)
      STAT1_CASE_CONCAT
      default:
        lua_assert(o != OP_RETURN); /* cannot have nested return statements */
        if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          markret:
          init_ins_property(fs, pc, INS_PRERETURN);
          leavingstat1("return");
          return;
        }
        break;
    }
  }
  init_ins_property(fs, 0, INS_PRERETURN);
  return;
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
  encounteredstat1("numeric for-loop prep");
  for (; ca->pc > 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      STAT1_CASE_CALL(markfornum)
      STAT1_CASE_CONCAT
      default:
        if (beginstempexpr(code, i, pc, firstreg, startpc)) {
          markfornum:
          init_ins_property(fs, pc, INS_PREFORNUM);
          leavingstat1("numeric for-loop prep");
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
  /*lua_assert(ca->pc < 0);*/
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
  encounteredstat1("list for-loop prep");
  for (; ca->pc > 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      STAT1_CASE_CALL(markforlist)
      STAT1_CASE_CONCAT
      default:
        if (beginstempexpr(code, i, pc, firstreg, startpc)) {
          markforlist:
          init_ins_property(fs, pc, INS_PREFORLIST);
          leavingstat1("list for-loop prep");
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
  /*lua_assert(ca->pc < 0);*/
  init_ins_property(fs, 0, INS_PREFORLIST);
}


#define encountered1(what,pc) printf("  encountered a " what " loop starting" \
" at (%d)\n", (pc)+1)


#define loop1(ca,fs,startpc,type) do { \
  struct BasicBlock *new; /* the new block */ \
  /* create the new block */ \
  bbl1(ca, fs, startpc, type, NULL, NULL, nextsibling); \
  new = fs->a->bbllist.first; \
  lua_assert(new != nextsibling); \
  addsibling1(new, nextsibling); \
  nextsibling = new; \
  /* if enclosed by a block, save a previous sibling for it, as the block may \
     really begin after this newly created block */ \
  if (block != NULL && block->state.prevsibling == NULL) \
    block->state.prevsibling = new; \
  if (branch != NULL && branch->elseprevsibling == NULL) \
    branch->elseprevsibling = new; \
} while(0)


struct block1;


/*
** `branch1' holds the context for an if-else branch construct in the first pass
*/
struct branch1 {
  struct branch1 *prev;  /* enclosing branch context if nested */
  struct block1 *parentblock;  /* enclosing block if nested */
  /*BasicBlock *next;*/  /* the next branch block in this group */
  BasicBlock *ifblock;  /* first if-part */
  BasicBlock *elseblock;  /* the else-part */
  BasicBlock *elseprevsibling;  /* the preceding sibling of the else-part */
  BasicBlock *firstblock;  /* the first block in the branch context, used when
                              no if-part is found when returning, meaning the
                              detection was erroneous and this block needs to be
                              accounted for in the parent call */
  struct stat1 *outer;  /* saved values for the outer loop statement */
  int target1;  /* jump target out of the branch condition */
  int startpc, endpc, midpc;  /* midpc is the start of the else-part */
  int optimalexit;  /* target of optimized jumps out of this branch */
  int contextswitched;  /* this flag was created to handle to the case where an
                           erroneous branch detection is discovered at the time
                           when a new branch has been detected; if this happens,
                           there is an enclosing branch context to return from,
                           the one which was erroneous, and the context for it
                           needs to be switched out with the new branch
                           detection, and the recursion performed again with the
                           new context, and the erroneous branch needs to be
                           changed to a repeat-loop node and the sibling chain
                           needs to be fixed */
};


/*
** `blockstate1' holds all block data that is continuously updated while within
** a block or branch context
*/
struct blockstate1 {
  int startpc;
  BasicBlock *firstchild;  /* see explanation in `block1' */
  BasicBlock *prevsibling;  /* see explanation in `block1' */
};


/*
** `block1' holds the context for a do-end block in the first pass
*/
struct block1 {
  struct block1 *prev;  /* parent block context if nested */
  /* the following fields may have less obvious purposes than it seems */
  /* `nextbranch' is used when an else-branch is encountered and recursed into,
     where the else-branch closes variables, which caused the analyzer to think
     that there was a do-end block which wasn't there. Because at that point the
     analyzer would have already recursed into a do-block context, it passes the
     resulting data (needed after the branch recursion) with the block context
     and then returns for that context. This way, the branch data gets saved
     even when the function returns again (after returning from the branch
     context) in order to pop off the erroneous block context */
  BasicBlock *nextbranch;
  /* `nextsibling' is used when 2 adjacent blocks are detected. The analyzer
     will end the existing block and overwrite the current context for the new
     block instead of recursing into a new context. In this case, the basic
     block entry for the adjacent block is created and the function continues
     as normal without returning or calling. This field is saved for each new
     block context so that when adjacent blocks are encountered, the block can
     be created and linked to its sibling wihtout returning to the caller which
     has the correct `nextsibling' */
  BasicBlock *nextsibling;
  /* `prevsibling' holds the last sibling before the start of the do-block. This
     is needed because do-blocks will not always return on the exact pc that
     they actually start. The analyzer will keep going until the start of the
     enclosing loop/function and then truncate the block to start on the first
     instruction that clobbers the first register that is closed by the block.
     The `nextsibling' block will continue to be updated even past the real
     start of the block, so the block's true previous sibling is saved */
  /*BasicBlock *prevsibling;*/
  /* `firstsibling' is used to pass the local variable `nextsibling' back to the
     caller, so that it may update its `nextsibling' to that value */
  BasicBlock *firstsibling;
  /* `firstchild' holds the first block that starts after the real start of the
     block. This is needed for the same reason as described in the description
     for `prevsibling' */
  /*BasicBlock *firstchild;*/
  struct stat1 *outer;  /* saved values for the outer loop statement */
  int pass;  /* true if passing data to its caller/parent block */
  int reg;  /* register to close up to in OP_CLOSE */
  int seenclosure;
  int endpc;
  struct blockstate1 state;  /* todo: explain these fields */
  struct blockstate1 possiblestate;
  /*int startpc, endpc;*/
};


static BasicBlock *fixsiblingchain1(BasicBlock *block, BasicBlock **chain) {
  int endpc = block->endpc;
  BasicBlock *lastchild = NULL;
  BasicBlock *nextsibling;
  BasicBlock *firstsibling;
  lua_assert(chain != NULL);
  nextsibling = firstsibling = *chain;
  printf("fixing up sibling chain\n");
  printf("--\n");
  while (nextsibling && nextsibling->startpc <= endpc) {
    lastchild = nextsibling;
    printbblmsg("found child", nextsibling);
    nextsibling = nextsibling->nextsibling;
  }
  printf("--\n");
  *chain = nextsibling;
  if (lastchild) {
    block->firstchild = firstsibling;
    if (firstsibling) {
      printbblmsg("first child is", firstsibling);
    }
    /* fix sibling chain for last child */
    lastchild->nextsibling = NULL;
    printbblmsg("last child is", lastchild);
  }
  else {
    printf("found no children for this block\n");
  }
  printbblmsg("next sibling changed to", nextsibling);
  printbblmsg("previous next sibling was", block->nextsibling);
  lua_assert(block->nextsibling == NULL || block->nextsibling == firstsibling);
  block->nextsibling = nextsibling;
  return lastchild;
}


static void newbranch1(DFuncState *fs, struct branch1 *branch, int midpc,
                       int endpc, int jumptarget, BasicBlock **nextsibling)
{
  BasicBlock *block;
  branch->startpc = -1;  /* startpc of the if-part is unknown right now */
  branch->midpc = midpc;
  branch->endpc = endpc;
  branch->target1 = -1;
  if (branch->prev)
    branch->optimalexit = branch->prev->optimalexit;
  else
    branch->optimalexit = jumptarget;
  addbbl1(fs, midpc, endpc, BBL_ELSE);
  block = fs->a->bbllist.first;
  if (branch->prev != NULL && branch->prev->elseprevsibling == NULL)
    branch->prev->elseprevsibling = block;
  lua_assert(block != NULL);
  printf("ELSE BLOCK - (%d-%d)\n", midpc+1, endpc+1);
  fixsiblingchain1(block, nextsibling);
  branch->elseblock = block;
  branch->ifblock = NULL;
  branch->elseprevsibling = NULL;
  branch->firstblock = NULL;
  branch->contextswitched = 0;
}



static void bbl1(CodeAnalyzer *ca, DFuncState *fs, int startpc, int type,
       struct branch1 *branch, struct block1 *block, BasicBlock *futuresibling)
{
  struct stat1 outer; /* the outer loop context, saved on the stack */
  BasicBlock *nextsibling; /* the most recently created block; it gets assigned
                              to the ->nextsibling field for each newly created
                              BasicBlock, before being set to the new block */
  BasicBlock *nextbranch = NULL;
  int nextbranchtarget = -1;
  int endpc; /* endpc of the current block */
  const Instruction *code = ca->code;
  if (branch == NULL && block == NULL) { /* inside a loop or function */
    nextsibling = NULL;
    endpc = ca->pc;
    outer = ca->curr; /* save old values */
    ca->curr.start = startpc; /* update current */
    ca->curr.end = endpc;
    ca->curr.type = type;
    printf("entering new basic block of type (%s)\n", bbltypename(type));
  }
  else {
    struct stat1 *outersaved;
    if (branch != NULL) { /* inside a branch construct */
      lua_assert(block == NULL);
      lua_assert(branch->elseblock != NULL);
      nextsibling = branch->elseblock; /* the else-block */
      outersaved = branch->outer;
      printf("entering new branch\n");
    }
    else { /* inside a block */
      lua_assert(block != NULL);
      nextsibling = NULL;
      outersaved = block->outer;
      printf("entering new do-block\n");
    }
    endpc = ca->curr.end;  /* endpc must be the end of the current loop */
    lua_assert(outersaved != NULL);
    outer = *outersaved;
  }
  ca->pc--;
  for (; ca->pc >= 0; ca->pc--) {
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    int sbx = GETARG_sBx(i);
    printf("pc %d: OP_%s\n", pc+1, getOpName(o));
    switch (o) {
      case OP_JMP: {
        /* todo: check that target is a valid pc */
        int target = pc + 1 + sbx; /* the jump target pc */
        int branchstartpc, branchendpc; /* these are used when creating
                                           branch BasicBlocks (BBL_IF/BBL_ELSE),
                                           they are declared here because they
                                           get set in different control paths
                                           depending on the type of jump */
        int tailbranchjump=0; /* this is set to true if the jump is a
                                 branch-exit, where the branch is at the end of
                                 a while-loop body, and the jump is optimized
                                 to jump back to the start of the while-loop,
                                 also declared here as it gets set in a
                                 different control path than the one which
                                 handles branch constructs */
        OpCode prevop; /* previous opcode to see if it is a test instruction */
        if (test_ins_property(fs, pc+1, INS_FORLIST)) {
          forlistprep1(ca, fs, target);
          break;
        }
        if (pc == 0) {
          ca->prevTMode = 0; /* unconditional jump */
          goto jmpisfirst;
        }
        /* excluding the above cases, this cannot be the first instruction
           (this is ok as an assert as long as PC is checked against zero
           above) */
        lua_assert(pc > 0);
        prevop = GET_OPCODE(code[pc-1]);
        /* OP_TESTSET needs to be handled differently than other test opcodes */
        if (prevop == OP_TESTSET) {/* a jump after OP_TESTSET is not a branch */
          if (ca->testset.endpc == -1) { /* no pending testset expression */
            ca->testset.endpc = target; /* create a new testset expression */
            init_ins_property(fs, target-1, INS_TESTSETEND);
            /* Some testset expressions can end with non-testset-controlled
               jumps, i.e. jumps that follow a test instruction that is not
               OP_TESTSET. For example:
                  local a;
                  a = a .. b or a or b;
               The above code will generate the following opcodes:
               OP_TESTSET      -- if (a .. b) then a = a .. b;
               OP_JMP          -- true-exit
               OP_TEST         -- if (a) then ; -- do nothing
               OP_JMP          -- true-exit
               OP_GETBLOBAL    -- if (b) then a = b;
               The code analyzer will encounter the final jump first, and it
               will detect a branch. But now, it has encountered the OP_TESTSET,
               so it has to check if it previously marked the jump target as the
               end of a branch, and correct the erroneous branch detection. */
            if (test_ins_property(fs, target-1, INS_BLOCKEND)) {
              int testsetpc; /* pc to traverse this testset expression */
              /* there can be no blocks inside a testset expression, this was
                 a false positive; unmark the target as a block-ending */
              unset_ins_property(fs, target-1, INS_BLOCKEND);
              /* todo: should this be a code check instead? */
              lua_assert(nextsibling == NULL ||
                         nextsibling == fs->a->bbllist.first);
              printbblmsg("nextbranch =", nextbranch);
              printf("cleaning up testset expression from (%d-%d)\n",
                     pc+1, target);
              printf("--\n");
              /* now, any and all blocks that were detected within this testset
                 expression need to be deleted, as they are all false */
              for (testsetpc = pc; testsetpc < target; testsetpc++) {
                /* delete all blocks which start on TESTSETPC */
                while (nextsibling && nextsibling->startpc == testsetpc) {
                  BasicBlock *nextnextsibling = nextsibling->nextsibling;
                  /* unlink NEXTSIBLING from the chain; it will be deleted */
                  fs->a->bbllist.first = nextsibling->next;
                  if (nextsibling == fs->a->bbllist.last)
                    fs->a->bbllist.last = fs->a->bbllist.first;
                  printbblmsg("deleting erroneous block", nextsibling);
                  /* todo: what if the block endpc is after target? */
                  luaM_free(fs->H, nextsibling);
                  nextsibling = nextnextsibling; /* update NEXTSIBLING */
                }
                /* make sure none of these markings are on this pc */
                unset_ins_property(fs, testsetpc, INS_BRANCHFAIL);
                unset_ins_property(fs, testsetpc+1, INS_BRANCHBEGIN);
                unset_ins_property(fs, testsetpc, INS_BRANCHPASS);
                unset_ins_property(fs, testsetpc, INS_BLOCKEND);
              }
              printf("--\n");
              /* also update NEXTBRANCH */
              if (nextsibling && nextsibling->type == BBL_IF)
                nextbranch = nextsibling;
              else
                nextbranch = NULL;
              printbblmsg("nextbranch =", nextbranch);
            }
          }
          else { /* part of a pending testset expression */
            if (ca->testset.endpc != target) {
              /* bad code */
            }

          }
          break;
        }
        ca->prevTMode = testTMode(prevop);
        jmpisfirst:
        if (ca->testset.endpc != -1) { /* in a OP_TESTSET expression */
          /* an unconditional jump cannot be in a conditional expression, nor a
             backwards jump, nor one which jumps past the expression */
          if (!ca->prevTMode || sbx < 0 || target > ca->testset.endpc)
            ca->testset.endpc = ca->testset.reg = -1; /* discharge */
          else
            break; /* this jump is part of the conditional expression */
        }
        if (sbx < 0) { /* backward jump */
          if (ca->prevTMode) { /* conditional backward jump */
            if (prevop == OP_TFORLOOP) { /* for-list-loop */
              encountered1("for-list", target);
              init_ins_property(fs, target, INS_FORLIST);
              init_ins_property(fs, pc, INS_LOOPEND);
              loop1(ca, fs, target, BBL_FORLIST);
            }
            else { /* either an optimized jump or a repeat-loop */
              /* check if this jumps to the start of a while-loop first; in that
                 case, this is an optimized jump either from a failed branch
                 test or a failed inner while-loop condition, e.g.:
                  failed tail branch:
                      while a do
                          local b;
                          if b then -- fail-jumps back to start of loop
                              ...
                          end
                      end
                  failed inner-loop test:
                      while a do
                          local b;
                          while b do -- fail-jumps back to start of outer loop
                              ...
                          end
                      end
                  */
              if (type == BBL_WHILE) { /* inside a while-loop */
                if (target == startpc) { /* jumps to the start of the loop */
                  /* this is an optimized jump from a branch test that would
                     otherwise jump to the very end of the loop */
                  tailbranchjump=1;
                  branchendpc = endpc-1;
                  init_ins_property(fs, pc, INS_BRANCHFAIL);
                  goto ifbranch;
                }
                else if (target < startpc) { /* jumps to an enclosing loop */
                  /* this is an optimized jump from a loop test that would
                     otherwise jump to the very end of an enclosing loop */
                  init_ins_property(fs, pc, INS_LOOPFAIL);
                }
                else { /* a repeat-loop fail-jump */
                  goto markrepeatstat;
                }
              }
              else if (type == BBL_REPEAT) { /* inside a repeat-loop */
                if (target == startpc) { /* jumps to the start of the loop */
                  /* this is a new repeat-loop that starts on the same
                     instruction as an enclosing repeat-loop, like the following
                     example:
                        repeat
                            repeat
                                ...
                            until b
                        until a */
                  /* todo: should this be an assert or a code check? */
                  lua_assert(test_ins_property(fs, target, INS_REPEATSTAT));
                  goto markrepeatstat;
                }
                /* todo: what should be done in this case? */
              }
              else { /* a repeat-loop fail-jump */
                markrepeatstat:
                encountered1("repeat", target);
                set_ins_property(fs, target, INS_REPEATSTAT);
                init_ins_property(fs, pc, INS_LOOPEND);
                init_ins_property(fs, pc, INS_LOOPPASS);
                loop1(ca, fs, target, BBL_REPEAT);
              }
            }
          }
          else { /* unconditional backward jump */
            if (prevop == OP_CLOSE) {
              int pc1=pc-2; /* separate pc variable for looking-behind, starting
                               with the one before the previous */
              const OpCode o1[2] = {OP_JMP, OP_CLOSE}; /* the pattern to match
                                                          alternates between
                                                          these codes */
              int i1=0; /* state machine variable */
              while (pc1 >= 0) {
                if (GET_OPCODE(code[pc1]) == o1[i1%2]) {
                  i1++;
                  if (i1 == 3) {
                  /* At this point there is:
                      OP_JMP    <-- pc1
                      OP_CLOSE
                      OP_JMP
                      OP_CLOSE  <-- prevop (pc-1)
                      OP_JMP    <-- o (pc)
                     This can either be a while-loop with multiple break
                     statements at the end or a repeat-loop, both containing
                     local variables that are used as upvaleus:
                     while-loop:
                        while a do
                            local b;
                            local function c()
                               b = b + 1;
                            end
                            do break; end
                            do break; end
                        end
                     repeat-loop
                        repeat
                            local b;
                            local function c()
                               b = b + 1;
                            end
                        until a;
                     In the case of a repeat-loop, the instruction that precedes
                     the OP_JMP at `pc1' must be a test instruction, and the
                     jump instruction at PC1 jumps to the instruction at (PC-1)
                     */
                    Instruction ins1 = code[pc1]; /* OP_JMP */
                    pc1--;
                    /* need at least 1 more preceding instruction for it to be a
                       repeat-loop */
                    if (pc1 >= 0) {
                      /* INS1 is the first jump instruction in the pattern
                         given above. It should target the last OP_CLOSE in said
                         pattern, i.e. (PC-1), if it is a repeat-loop */
                      if ((pc1 + 1 + 1 + GETARG_sBx(ins1)) == (pc - 1)) {
                        /* a normal repeat-loop tests a condition before the
                           jump at PC1 */
                        /*if (testTMode(GET_OPCODE(code[pc1])))*/
                          goto markrepeatstat;
                      }
                    }
                    /* while-loop is handled below */
                    break; /* pattern matched */
                  }
                }
                else
                  break; /* pattern not matched */
                pc1--;
              }
              /* fallthrough */
            }
            /* inside a while-loop, jumping unconditionally back to the start
               is an optimized jump out of a branch-block which would otherwise
               jump to the very end of the loop, eg.:
                  while a do
                      local b;
                      if b then
                          ...
                      else -- jump back to start of the while-loop
                          ...
                      end
                  end */
            if (type == BBL_WHILE && target == startpc) {
              /* ...unless this jump is immediately before the end of the
                 enclosing loop, in which case there is a nested while-loop,
                 e.g.:
                    while true do
                        while a do
                            local b = 12;
                        end -- OP_JMP to pc=0
                    end -- OP_JMP to pc=0
                 though the decompiler detects this as:
                    while true do
                        while true do
                            if a then
                                local b = 12;
                            end
                        end
                    end
                 which is semantically equivalent and generates the same code
                 NOTE: this could also be the following code:
                    while a do
                        ...
                        repeat
                            break;
                        until true;
                    end
                 this is a special case that is handled later, in the code right
                 before the if-branch handler code below, once the while-loop
                 context has already been recursed into */
              if (pc == endpc-1)
                goto markwhilestat;
              /* this instruction is the end of a tail-if-block with an
                 else-part */
              branchendpc = endpc-1;
              goto elsebranch;
            }
            /* inside a loop enclosed by a while-loop, jumping back to the start
               of the while-loop, i.e. an optimized `break' out of the
               inner-loop, e.g.:
                  while a do
                      local a = 12;
                      while b do
                          local b = 34;
                          do break; end
                      end
                  end
               */
            else if (type != BBL_FUNCTION && outer.type == BBL_WHILE &&
                     target == outer.start) {
              init_ins_property(fs, pc, INS_BREAKSTAT);
            }
            else { /* a while-loop */
              markwhilestat:
              encountered1("while", target);
              set_ins_property(fs, target, INS_WHILESTAT);
              init_ins_property(fs, pc, INS_LOOPEND);
              /* if this while-loop is at the end of an enclosing block, mark
                 the exit-target of the enclosing block as a potential target
                 for optimized exits out of this while-loop */
              if (test_ins_property(fs, pc+1, INS_BLOCKEND)) {
                Instruction nexti = code[pc+1];
                int nexttarget;
                /* only a jump could be marked as a block ending this near
                   removed */
                lua_assert(GET_OPCODE(nexti) == OP_JMP);
                nexttarget = pc + 1 + 1 + GETARG_sBx(nexti);
                if (nexttarget > pc + 1) /* really a branch exit */
                  /* this while-loop is at the very end of a branch-block, which
                     means the condition at the beginning of the loop will have
                     an optimized fail-jump; the target that the while-loop will
                     fail-jump to is marked now and used later to determine
                     whether a conditional jump is part of a loop or a branch */
                  init_ins_property(fs, nexttarget, INS_OPTLOOPFAILTARGET);
              }
              loop1(ca, fs, target, BBL_WHILE);
            }
          }
        }
        else { /* forward jump (sbx >= 0) */
          if (target > endpc+1) { /* jumps past the end of the enclosing loop */
            /* this jump must be optimized if its target is outside the
               enclosing loop; it could be a break or a fail-exit from the loop
               condition, or an unconditional exit in the case of `while false'
               */
            Instruction siblingjump;
            int siblingtarget;
            lua_assert(type != BBL_FUNCTION);
            /* the current loop must have a sibling for it to be an optimized
               exit */
            if (futuresibling != NULL) {
              siblingjump = code[futuresibling->startpc];
              if (GET_OPCODE(siblingjump) != OP_JMP) {
                /* todo: bad code? */
                lua_assert(0);
              }
              siblingtarget = futuresibling->startpc+1+GETARG_sBx(siblingjump);
              if (target == siblingtarget) { /* optimized jump? */
                if (ca->prevTMode || pc == startpc) /* loop-exit */
                  init_ins_property(fs, pc, INS_LOOPFAIL);
                else /* break */
                  init_ins_property(fs, pc, INS_BREAKSTAT);
                break;
              }
              else { /* bad code? */
                lua_assert(0);
              }
            }
            /* fallthrough */
          }
          if (ca->prevTMode) { /* conditional forward jump */
            /* check for a fail-jump out of a repeat-loop condition */
            if (type == BBL_REPEAT && target-1 == endpc) {
              markrepeatfail:
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else if (type == BBL_REPEAT &&
                     test_ins_property(fs, pc, INS_LOOPPASS)) {
              break; /* already marked; do nothing */
            }
            /* if the jump skips over a false-jump, this is a true-jump */
            else if (test_ins_property(fs, target-1, INS_LOOPFAIL)) {
              init_ins_property(fs, pc, INS_LOOPPASS);
            }
            /* if the jump sips over a true-jump, this is a false-jump */
            else if (test_ins_property(fs, target-1, INS_LOOPPASS)) {
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            /* check for a fail-jump out of a while-loop condition */
            else if (type == BBL_WHILE &&
                      /* a break statement follows the while-loop, which means
                         this fail-jump is optimized */
                     ((target-1 == outer.end &&
                       test_ins_property(fs, endpc+1, INS_BREAKSTAT)) ||
                      /* the typical case, a jump past the end of the loop */
                      (target-1 == endpc) ||
                      /* a specially marked target for optimized fail-jumps */
                      (test_ins_property(fs, target, INS_OPTLOOPFAILTARGET)))) {
              lua_assert(outer.end != -1);
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            /* before assuming this is an if-branch, check the jump target and
               the enclosing loops to see if there has been an erroneous
               while-loop detection caused by an optimized jump from an `if
               false' or a `repeat break' statement at the end of the enclosing
               while-loop */
            else if (type == BBL_WHILE && outer.type == BBL_WHILE &&
                     target > outer.end) {
              /* todo: should this be a code check */
              lua_assert(branch == NULL && block == NULL);
              if (target-1 == outer.end && outer.end-1 == endpc &&
                  futuresibling == NULL) {
                init_ins_property(fs, pc, INS_LOOPFAIL);
                /* this block is really `if false then ... end' or equivalent */
                addbbl1(fs, endpc+1, endpc, BBL_IF);
                ca->testset.endpc = ca->testset.reg = -1;
                ca->curr = outer;
                return;
              }
              else { /* todo: when is this the case? */
                lua_assert(0);
              }
            }
            else { /* jumping inside a branch-condition */
              if (test_ins_property(fs, target-1, INS_BRANCHFAIL))
                init_ins_property(fs, pc, INS_BRANCHPASS);
              else {
                init_ins_property(fs, pc, INS_BRANCHFAIL);
                if (!test_ins_property(fs, target-1, INS_BRANCHPASS)) {
                  int issingle; /* true if this branch has no else-block; this
                                   is significant because only branches with an
                                   else-block cause a recursive call, whereas
                                   else-less branches are always child blocks in
                                   the current branch context */
                  BasicBlock *new_block;
                  ifbranch:
                  branchstartpc = pc+1;
                  if (branch != NULL)
                    branch->startpc = branchstartpc;
                  /* determine if this is a single branch statement ot not and
                     calculate the endpc of the if-part */
                  if (branch != NULL &&
                      (target == branch->midpc || tailbranchjump)) {
                    branchendpc = branch->midpc-1; /* the end of the if-part */
                    issingle = 0; /* there is an else-block */
                  }
                  else {
                    if (tailbranchjump && branch == NULL)
                      /* a single tail if-statement in a while-loop */
                      branchendpc = endpc-1;
                    else if (branch == NULL || target < branch->midpc)
                      /* a single (possibly child) if-statement */
                      branchendpc = target-1;
                    else
                      branchendpc = branch->midpc-1;
                    /*  */
                    if (nextbranch != NULL) {
                      BasicBlock *next = nextbranch;
                      lua_assert(nextbranchtarget != -1);
                      if (next->endpc == branchendpc &&
                          nextbranchtarget == target) {
                        new_block = next;
                        printbblmsg("merging this branch with the next block",
                                    next);
                        new_block->startpc = branchstartpc;
                        issingle = 0;
                        nextsibling = new_block->nextsibling;
                        goto blockcreated;
                      }
                    }
                    issingle = 1;
                  }
                  nextbranch = NULL;
                  nextbranchtarget = -1;
                  if (branch) branch->target1 = target;
                  if (issingle && branch != NULL) {
                    printf("THIS BRANCH IS A CHILD BLOCK of the top-level one\n");
                  }
                  addbbl1(fs, branchstartpc, branchendpc, BBL_IF);
                  new_block = fs->a->bbllist.first;
                  blockcreated:
                  /* BRANCHENDPC can be less than BRANCHSTARTPC if the block is
                     empty */
                  if (branchstartpc <= branchendpc) {
                    init_ins_property(fs, branchstartpc, INS_BRANCHBEGIN);
                    set_ins_property(fs, branchendpc, INS_BLOCKEND);
                  }
                  else { /* empty block */
                    init_ins_property(fs, branchstartpc, INS_EMPTYBLOCK);
                  }
                  lua_assert(new_block != NULL);
                  printbblmsg("BRANCH BLOCK -", new_block);
                  fixsiblingchain1(new_block, &nextsibling);
                  nextsibling = new_block;
                  if (branch) {
                    if (nextbranch == NULL) {
                      branch->ifblock = new_block;
                      branch->firstblock = new_block;
                      ca->testset.endpc = ca->testset.reg = -1;
                      return;
                    }
                    else {
                      nextbranch = new_block;
                      nextbranchtarget = target;
                    }
                  }
                }
              }
            }
          }
          else { /* unconditional forward jump */
            if (prevop == OP_CLOSE && type == BBL_REPEAT && target-1 == endpc) {
              /* need at least 3 preceding instructions to check for a
                 repeat-loop */
              if (pc >= 2 && GET_OPCODE(code[pc-2]) == OP_JMP) {
                if (((pc-2) + 1 + GETARG_sBx(code[pc-2])) == (pc+1)) {
                  /* mark PC-2 now so it doesn't get detected later as something
                     else */
                  init_ins_property(fs, pc-2, INS_LOOPPASS);
                  goto markrepeatfail;
                }
              }
#if 0
              if (pc > 3 && GET_OPCODE(code[pc-2]) == OP_JMP) {
                if (testTMode(GET_OPCODE(code[pc-3])))
                  goto markrepeatfail; /* fail-jump out of repeat-loop */
                else {
                  /* in a repeat-loop, OP_JMP at PC1 should jump to (PC+1):
                       JMP     2 <-- PC1  (jump to PC+1)
                       CLOSE   n <-- PC1+1 (PREVOP)
                       JMP     2 <-- PC
                       CLOSE   n <-- PC+1
                       JMP     (to start of repeat-loop)
                   */
                  if ((pc-2 + 1 + GETARG_sBx(code[pc1])) == (pc+1)) {
                  /*if (pc1 >= 0 && testTMode(GET_OPCODE(code[pc1])))*/
                    /* mark this now so it doesn't get detected as something
                       else */
                    init_ins_property(fs, pc-2, INS_LOOPPASS);
                    goto markrepeatfail; /* fail-jump out of repeat-loop */
                  }
                }
              }
#endif
              /* fallthrough */
            }
            if (type == BBL_REPEAT && target <= endpc &&
                     test_ins_property(fs, pc, INS_LOOPPASS)) {
              break; /* already marked from above; do nothing */
            }
            else if (test_ins_property(fs, target-1, INS_LOOPEND) &&
                target-1 == endpc) { /* break statement */
              /* a while-loop can begin with a jump that is not a break if it
                 has a literal `false' condition */
              if (pc != startpc || type != BBL_WHILE)
                init_ins_property(fs, pc, INS_BREAKSTAT);
              else /* (pc == startpc && type == BBL_WHILE) */
                init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else { /* exiting a branch */
              struct block1 *parentblock;
              struct branch1 new_branch;
              int noblock; /* true if the analyzer marked this branch as a
                              do-block and needs to correct */
              if (branch != NULL &&
                  (target == branch->optimalexit || target == branch->endpc+1))
                branchendpc = branch->midpc-1;
              else
                branchendpc = target-1;
              elsebranch:
              if (branch != NULL) {
                printf("nested branch ends at (%d), the enclosing if-branch "
                       "ends at (%d)\n", branchendpc+1, branch->midpc+1);
              }
              /*if (branch != NULL && branchendpc >= branch->midpc) {
              }*/
              /* something is wrong if this new else-branch ends after an
                 enclosing if-branch ends  */
              if (branch != NULL && branchendpc >= branch->midpc) {
                /* the problem: a new branch has been encountered and a context
                   for it must be created, but simultaneously, the enclosing
                   branch context is false and needs to be returned from */
                /* the idea: switch out the branch context for the enclosing
                   one, return from the enclosing context, fix the erroneous
                   block, and recurse into the new branch context */
                printf("existing branch-context is erroneous, performing "
                       "context-switch and returning from context before "
                       "recursing\n");
                lua_assert(branch->contextswitched == 0);
                branch->contextswitched = 1;
                lua_assert(branch->ifblock == NULL);
                branch->firstblock = nextsibling;
                branch->endpc = branchendpc;
                return;
              }
              if (block != NULL && block->endpc <= branchendpc) {
                struct block1 *bl = block;
                struct block1 *lastbl = NULL;
                int i=0;
                BasicBlock *childblock = bl->state.firstchild;
                printf("encountered an else-block which closes variables, "
                       "correcting erroneous do-block detection\n");
                /* the OP_CLOSE at the end of this branch is not for a block */
                noblock = 1;
                /* need to close all the child blocks that are contained inside
                   the else-part as well */
                printbblmsg("childblock initialized to", childblock);
                for (; bl != NULL; bl = bl->prev) {
                  printf("i = (%d), bl = (%p)\n", i, (void *)bl);
                  lastbl = bl;
                  if (bl->endpc < branchendpc) {
                    BasicBlock *currblock;
                    addbbl1(fs, bl->state.startpc, bl->endpc, BBL_DO);
                    currblock = fs->a->bbllist.first;
                    lua_assert(currblock != NULL);
                    printbblmsg("found an inner block", currblock);
                    currblock->firstchild = childblock;
                    childblock = currblock;
                    bl->pass = 1; /* need to pass branch data to parent */
                  }
                  else
                    break;
                  i++;
                }
                lua_assert(lastbl != NULL);
                lastbl->pass = 0;
                if (childblock != NULL)
                  nextsibling = childblock;
                printbblmsg("nextsibling set to", nextsibling);
                if (lastbl->endpc > branchendpc)
                  parentblock = lastbl;
                else
                  parentblock = NULL;
              }
              else {
                noblock = 0;
                parentblock = block;
              }
              branchstartpc = pc+1;
              init_ins_property(fs, pc, INS_BLOCKEND);
              /* BRANCHENDPC will be less than BRANCHSTARTPC if it is an empty
                 block (target-1 == pc) */
              if (branchstartpc <= branchendpc) {
                init_ins_property(fs, branchstartpc, INS_BRANCHBEGIN);
                set_ins_property(fs, branchendpc, INS_BLOCKEND);
              }
              else { /* empty block */
                init_ins_property(fs, branchstartpc, INS_EMPTYBLOCK);
              }
              new_branch.prev = branch;
              new_branch.outer = &outer; /* keep the same loop context */
              if (branch != NULL)
                new_branch.parentblock = branch->parentblock;
              else
                new_branch.parentblock = parentblock;
              if (block != NULL)
                /* update the block's possible state to its actual state, as the
                   possible state may be what gets updated within the branch
                   context, so it needs to be initialized */
                block->possiblestate = block->state;
              newbranch1(fs, &new_branch, branchstartpc, branchendpc, target,
                         &nextsibling);
              bbl1(ca, fs, startpc, type, &new_branch, NULL, nextsibling);
              {
                BasicBlock *new_block;
                BasicBlock *ifblock = new_branch.ifblock;
                BasicBlock *elseblock = new_branch.elseblock;
                /*lua_assert(ifblock != NULL);*/
                lua_assert(elseblock != NULL);
                lua_assert(elseblock->type == BBL_ELSE);
                lua_assert(new_branch.midpc == elseblock->startpc);
                if (ifblock != NULL) {
                  lua_assert(ifblock->type == BBL_IF);
                  if (ifblock->endpc != new_branch.midpc-1 ||
                      ifblock->nextsibling != elseblock) {
                    /* erroneous else-branch */
                    goto badelsebranch;
                  }
                  else
                    new_block = nextsibling = ifblock;
                }
                else {
                  BasicBlock *elseprevsibling;
                  badelsebranch:
                  printbblmsg("handling erroneous else-branch detection",
                              elseblock);
                  /* correct the else-block that has already been created */
                  /*elseblock->startpc = elseblock->endpc = pc;*/
                  elseblock->startpc = pc;
                  elseblock->isempty = (elseblock->endpc < elseblock->startpc);
                  /*elseblock->isempty = 0;*/
                  elseblock->type = BBL_REPEAT;
                  printbblmsg("else-block corrected to", elseblock);
                  printbblmsg("elseblock->nextsibling =", elseblock->nextsibling);
                  unset_ins_property(fs, pc, INS_BLOCKEND);
                  if (branchstartpc <= branchendpc) {
                    unset_ins_property(fs, branchstartpc, INS_BRANCHBEGIN);
                    unset_ins_property(fs, branchendpc, INS_BLOCKEND);
                  }
                  else { /* empty block */
                    unset_ins_property(fs, branchstartpc, INS_EMPTYBLOCK);
                  }
                  set_ins_property(fs, pc, INS_REPEATSTAT);
                  set_ins_property(fs, elseblock->endpc, INS_LOOPEND);
                  set_ins_property(fs, pc, INS_BREAKSTAT);
                  /* ELSEBLOCK is not actually an else-block */
                  elseprevsibling = new_branch.elseprevsibling;
                  printbblmsg("elseprevsibling =", elseprevsibling);
                  if (elseprevsibling != NULL) {
                    BasicBlock *dummynextsibling = elseblock;
                    /* make sure the sibling relation exists */
                    elseprevsibling->nextsibling = elseblock;
                    if (elseprevsibling->type == BBL_ELSE &&
                        elseprevsibling->endpc == pc) {
                      printf("changing elseprevsibling->endpc from (%d) to "
                             "(%d)\n", pc+1, branchendpc+1);
                      elseprevsibling->endpc = branchendpc;
                    }
                    fixsiblingchain1(elseprevsibling, &dummynextsibling);
                    /*lua_assert(new_branch.elseprevsibling->nextsibling ==
                               elseblock);*/
                    /* the analyzer thought ELSEBLOCK was an else-block that
                       started after the jump at PC, but really it starts ON the
                       jump at PC, and said jump is a break (maybe?), so the
                       previous sibling of ELSEBLOCK can't contain the jump, as
                       it should be inside to ELSEBLOCK */
                    /*lua_assert(elseprevsibling->endpc+1 < elseblock->startpc);*/
                  }
                  new_block = elseblock;
                  /* firstblock should never be NULL as it gets set to the value
                     of NEXTSIBLING when returning, and NEXTSIBLING is
                     intialized to the else-part (ELSEBLOCK when recursing into
                     a branch context) */
                  lua_assert(new_branch.firstblock != NULL);
                  nextsibling = new_branch.firstblock;
                  printbblmsg("new_branch.firstblock =", new_branch.firstblock);
                  /* This is here to handle back-to-back `repeat-break'
                     statements and turn them into child and parent instead of
                     siblings, since the outer one's endpc will be after the
                     inner one's startpc. If NEXTSIBLING is not NEW_BLOCK, then
                     this is not the innermost context, and the sibling chain
                     can be fixed up. In this case, NEXTSIBLING is actually the
                     previous sibling of NEW_BLOCK (confusing, I know) as the
                     order of recursive calls for this type of else-branch is
                     from innermost-to-outermost, and the order in which the
                     calls return is outermost-to-innermost, so the `firstblock'
                     field in each call will be set to the block's enclosing
                     context, because that is what has returned before returning
                     from the current context */
                  if (nextsibling != new_block &&
                      nextsibling->nextsibling == new_block) {
                    /* NEXTSIBLING is the outer block that needs to be made the
                       parent. NEW_BLOCK is the inner block that is currently
                       the next sibling, but needs to be made the child. Use a
                       dummy variable so `fixsiblingchain' doesn't change these
                       variables */
                    BasicBlock *dummynextsibling = new_block;
                    fixsiblingchain1(nextsibling, &dummynextsibling);
                  }
                  if (new_branch.contextswitched) {
                    printf("handling else-branch context-switch for pc (%d)\n",
                           ca->pc+1);
                    pc = ca->pc;
                    branchendpc = new_branch.endpc;
                    target = pc + 1 + GETARG_sBx(code[pc]);
                    printf("update variables before recursing with new context:\n"
                           "  pc = (%d)\n"
                           "  branchendpc = (%d)\n"
                           "  target = (%d)\n", pc+1, branchendpc+1, target+1);
                    printbblmsg("  nextsibling =",nextsibling);
                    nextbranch = NULL;
                    nextbranchtarget = -1;
                    if (block != NULL) {
                      block->state = block->possiblestate;
                    }
                    goto elsebranch;
                  }
                }
                printbblmsg("exited block", new_block);
                lua_assert(new_block != NULL);
                if (new_block == ifblock) {
                  nextbranch = ifblock;
                  printbblmsg("setting nextbranch to", ifblock);
                  nextbranchtarget = new_branch.target1;
                }
                else {
                  nextbranch = NULL;
                  nextbranchtarget = -1;
                  if (block != NULL) {
                    block->state = block->possiblestate;
                  }
                }
                if (noblock) { /* close the erroneous block and pass data */
                  lua_assert(block != NULL);
                  block->nextbranch = nextbranch;
                  return; /* return from the do-block context */
                }
              }
            }
          }
        }
        break;
      }
      case OP_TFORLOOP: /* handled in case OP_JMP */
        lua_assert(test_ins_property(fs, pc+1, INS_LOOPEND));
        break;
      case OP_FORLOOP: { /* a for-num loop */
        int target = pc + 1 + sbx; /* start of for-loop */
        init_ins_property(fs, target, INS_FORNUM);
        init_ins_property(fs, pc, INS_LOOPEND);
        loop1(ca, fs, target, BBL_FORNUM);
        break;
      }
      case OP_FORPREP: { /* mark the start of preparation code */
        int target = pc + 1 + sbx; /* end of for-loop */
        test_ins_property(fs, target, INS_LOOPEND);
        fornumprep1(ca, fs, target);
        goto poststat;
      }
      case OP_CLOSE: /* check for a block-ending if the next op is not a jump */
        if (GET_OPCODE(code[pc+1]) != OP_JMP) {
          /* when multiple, possibly nested OP_CLOSE are encountered, it is
             important to compare the argA of each. A nested block would not
             close up to register <= to that of its enclosing block. So, if
             there is a previous OP_CLOSE context, check its register. If it is
             greater than or equal to this A, it cannot logically be nested. */
          if (block != NULL && block->reg >= a) {
            /* this block is adjacent to the previous, switch out the context
               instead of pushing a new one */
            BasicBlock *new_block;
            printf("encountered adjacent do-block\n");
            addbbl1(fs, block->state.startpc, block->endpc, BBL_DO);
            new_block = fs->a->bbllist.first;
            lua_assert(new_block != NULL);
            new_block->firstchild = block->state.firstchild;
            printbblmsg("setting first child for block to",
                        block->state.firstchild);
            new_block->nextsibling = block->nextsibling;
            printbblmsg("setting next sibling for block to",block->nextsibling);
            /* link the last sibling before the adjacent block to it */
            if (block->state.prevsibling) {
              block->state.prevsibling->nextsibling = new_block;
              printbblmsg("connecting previous sibling",
                          block->state.prevsibling);
            }
            else
              nextsibling = new_block;
            /* re-initialize the context and continue without recursing */
            block->state.prevsibling = NULL;
            block->nextsibling = nextsibling;
            block->firstsibling = NULL;
            block->state.firstchild = NULL;
            block->nextbranch = NULL;
            block->pass = 0;
            block->state.startpc = pc;
            block->endpc = pc;
            block->reg = a;
            nextsibling = NULL; /* set to NULL as if recursing */
          }
          else {
            struct block1 new_block;
            new_block.prev = block;
            new_block.nextbranch = NULL;
            new_block.nextsibling = nextsibling;
            new_block.firstsibling = NULL;
            new_block.state.prevsibling = NULL;
            new_block.state.firstchild = NULL;
            new_block.pass = 0;
            new_block.outer = &outer; /* keep the same loop context */
            new_block.state.startpc = pc;
            new_block.endpc = pc;
            new_block.reg = a;
            bbl1(ca, fs, startpc, type, NULL, &new_block, nextsibling);
            printbblmsg("new_block.prevsibling =", new_block.state.prevsibling);
            /* check if the block was inside an else-branch */
            if (new_block.nextbranch != NULL) {
              nextbranch = new_block.nextbranch;
              nextsibling = nextbranch;
              /* check if the parent was also inside the else-branch */
              if (new_block.pass) { /* also pass this data to the parent */
                lua_assert(block != NULL);
                block->nextbranch = nextbranch;
                return; /* return for each block that was in the else-branch */
              }
            }
            else { /* create a new basic block entry */
              BasicBlock *doblock;
              lua_assert(new_block.pass == 0);
              addbbl1(fs, new_block.state.startpc, new_block.endpc, BBL_DO);
              doblock = fs->a->bbllist.first;
              lua_assert(doblock != NULL);
              doblock->firstchild = new_block.state.firstchild;
              doblock->nextsibling = new_block.nextsibling;
              if (new_block.state.prevsibling) {
                printbblmsg("connecting previous sibling",
                            new_block.state.prevsibling);
                new_block.state.prevsibling->nextsibling = doblock;
                /* there were blocks encountered that come before this block */
                nextsibling = new_block.firstsibling;
              }
              else {
                /* this is the actual earliest block so far */
                nextsibling = doblock;
              }
              if (block != NULL)
                /* update child for the parent */
                block->state.firstchild = doblock;
              if (branch != NULL && branch->elseprevsibling == NULL)
                branch->elseprevsibling = doblock;
            }
          }
        }
        break;
      CASE_OP_CALL: /* a function call expression */
        callstat1(ca, fs);
        goto poststat;
      case OP_CONCAT: /* a concat expression */
        concat1(ca, fs);
        goto poststat;
      case OP_RETURN: /* a return expression */
        retstat1(ca, fs);
        goto poststat;
      case OP_TESTSET: /* a testset expression */
        if (ca->testset.reg == -1)
          ca->testset.reg = b;
        else {
          if (ca->testset.reg != b) {
            /* bad code */
          }
        }
        fs->firstclob = pc;
        break;
      poststat: /* update variables after calls which change the pc */
        pc = ca->pc;
        i = code[pc];
        o = GET_OPCODE(i);
        a = GETARG_A(i);
        b = GETARG_B(i);
        c = GETARG_C(i);
      default:
        if (beginseval(o, a, b, c, 0)) {
          int stateunsure;
          struct block1 *bl;
          struct blockstate1 *blstate;
          fs->firstclob = pc; /* update first unstruction that clobbers A */
          if (branch != NULL && branch->parentblock != NULL) {
            bl = branch->parentblock;
            blstate = &bl->possiblestate;
            stateunsure = 1;
            goto updateblockstate1;
          }
          else if (block != NULL) { /* update for enclosing block contexts */
            int i;
            bl = block;
            blstate = &bl->state;
            stateunsure = 0;
            updateblockstate1:
            i = 0;
            if (bl->reg == a) {
              /* this block is now caught up to the current pc */
              blstate->startpc = pc; /* update start */
              blstate->firstchild = nextsibling; /* update first child */
              /* discharge saved previous sibling */
              blstate->prevsibling = NULL;
            }
            /* update the rest of the parents but only update startpc */
            for (bl = bl->prev; bl != NULL; bl = bl->prev) {
              blstate = stateunsure ? &bl->possiblestate : &bl->state;
              if (bl->reg == a) {
                blstate->startpc = pc;
                printf("%supdating .firstclob for (%d) to (%d), %d block "
                       "removed\n", stateunsure ? "(possibly) " : "",
                       bl->reg, pc+1, i);
              }
              i++;
            }
          }
        }
        break;
    }
    if (ca->pc == startpc)
      break;
  }
  ca->testset.endpc = ca->testset.reg = -1;
  if (branch == NULL && block == NULL) {
    ca->curr = outer; /* pop old values */
  }
  else {
    if (branch != NULL) {
      printf("forced to return from branch without finding an if-part\n");
      branch->firstblock = nextsibling;
      /*lua_assert(branch->next != NULL && branch->next->type == BBL_IF)*/;
    }
    else {
      printbblmsg("setting block->firstsibling to", nextsibling);
      block->firstsibling = nextsibling;
    }
    return;
  }
  addbbl1(fs, startpc, endpc, type); /* create a new basic block node */
  /* `nextsibling' will end up being the first child of this new block */
  fs->a->bbllist.first->firstchild = nextsibling;
  printf("ca->pc = (%d)\n", ca->pc);
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
  ca.curr.start = ca.curr.end = ca.curr.type = -1;
  ca.pc = f->sizecode - 1; /* will be decremented again to skip final return */
  ca.prevTMode = 0;
  ca.testset.endpc = ca.testset.reg = -1;
  ca.code = f->code;
  UNUSED(D);
  bbl1(&ca, fs, 0, BBL_FUNCTION, NULL, NULL, NULL);
  /*init_ins_property(fs, f->sizecode - 1, INS_BLOCKEND);*/ /* mark end of function */
  {
    BasicBlock *first = fs->a->bbllist.first;
    lua_assert(first != NULL);
    lua_assert(first->nextsibling == NULL);
    lua_assert(first->type == BBL_FUNCTION);
  }
  printf("ca.pc == (%d)\n", ca.pc);
  lua_assert(ca.pc <= 0);
  lua_assert(ca.testset.endpc == -1 && ca.testset.reg == -1);
}


#define varstartshere2(fs,pc) (*((fs)->D->varstarts))((fs),(pc))

static int varstarts_nodebug(DFuncState *fs, int pc) {
  UNUSED(fs); UNUSED(pc);
  return 0;
}

static int varstarts_withdebug(DFuncState *fs, int pc) {
  struct LocVar *var;
  int idx = fs->locvaridx;
  int n = 0;
  while (idx < fs->sizelocvars && (var = &fs->locvars[idx])->startpc == pc) {
    idx = ++fs->locvaridx;
    n++;
    printf("variable '%s' begins at (%d)\n",getstr(var->varname), pc+1);
  }
  return n;
}

typedef struct StackAnalyzer {
  int firstfree;
  int pc;
  int sizecode;
  int maxstacksize;
  const Instruction *code;
} StackAnalyzer;

#ifdef LUA_DEBUG
static void assertbblvalid(DFuncState *fs, BasicBlock *bbl)
{
  int startpc = bbl->startpc;
  int endpc = bbl->endpc;
  int type = bbl->type;
  lua_assert(startpc <= endpc || bbl->isempty);
  lua_assert(type >= 0 && type < MAX_BBLTYPE);
  if (type < BBL_DO && type != BBL_FUNCTION) {
    lua_assert(test_ins_property(fs, endpc, INS_LOOPEND));
    if (type == BBL_WHILE)
      lua_assert(test_ins_property(fs, startpc, INS_WHILESTAT));
    else if (type == BBL_REPEAT)
      lua_assert(test_ins_property(fs, startpc, INS_REPEATSTAT));
    else if (type == BBL_FORNUM)
      lua_assert(test_ins_property(fs, startpc, INS_FORNUM));
    else if (type == BBL_FORLIST)
      lua_assert(test_ins_property(fs, startpc, INS_FORLIST));
  }
}
#else /* !LUA_DEBUG */
#define assertbblvalid(fs,bbl) ((void)(fs), (void)(bbl))
#endif /* LUA_DEBUG */

/*
** Second pass `basic block' handler - the function's registers and stack are
** analyzed and local variables are detected. With information about the local
** variables, any undetected do-end blocks can be detected in this pass.
*/
static void bbl2(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bbl)
{
  const Instruction *code = sa->code;
  BasicBlock *nextchild = bbl->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  int startpc = bbl->startpc;
  int endpc = bbl->endpc;
  int type = bbl->type;
  lua_assert(sa->pc == startpc);
  if (type == BBL_FUNCTION) {
    lua_assert(sa->pc == 0);
    lua_assert(endpc == sa->sizecode-1);
  }
  assertbblvalid(fs,bbl);
  printf("pass2: entered %sblock %s (%d-%d) at pc (%d)\n", 
         bbl->isempty ? "empty " : "", bbltypename(type),
         startpc+1, endpc+1, sa->pc+1);
  if (bbl->isempty) goto block2finished;
  for (; sa->pc < sa->sizecode; sa->pc++) {
    int pc, a, b, c;
    Instruction i;
    OpCode o;
    if (sa->pc == nextchildstartpc) {
      int ischildempty;
      lua_assert(nextchild != NULL);
      ischildempty = nextchild->isempty;
      bbl2(sa, fs, nextchild);
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      if (!ischildempty) {
        /* multiple blocks can end on the same instruction, so make sure not to
           process the last instruction more than once */
        if (sa->pc == endpc)
          break;
        /* go to the next instruction */
        continue;
      }
      /* process the same instruction, as it was not processed in the child
         call due to the child being empty */
    }
    pc = sa->pc;
    i = code[pc];
    o = GET_OPCODE(i);
    a = GETARG_A(i);
    b = GETARG_B(i);
    c = GETARG_C(i);
    {
      const char *opname;
      char spaces[18];
      int numspaces;
      memset(spaces, ' ', sizeof(spaces)-1);
      spaces[sizeof(spaces)-1] = '\0';
      opname = getOpName(o);
      numspaces = sizeof(spaces)-1-strlen(opname);
      if (numspaces <= 0) numspaces = 1;
      lua_assert(cast(size_t, numspaces) < sizeof(spaces));
      spaces[numspaces] = '\0';
      DumpStringf(fs->D, "pc = (%d), OP_%s%s%s\n", pc+1, opname, spaces,
                  bbltypename(type));
    }
    if (pc == endpc)
      break;
    (void)a; (void)b; (void)c;
  }
  block2finished:
  printf("pass2: leaving %sblock %s (%d-%d) at pc (%d)\n", bbl->isempty ? 
         "empty " : "", bbltypename(type),
         startpc+1,endpc+1,sa->pc+1);
}

static void pass2(const Proto *f, DFuncState *fs, DecompState *D)
{
  BasicBlock *func = fs->a->bbllist.first;
  StackAnalyzer sa;
  sa.firstfree = 0;
  sa.pc = 0;
  sa.code = f->code;
  sa.sizecode = f->sizecode;
  sa.maxstacksize = f->maxstacksize;
  lua_assert(func != NULL);
  UNUSED(D);
  bbl2(&sa, fs, func);
}

static void DecompileFunction(DecompState *D, const Proto *f);

static void DecompileFunction(DecompState *D, const Proto *f)
{
  DFuncState new_fs;
  open_func(&new_fs, D, f);
  if (f->name && getstr(f->name))
    printf("-- Decompiling function (%d) named '%s'\n", D->funcidx, getstr(f->name));
  else
    printf("-- Decompiling anonymous function (%d)\n", D->funcidx);
  pass1(f,&new_fs,D);
  (void)dumploopinfo;
  printf("new_fs.firstclob = (%d)", new_fs.firstclob+1);
  if (new_fs.firstclob != -1)
    printf(", a = (%d)",GETARG_A(f->code[new_fs.firstclob]));
  printf("\n");
  debugbblsummary(&new_fs);
#ifdef HKSC_DECOMP_DEBUG_PASS1
  (void)pass2;
#else
  dumploopinfo(f, &new_fs, D);
  DumpLiteral("\n\n", D);
  pass2(f,&new_fs,D);
  /*DecompileCode(f,&new_fs,D);*/
#endif /* HSKC_DECOMP_DEBUG_PASS1 */
  close_func(D);
}

/*
** Execute a protected decompiler.
*/
struct SDecompiler {  /* data to `f_decompiler' */
  DecompState *D;  /* decompiler state */
  const Proto *f;  /* compiled chunk */
};

static void f_decompiler (hksc_State *H, void *ud) {
  struct SDecompiler *sd = (struct SDecompiler *)ud;
  DecompileFunction(sd->D, sd->f);
  UNUSED(H);
}

/*
** dump Lua function as decompiled chunk
*/
int luaU_decompile (hksc_State *H, const Proto *f, lua_Writer w,
                            void *data)
{
  DecompState D;
  int status;
  struct SDecompiler sd;
  D.H=H;
  D.fs=NULL;
  D.writer=w;
  D.data=data;
  D.status=0;
  D.indentlevel=0;
  D.funcidx=0;
  D.needspace=0;
  D.usedebuginfo = (!Settings(H).ignore_debug && f->sizelineinfo > 0);
  D.matchlineinfo = (Settings(H).match_line_info && D.usedebuginfo);
  if (D.usedebuginfo)
    D.varstarts = varstarts_withdebug;
  else
    D.varstarts = varstarts_nodebug;
  sd.D=&D;
  sd.f=f;
  status = luaD_pcall(H, f_decompiler, &sd);
  if (status) D.status = status;
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
  (void)regflagnames;
  (void)maybe_begin_line;
  (void)implicit_decl;
  (void)k2exp;
  (void)previsjump;
  return D.status;
}

#endif /* HKSC_DECOMPILER */
