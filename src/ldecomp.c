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

#include "lanalyzer.h"
#include "ldebug.h"
#include "ldo.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"


#define opLoadsK(o) ((o) == OP_LOADK || (o) == OP_LOADBOOL || (o) == OP_LOADNIL)


#define DEFBBLTYPE(e)  #e,
static const char *const bbltypenames [] = {
  BBLTYPE_TABLE
  "BBL_MAX"
};
#undef DEFBBLTYPE

#define DEFINSFLAG(e)  "INS_" #e,
static const char *const insflagnames [] = {
  INSFLAG_TABLE
  "INS_MAX"
};
#undef DEFINSFLAG

#define DEFREGFLAG(e)  "REG_" #e,
static const char *const regflagnames [] = {
  REGFLAG_TABLE
  "REG_MAX"
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


#ifdef LUA_DEBUG

static void debugbbl(Analyzer *a, BasicBlock *bbl, int indent) {
  BasicBlock *child = bbl->firstchild;
  BasicBlock *nextsibling = bbl->nextsibling;
  int i;
  for (i = 0; i < indent; i++)
    printf("  ");
  if (indent) printf("- ");
  printf("(%d-%d) %s (%s sibling)\n", bbl->startpc+1, bbl->endpc+1,
         bbltypename(bbl->type), (nextsibling != NULL)?"YES":"NO");
  indent++;
  while (child != NULL) {
    debugbbl(a, child, indent);
    child = child->nextsibling;
  }
}

static void debugbblsummary(DFuncState *fs)
{
  Analyzer *a = fs->a;
  fputs("BASIC BLOCK SUMMARY\n"
        "-------------------\n", stdout);
  debugbbl(a, a->bbllist.first, 0);
  fputs("-------------------\n", stdout);
}
#endif /* LUA_DEBUG */


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
  for (pc = 0; pc < sizecode - 1; pc++) {
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


/* structure for handling nested do-blocks with OP_CLOSE */
struct doblock {
  int reg; /* the register to close up to in OP_CLOSE */
  int firstclob; /* pc of first possible instruction which clobbers REG */
  int endbeforecall; /* true if the analyzer encountered another OP_CLOSE while
  in a block and concluded the block was not nested, and therefore returned
  before calling `bbl1' for the new OP_CLOSE, and now has to go backward and
  push this OP_CLOSE context */
  struct doblock *prev; /* enclosing do-block */
};


/*
** `Code Analyzer' - the main structure used in the first pass. It populates the
** `insproperties' array of its corresponding `DFuncState'.
*/
typedef struct CodeAnalyzer {
  /* a record of all encountered loops is saved implicitly on the callstack;
     these values hold the bounds of the current loop */
  struct {
    int start, end, type;
  } curr;  /* the start and end of the current basic block */
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
  bbl1(ca, fs, startpc, type, NULL, NULL); /* create the new block */ \
  new = fs->a->bbllist.first; \
  lua_assert(new != nextsibling); \
  addsibling1(new, nextsibling); \
  nextsibling = new; \
} while(0)


/*
** `branch1' holds the context for an if-else branch construct in the first pass
*/
struct branch1 {
  struct branch1 *prev;  /* previous branch context */
  BasicBlock *next;  /* the next branch block in this group */
  int target1;  /* jump target out of the branch condition */
  int startpc, endpc, midpc;  /* midpc is the start of the else-part */
  int optimalexit;  /* target of optimized jumps out of this branch */
  /* saved values for the outer loop statement */
  int outerstatstart, outerstatend, outerstattype;
};


/*
** `block1' holds the context for a do-end block
*/
struct block1 {
  struct block1 *prev;  /* previous block context */
  BasicBlock *nextbranch;
  BasicBlock *nextsibling;
  BasicBlock *firstchild;
  int pass;
  int reg;  /* register to close up to in OP_CLOSE */
  int firstclob;  /* first pc to clobber REG */
  int seenclosure;
  int startpc, endpc;
  /* saved values for the outer loop statement */
  int outerstatstart, outerstatend, outerstattype;
};


static BasicBlock *fixsiblingchain1(BasicBlock *block, BasicBlock **chain) {
  int prevtype = -1;
  int endpc = block->endpc;
  BasicBlock *lastchild = NULL;
  BasicBlock *nextsibling = *chain;
  BasicBlock *firstsibling = nextsibling;
  lua_assert(chain != NULL);
  printf("fixing up sibling chain\n");
  printf("--\n");
  while (nextsibling && nextsibling->startpc <= endpc) {
    lastchild = nextsibling;
    printf("found child (%s) (%d-%d)\n", bbltypename(nextsibling->type),
           nextsibling->startpc+1, nextsibling->endpc+1);
    nextsibling = nextsibling->nextsibling;
    if (prevtype != BBL_ELSEIF && lastchild->type == BBL_ELSEIF)
      lastchild->type = BBL_IF;
    prevtype = lastchild->type;
  }
  printf("--\n");
  *chain = nextsibling;
  if (lastchild) {
    block->firstchild = firstsibling;
    /*if (firstsibling && firstsibling->type == BBL_ELSEIF)
      firstsibling->type = BBL_IF;*/
    if (firstsibling) {
      printf("first child is (%s) (%d-%d)\n", bbltypename(firstsibling->type),
             firstsibling->startpc+1, firstsibling->endpc+1);
    }
    /* fix sibling chain for last child */
    lastchild->nextsibling = NULL;
    printf("last child is (%s) (%d-%d)\n", bbltypename(lastchild->type),
           lastchild->startpc+1, lastchild->endpc+1);
  }
  else {
    printf("found no children for this block\n");
  }
  if (nextsibling) {
    printf("next sibling changed to (%s) (%d-%d)\n",
           bbltypename(nextsibling->type), nextsibling->startpc+1,
           nextsibling->endpc+1);
  }
  else
    printf("next sibling changed to (NULL)\n");
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
  lua_assert(block != NULL);
  printf("ELSE BLOCK - (%d-%d)\n", midpc+2, endpc+1);
  fixsiblingchain1(block, nextsibling);
  branch->next = block;
}



static void bbl1(CodeAnalyzer *ca, DFuncState *fs, int startpc, int type,
                 struct branch1 *branch, struct block1 *block)
{
  BasicBlock *nextsibling;
  BasicBlock *nextbranch = NULL;
  int nextbranchtarget = -1;
  int endpc;
  int outerstatstart, outerstatend; /* enclosing loop start and end pc */
  int outerstattype; /* type of enclosing loop */
  const Instruction *code = ca->code;
  if (branch == NULL && block == NULL) { /* inside a loop */
    nextsibling = NULL;
    endpc = ca->pc;
    /* save old values */
    outerstatstart = ca->curr.start;
    outerstatend = ca->curr.end;
    outerstattype = ca->curr.type;
    /* update current */
    ca->curr.start = startpc;
    ca->curr.end = endpc;
    ca->curr.type = type;
    printf("entering new basic block of type (%s)\n", bbltypename(type));
  }
  else { /* inside a branch construct */
    if (branch != NULL) {
      lua_assert(block == NULL);
      nextsibling = branch->next; /* the else-block */
      outerstatstart = branch->outerstatstart;
      outerstatend = branch->outerstatend;
      outerstattype = branch->outerstattype;
      printf("entering new branch\n");
    }
    else {
      nextsibling = NULL;
      outerstatstart = block->outerstatstart;
      outerstatend = block->outerstatend;
      outerstattype = block->outerstattype;
      printf("entering new do-block\n");
    }
    endpc = ca->curr.end;  /* endpc must be the end of the current loop */
  }
  ca->pc--;
  lua_assert(type < BBL1_MAX); /* first pass only handles loop types */
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
        int target = pc + 1 + sbx; /* the jump target pc */
        int branchstartpc, branchendpc;
        int tailbranchjump=0;
        OpCode prevop; /* previous opcode to see if it is a test instruction */
        if (test_ins_property(fs, pc+1, INS_FORLIST)) {
          forlistprep1(ca, fs, target);
          break;
        }
        if (pc == 0) {
          ca->prevTMode = 0;
          goto jmpisfirst;
        }
        /* excluding the above cases, this cannot be the first instruction */
        lua_assert(pc > 0);
        prevop = GET_OPCODE(code[pc-1]);
        if (prevop == OP_TESTSET) {/* a jump after OP_TESTSET is not a branch */
          if (ca->testset.endpc == -1) {
            ca->testset.endpc = target;
            init_ins_property(fs, target-1, INS_TESTSETEND);
            if (test_ins_property(fs, target-1, INS_BLOCKEND)) {
              int testsetpc;
              unset_ins_property(fs, target-1, INS_BLOCKEND);
              lua_assert(nextsibling == NULL ||
                         nextsibling == fs->a->bbllist.first);
              printf("nextbranch = (%p), (%s) (%d-%d)\n", (void *)nextbranch,
                     bbltypename(nextbranch->type), nextbranch->startpc+1,
                     nextbranch->endpc+1);
              printf("cleaning up testset expression from (%d-%d)\n",
                     pc+1, target);
              printf("--\n");
              for (testsetpc = pc; testsetpc < target; testsetpc++) {
                while (nextsibling && nextsibling->startpc == testsetpc) {
                  BasicBlock *nextnextsibling = nextsibling->nextsibling;
                  fs->a->bbllist.first = nextsibling->next;
                  if (nextsibling == fs->a->bbllist.last)
                    fs->a->bbllist.last = fs->a->bbllist.first;
                  printf("deleting erroneous block (%s) (%d-%d)\n",
                         bbltypename(nextsibling->type),nextsibling->startpc+1,
                         nextsibling->endpc+1);
                  luaM_free(fs->H, nextsibling);
                  nextsibling = nextnextsibling;
                }
                unset_ins_property(fs, testsetpc, INS_BRANCHFAIL);
                unset_ins_property(fs, testsetpc+1, INS_BRANCHBEGIN);
                unset_ins_property(fs, testsetpc, INS_BRANCHPASS);
                unset_ins_property(fs, testsetpc, INS_BLOCKEND);
              }
              printf("--\n");
              if (nextsibling && nextsibling->type == BBL_IF)
                nextbranch = nextsibling;
              else
                nextbranch = NULL;
              printf("nextbranch = (%p)", (void *)nextbranch);
              if (nextbranch)
                printf("(%s) (%d-%d)\n",
                     bbltypename(nextbranch->type), nextbranch->startpc+1,
                     nextbranch->endpc+1);
              printf("\n");
            }
          }
          else {
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
                 test or a failed inner while-loop condition */
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
                else {
                  /* a repeat-loop jumps to somewhere inside the current loop */
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
                  lua_assert(test_ins_property(fs, target, INS_REPEATSTAT));
                  goto markrepeatstat;
                }
              }
              else { /* a repeat-loop */
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
              int pc1=pc-2;
              const OpCode o1[2] = {OP_JMP, OP_CLOSE};
              int i1=0;
              while (pc1 >= 0) {
                if (GET_OPCODE(code[pc1]) == o1[i1%2]) {
                  i1++;
                  if (i1 == 3) {
                  /* At this point there is:
                      OP_JMP    <-- pc1
                      OP_CLOSE
                      OP_JMP
                      OP_CLOSE  <-- prevop
                      OP_JMP    <-- o
                     This can either be a while-loop with multiple break
                     statements at the end or a repeat-loop, both containing
                     local variables that are used as upvaleus. In the case of a
                     repeat-loop, the instruction that precedes the OP_JMP at
                     `pc1' must be a test instruction. */
                    pc1--;
                    if (pc1 >= 0 && testTMode(GET_OPCODE(code[pc1])))
                      goto markrepeatstat; /* repeat-loop that has upvalues */
                    break;
                  }
                }
                else
                  break;
                pc1--;
              }
              /* fallthrough */
            }
            /* inside a while-loop, jumping unconditionally back to the start
               is an optimized jump out of a branch-block which would otherwise
               jump to the very end of the loop */
            if (type == BBL_WHILE && target == startpc) {
              if (pc == endpc-1)
                goto markwhilestat;
              /* this instruction is the end of a basic block */
              branchendpc = endpc-1;
              goto elsebranch;
            }
            /* inside a loop enclosed by a while-loop, jumping back to the start
               of the while-loop, i.e. an optimized `break' out of the for-loop
               */
            else if (type != BBL_FUNCTION && outerstattype == BBL_WHILE &&
                     target == outerstatstart) {
              init_ins_property(fs, pc, INS_BREAKSTAT);
            }
            else { /* a while-loop */
              markwhilestat:
              encountered1("while", target);
              set_ins_property(fs, target, INS_WHILESTAT);
              init_ins_property(fs, pc, INS_LOOPEND);
              if (test_ins_property(fs, pc+1, INS_BLOCKEND)) {
                Instruction nexti = code[pc+1];
                int nexttarget;
                /* only jumps can be marked as block endings */
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
          if (ca->prevTMode) { /* conditional forward jump */
            /* check for a fail-jump out of a repeat-loop condition */
            if (type == BBL_REPEAT && target-1 == endpc) {
              markrepeatfail:
              init_ins_property(fs, pc, INS_LOOPFAIL);
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
                     ((target-1 == outerstatend &&
                       test_ins_property(fs, endpc+1, INS_BREAKSTAT)) ||
                      /* the typical case, a jump past the end of the loop */
                      (target-1 == endpc) ||
                      /* a specially marked target for optimized fail-jumps */
                      (test_ins_property(fs, target, INS_OPTLOOPFAILTARGET)))) {
              lua_assert(outerstatend != -1);
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else { /* jumping inside a branch-condition */
              if (test_ins_property(fs, target-1, INS_BRANCHFAIL))
                init_ins_property(fs, pc, INS_BRANCHPASS);
              else {
                init_ins_property(fs, pc, INS_BRANCHFAIL);
                if (!test_ins_property(fs, target-1, INS_BRANCHPASS)) {
                  /* create a basic block for this branch */
                  /* true if this branch has no else-block; this is significant
                     because only branches with an else-block cause a recursive
                     call, whereas else-less branches are always child blocks in
                     the current branch context */
                  int issingle;
                  BasicBlock *new_block;
                  ifbranch:
                  branchstartpc = pc+1;
                  if (branch != NULL)
                    branch->startpc = branchstartpc;
                  if (branch != NULL &&
                      (target == branch->midpc || tailbranchjump)) {
                    branchendpc = branch->midpc-1; /* the end of the if-part */
                    issingle = 0; /* there is an else-block */
                  }
                  else {
                    if (tailbranchjump && branch == NULL)
                      branchendpc = endpc-1;
                    else if (branch == NULL || target < branch->midpc)
                      branchendpc = target-1;
                    else
                      branchendpc = branch->midpc-1;
                    if (nextbranch != NULL) {
                      BasicBlock *next = nextbranch;
                      lua_assert(nextbranchtarget != -1);
                      if (next->endpc == branchendpc &&
                          nextbranchtarget == target) {
                        new_block = next;
                        printf("merging this branch with the next block (%s) "
                               "(%d-%d)\n", bbltypename(next->type),
                               next->startpc+1, next->endpc+1);
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
                  /* at this point the start and endpc of the block is known */
                  lua_assert(branchendpc >= branchstartpc);
                  lua_assert(issingle == 0 || issingle == 1);
                  if (issingle && branch != NULL) {
                    printf("THIS BRANCH IS A CHILD BLOCK of the top-level one\n");
                  }
                  addbbl1(fs, branchstartpc, branchendpc, BBL_IF);
                  new_block = fs->a->bbllist.first;
                  blockcreated:
                  init_ins_property(fs, branchstartpc, INS_BRANCHBEGIN);
                  set_ins_property(fs, branchendpc, INS_BLOCKEND);
                  lua_assert(new_block != NULL);
                  printf("BRANCH BLOCK - (IF) (%d-%d)\n", new_block->startpc+1,
                         new_block->endpc+1);
                  fixsiblingchain1(new_block, &nextsibling);
                  nextsibling = new_block;
                  if (branch) {
                    if (nextbranch == NULL) {
                      branch->next = new_block;
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
              int pc1 = pc-2;
              if (pc1 >= 0 && GET_OPCODE(code[pc1]) == OP_JMP) {
                pc1--;
                if (pc1 >= 0 && testTMode(GET_OPCODE(code[pc1])))
                  goto markrepeatfail; /* fail-jump out of repeat-loop */
              }
              /* fallthrough */
            }
            if (test_ins_property(fs, target-1, INS_LOOPEND) &&
                target-1 == endpc) { /* break statement */
              /* a while-loop can begin with a jump that is not a break if it
                 has a literal `false' condition */
              if (pc != startpc || type != BBL_WHILE)
                init_ins_property(fs, pc, INS_BREAKSTAT);
            }
            else { /* exiting a branch */
              struct branch1 new_branch;
              int noblock;
              if (branch != NULL &&
                  (target == branch->optimalexit || target == branch->endpc+1))
                branchendpc = branch->midpc-1;
              else
                branchendpc = target-1;
              elsebranch:
              if (block != NULL && block->endpc <= branchendpc) {
                struct block1 *bl = block;
                BasicBlock *childblock = NULL;
                /* the OP_CLOSE at the end of this branch is not for a block */
                noblock = 1;
                for (; bl != NULL; bl = bl->prev) {
                  if (bl->endpc < branchendpc) {
                    BasicBlock *currblock;
                    bl->startpc = bl->firstclob;
                    addbbl1(fs, bl->startpc, bl->endpc, BBL_DO);
                    currblock = fs->a->bbllist.first;
                    lua_assert(currblock != NULL);
                    currblock->firstchild = childblock;
                    childblock = currblock;
                    bl->pass = 1;
                  }
                }
                bl->pass = 0;
              }
              else
                noblock = 0;
              branchstartpc = pc+1;
              init_ins_property(fs, pc, INS_BLOCKEND);
              init_ins_property(fs, branchstartpc, INS_BRANCHBEGIN);
              set_ins_property(fs, branchendpc, INS_BLOCKEND);
              new_branch.prev = branch;
              /* keep the same loop context */
              new_branch.outerstatstart = outerstatstart;
              new_branch.outerstatend = outerstatend;
              new_branch.outerstattype = outerstattype;
              newbranch1(fs, &new_branch, branchstartpc, branchendpc, target,
                         &nextsibling);
              bbl1(ca, fs, startpc, type, &new_branch, NULL);
              {
                BasicBlock *first_block = new_branch.next;
                lua_assert(first_block != NULL);
                lua_assert(first_block->type == BBL_IF);
                printf("exited block (%s) (%d-%d)\n", bbltypename(first_block->type),
                       first_block->startpc+1, first_block->endpc+1);
                nextsibling = first_block;
                nextbranch = first_block;
                printf("setting nextbranch to (%p)\n", (void *)first_block);
                nextbranchtarget = new_branch.target1;
                if (noblock) {
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
            BasicBlock *new_block;
            addbbl1(fs, pc+1, block->endpc, BBL_DO);
            new_block = fs->a->bbllist.first;
            lua_assert(new_block != NULL);
            new_block->firstchild = nextsibling;
            new_block->nextsibling = block->nextsibling;
            block->nextsibling = new_block;
            nextsibling = NULL;
            block->nextbranch = NULL;
            /* overwrite the current context and continue as normal */
            block->pass = 1;
            block->startpc = -1;
            block->endpc = pc;
            block->reg = a;
            block->firstclob = pc;
            block->firstchild = NULL;
          }
          else {
            struct block1 new_block;
            new_block.prev = block;
            new_block.nextbranch = NULL;
            new_block.nextsibling = nextsibling;
            new_block.firstchild = NULL;
            new_block.pass = 0;
            new_block.outerstatstart = outerstatstart;
            new_block.outerstatend = outerstatend;
            new_block.outerstattype = outerstattype;
            new_block.startpc = -1;
            new_block.endpc = pc;
            new_block.reg = a;
            new_block.firstclob = pc;
            bbl1(ca, fs, startpc, type, NULL, &new_block);
            if (new_block.nextbranch != NULL) {
              nextbranch = new_block.nextbranch;
              nextsibling = nextbranch;
              if (new_block.pass) {
                lua_assert(block != NULL);
                block->nextbranch = nextbranch;
                return;
              }
            }
            else {
              BasicBlock *doblock;
              lua_assert(new_block.pass == 0);
              addbbl1(fs, new_block.startpc, new_block.endpc, BBL_DO);
              doblock = fs->a->bbllist.first;
              lua_assert(doblock != NULL);
              doblock->firstchild = new_block.firstchild;
              doblock->nextsibling = nextsibling;
              nextsibling = doblock;
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
        if (/*testAMode(o)*/ beginseval(o, a, b, c, 0)) {
          fs->firstclob = pc; /* update first unstruction that clobbers A */
          if (block != NULL) {
            struct block1 *bl = block;
            int i = 0;
            for (; bl != NULL; bl = bl->prev) {
              if (bl->reg == a) {
                bl->firstclob = pc;
                printf("updating .firstclob for (%d) to (%d), block %d removed\n",
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
    /* pop old values */
    ca->curr.start = outerstatstart;
    ca->curr.end = outerstatend;
    ca->curr.type = outerstattype;
  }
  else {
    if (branch != NULL)
      lua_assert(branch->next != NULL && branch->next->type == BBL_IF);
    else {
      lua_assert(block != NULL);
      block->firstchild = nextsibling;
      block->startpc = block->firstclob;
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
  bbl1(&ca, fs, 0, BBL_FUNCTION, NULL, NULL);
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
  struct {
    int start, end, type;
  } curr;
  int needendpc;
  int firstfree;
  int pc;
  int sizecode;
  int maxstacksize;
  const Instruction *code;
} StackAnalyzer;

/*
** Second pass `basic block' handler - it is aware of all loops in the current
** function. The rest of the blocks are identified and the function's registers
** and stack are analyzed and local variables are detected. With information
** about the local variables, do-end blocks can be detected in this pass.
*/
static void bbl2(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bl)
{
  const Instruction *code = sa->code;
  BasicBlock *nextchild = bl->firstchild;
  int nextchildstart = nextchild ? nextchild->startpc : -1;
  int startpc = bl->startpc;
  int endpc = bl->endpc;
  int type = bl->type;
  lua_assert(sa->pc == startpc);
#ifdef LUA_DEBUG
  if (type == BBL_FUNCTION) {
    lua_assert(sa->pc == 0);
    lua_assert(endpc == sa->sizecode-1);
  }
  else if (type < BBL_DO) {
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
#endif /* LUA_DEBUG */
  printf("pass2: entered block %s (%d-%d) at pc (%d)\n", bbltypename(type),
         startpc+1, endpc+1, sa->pc+1);
  for (; sa->pc < sa->sizecode; sa->pc++) {
    int pc = sa->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    DumpStringf(fs->D, "pc = (%d), OP_%s\n", pc+1, luaP_opnames[o]);
    if (pc == nextchildstart) {
      lua_assert(nextchild != NULL);
      bbl2(sa, fs, nextchild);
      nextchild = nextchild->nextsibling;
      nextchildstart = nextchild ? nextchild->startpc : -1;
      continue;
    }
    if (pc == endpc)
      break;
    (void)a; (void)b; (void)c;
  }
  printf("pass2: leaving block %s (%d-%d) at pc (%d)\n", bbltypename(type),
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
  dumploopinfo(f, &new_fs, D);
  printf("new_fs.firstclob = (%d)", new_fs.firstclob+1);
  if (new_fs.firstclob != -1)
    printf(", a = (%d)",GETARG_A(f->code[new_fs.firstclob]));
  printf("\n");
  DumpLiteral("\n\n", D);
  pass2(f,&new_fs,D);
  debugbblsummary(&new_fs);
  /*DecompileCode(f,&new_fs,D);*/
  /*discharge(&new_fs, D);*/ /* todo: should the chain always be empty by this point? */
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
int luaU_decompile (hksc_State *H, const Proto *f, lua_Writer w, void *data)
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
