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

#define isCall(o) ((o) == OP_CALL || (o) == OP_CALL_I || (o) == OP_CALL_C \
  (o) == OP_CALL_M || (o) == OP_TAILCALL || (o) == OP_TAILCALL_I || \
  (o) == OP_TAILCALL_C || (o) == OP_TAILCALL_M)

#define nodebug(H) (Settings(H).ignore_debug)

typedef struct {
  hksc_State *H;
  lua_Writer writer;
  void *data;
  int status;
  int line;
  /* data for the current function's decompilation */
  int indentlevel; /* the initial indentation level of this function */
  int funcidx; /* n for the nth function that is being decompiled */
} DecompState;

#define expliteral(e,l) ((e)->str = "" l, (e)->len = sizeof(l)-1, (e))
#define expts(e,ts) ((e)->str = getstr(ts), (e)->len = (ts)->tsv.len, (e))

#define detype(e) ((e)->flags)
#define dereg(e) ((e)->u.reg)
#define detok(e) ((e)->u.token)
#define delocidx(e) ((e)->u.locidx)

/* decompiler expression flags */
enum DEXPTYPE {
  DK = (1 << 0), /* an item from the constant table */
  DID = (1 << 1), /* an identifier */
  DTOKEN = (1 << 2), /* a reserved Lua token */
  DLHS = (1 << 3), /* left-hand-side of an assignment */
  DDECL = (1 << 4), /* local variable declaration, e.g. `local foo ...' */
  DRET = (1 << 5), /* expression is returned */
  DCOMMENT = (1 << 20), /* a Lua line comment */
  DLONGCOMMENT = (1 << 21) /* a Lua block comment */
};

#define deisk(e)              ((detype(e) & DK) != 0)
#define deisid(e)             ((detype(e) & DID) != 0)
#define deistoken(e)          ((detype(e) & DTOKEN) != 0)
#define deislhs(e)            ((detype(e) & DLHS) != 0)
#define deisdecl(e)           ((detype(e) & DDECL) != 0)
#define deisret(e)            ((detype(e) & DRET) != 0)
#define deiscomment(e)        ((detype(e) & DCOMMENT) != 0)
#define deislongcomment(e)    ((detype(e) & DLONGCOMMENT) != 0)

#define desettype(e,t) ((detype(e) |= (t)))
#define deunsettype(e,t) ((detype(e) &= ~(t)))
#define deresettype(e,t) ((detype(e) = (t)))

/* a decompiled expression */
typedef struct DExp {
  struct DExp *prev, *next;
  const char *str; /* the expressions's printable form */
  size_t len; /* length of str */
  int flags;
  int pc; /* pc at which this expression was encountered */
  int istemp; /* true if this expression is stored in a temp register */
  union {
    int locidx; /* local variable index for identifiers */
    int reg; /* the register that stores this expression */
    int token; /* the token id if this is a reserved token */
  } u;
} DExp;


/* a decompiled function */
typedef struct DFuncState {
  const Proto *f;
  struct {
    DExp *first, *last;
  } explist; /* current chain of pending expressions */
  int nexps; /* number of expressions in current chain */
  int idx; /* the nth function */
  hksc_State *H;
  /*lu_byte *reg2loc;*/
  struct LocVar *locvars;  /* information about local variables */
  int nlocvars; /* number of local variables declared so far */
  int ownlocvars; /* true if the locvars array was allocated for this struct */
  int sizelocvars;
  int pc; /* current pc */
} DFuncState;

static DExp *newexp(DFuncState *fs, int type) {
  DExp *e = luaM_new(fs->H, DExp);
  memset(e, 0, sizeof(DExp));
  desettype(e, type);
  e->pc = fs->pc;
  return e;
}

static void open_func(DFuncState *fs, const Proto *f,
                      DecompState *D) {
  fs->f = f;
  fs->explist.first = fs->explist.last = NULL;
  fs->nexps = 0;
  fs->H = D->H;
  fs->idx = D->funcidx++;
  fs->nlocvars = 0;
  if (!nodebug(fs->H) && f->sizelineinfo > 0) { /* have debug info */
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
}

static void close_func(DFuncState *fs, const Proto *f, DecompState *D) {
  UNUSED(f);
  lua_assert(fs->explist.first == NULL);
  lua_assert(fs->explist.last == NULL);
  lua_assert(fs->nexps == 0);
  D->funcidx--;
  if (fs->ownlocvars)
    luaM_freearray(fs->H, fs->locvars, fs->sizelocvars, struct LocVar);
  UNUSED(fs->locvars);
  UNUSED(fs->sizelocvars);
  UNUSED(fs->ownlocvars);
}

#define match_lines(D) \
  (Settings((D)->H).match_line_info && !Settings((D)->H).ignore_debug)

#define DumpLiteral(s,D) DumpBlock("" s, sizeof(s)-1, D)
#define DumpString(s,D) DumpBlock(s, strlen(s), D)
#define DumpExp(e,D) DumpBlock((e)->str, (e)->len, (D))

static void DumpBlock(const void *b, size_t size, DecompState *D)
{
  if (D->status==0)
  {
    lua_unlock(D->H);
    D->status=(*D->writer)(D->H,b,size,D->data);
    lua_lock(D->H);
  }
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

#define DumpAugVar(fs,n,D,s) DumpStringf((D), "f%d_" s "%d", (fs)->idx, (n))
#define DumpAugLocVar(fs,n,D) DumpAugVar(fs,n,D,"local")
#define DumpAugArgVar(fs,n,D) DumpAugVar(fs,n,D,"arg")

#define NextLine(D) AddLines(1, D)
static void AddLines(int n, DecompState *D)
{
  int i;
  const char lf = '\n';
  for (i=0; i<n; i++)
    DumpBlock(&lf, 1, D);
}


static void insert_exp(DFuncState *fs, DExp *pos, DExp *exp) {
  DExp *next = pos->next;
  next->prev = exp;
  exp->prev = pos;
  exp->next = next;
  fs->nexps++;
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

static void setlhs(DFuncState *fs, DExp *exp) {
  if (fs->nexps >= 2) {
    DExp *second = fs->explist.first->next;
    lua_assert(deislhs(second)); /* LHS cannot already be set */
  }
  desettype(exp, DLHS);
  prepend_exp(fs, exp);
}

/* prepend a local variable declaration as an LHS in an assignment */
static void adddecl(DFuncState *fs, DExp *exp) {
  DExp *decl;
  int r = dereg(exp);
  struct LocVar *var;
  int idx = fs->nlocvars++;
  lua_assert(r < fs->f->maxstacksize);
  lua_assert(idx < fs->sizelocvars);
  var = &fs->locvars[idx];
  decl = newexp(fs, DDECL | DLHS | DID);
  dereg(decl) = r;
  delocidx(decl) = idx;
  if (fs->ownlocvars) {
    var->startpc = fs->pc;
    /*var->endpc = fs->f->sizecode;*/
    var->varname = NULL;
    decl->str = NULL;
    decl->len = 0;
  }
  else {
    lua_assert(var->startpc == fs->pc);
    expts(decl, var->varname);
  }
  prepend_exp(fs, decl);
}


/* discharge the pending expression chain */
static void discharge(DFuncState *fs, DecompState *D) {
  DExp *exp = fs->explist.first;
  printf("--> DISCHARGING:\n");
  if (exp == NULL) {
    lua_assert(fs->nexps == 0);
    return; /* nothing to output */
  }
  /* the first expression is treated differently than the rest */
  /* currently the logic handles an LHS expression */
  printf("    dumping \"%s\" (len %zu)\n", exp->str, exp->len);
  if (deisdecl(exp)) {
    /*lua_assert(!deiscall(exp));*/
    DumpLiteral("local ",D);
    if (exp->str != NULL) { /* dump variable name */
      lua_assert(exp->len > 0);
      DumpExp(exp,D);
    }
    else { /* generate a variable name */
      int idx = delocidx(exp);
      lua_assert(idx < fs->nlocvars);
      DumpAugLocVar(fs,idx,D);
    }
  }
  else if (deisret(exp)) {
    DumpLiteral("return",D);
    if (exp->str != NULL) {
      DumpLiteral(" ",D);
    }
  }
  DumpExp(exp,D);
  if (deislhs(exp)) DumpLiteral(" = ",D); /* RHS follows */
  goto nextexp;
  while (exp != NULL) {
    DExp *next;
    lua_assert(!deislhs(exp));
    printf("    dumping \"%s\" (len %zu)\n", exp->str, exp->len);
    DumpExp(exp, D);
    nextexp:
    next = exp->next;
    luaM_free(D->H, exp);
    exp = next;
    fs->nexps--;
  }
  fs->explist.first = fs->explist.last = NULL;
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

static void DecompileFunction(Proto *f, TString *p, DecompState *D);

static void DecompileCode(Proto *f, DFuncState *fs, DecompState *D)
{
  const Instruction *code=f->code;
  int n=f->sizecode;
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
    int line=getline(f,pc); (void)line;
#if 0
    if (line > 0 && match_lines(D)) {
      /* TODO: need to append a DExp with type LF to the list, so that pending
      expressions get output on the correct line */
      int lines_needed = line - D->line;
      lua_assert(lines_needed >= 0);
      AddLines(lines_needed, D);
    }
#endif
    if (testAMode(o)) { /* a is a register */
      DExp *last = fs->explist.last;
      printf("%s uses reg A (%d)", luaP_opnames[o], a);
      if (last != NULL)
        printf(" last pending uses (%d)", dereg(last));
      printf("\n");
      if (last != NULL && dereg(last) < a) {
        /*if (deisid(last)) {
          adddecl(fs, last);
        }*/
        if (deisid(last))
          adddecl(fs, last);
        discharge(fs, D);
        NextLine(D);
      }
    }
    switch (o)
    {
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
      case OP_RETURN: {
        if (pc == n-1) { /* final return */
          lua_assert(a == 0 && b == 1);
        }
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
        NextLine(D);
      }
    }
  }
}

static void DecompileFunction(Proto *f, TString *p, DecompState *D)
{
  DFuncState fs;
  open_func(&fs, f, D);
  UNUSED(p);
  if (f->name && getstr(f->name))
    printf("-- Decompiling function named '%s'\n", getstr(f->name));
  DecompileCode(f,&fs,D);
  discharge(&fs, D);
  close_func(&fs, f, D);
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
  D.line=1;
  D.indentlevel=0;
  D.funcidx=0;
  DecompileFunction((Proto *)f,NULL,&D);
  (void)f;
  return D.status;
}
