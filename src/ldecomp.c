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
#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"
#include "lzio.h"

#ifdef HKSC_DECOMPILER

#ifdef HKSC_DECOMP_DEBUG_PASS1
#undef HKSC_DECOMP_HAVE_PASS2
#else /* !HKSC_DECOMP_DEBUG_PASS1 */
#define HKSC_DECOMP_HAVE_PASS2
#endif /* HKSC_DECOMP_DEBUG_PASS1 */

/*
** Check whether an op loads a constant into register A. Helps determine if a
** tested expression starts evaluating at an instruction. Ops that load
** constants warrant their own statements, as otherwise the constant would be
** indexed as an operand directly in the test-instruction.
*/
#define opLoadsK(o) ((o) == OP_LOADK || (o) == OP_LOADBOOL || (o) == OP_LOADNIL)

#ifdef LUA_DEBUG

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

#ifdef HKSC_DECOMP_HAVE_PASS2
#define DEFREGFLAG(e)  "REG_" #e,
static const char *const regflagnames [] = {
  REGFLAG_TABLE
  "MAX_REGFLAG"
};
#undef DEFREGFLAG
#endif /* HKSC_DECOMP_HAVE_PASS2 */

#define bbltypename(v) (bbltypenames[v])
#define insflagname(v) (insflagnames[v])
#define regflagname(v) (regflagnames[v])

#else /* !LUA_DEBUG */

#define bbltypename(v) ("")
#define insflagname(v) ("")
#define regflagname(v) ("")

#endif /* LUA_DEBUG */


/* for formatting decimal integers in generated variable names */
#define INT_CHAR_MAX_DEC (3 * sizeof(int) * (CHAR_BIT/8))

struct DFuncState;

struct HoldItem; /* used in pass2 to hold onto strings before dumping them */

typedef struct {
  hksc_State *H;
  struct DFuncState *fs;  /* current function state */
  lua_Writer writer;
  void *data;
  const char *name;  /* input name */
  int status;
  int usedebuginfo;  /* true if using debug info */
  int matchlineinfo;  /* true if matching statements to line info */
  int funcidx;  /* n for the nth function that is being decompiled */
  int indentlevel;  /* indentation level counter */
  int linenumber;  /* output line counter */
  int needspace;  /* for adding space between tokens */
  struct HoldItem *holdfirst;  /* first hold item in the chain */
  struct HoldItem *holdlast;  /* last hold item in the chain */
  Mbuffer buff;  /* buffer for building strings */
} DecompState;


/* a decompiled function */
typedef struct DFuncState {
  struct DFuncState *prev;  /* enclosing function */
  DecompState *D;  /* decompiler state */
  Analyzer *a;  /* function analyzer data */
  const Proto *f;  /* current function header */
  hksc_State *H;  /* copy of the Lua state */
  int idx;  /* the nth function, used for generating local variable names */
  struct LocVar *locvars;  /* information about local variables */
  TString **upvalues;
  int nlocvars;  /* number of local variables created so far */
  int nlocvarsdetected;  /* number of local variables detected so far */
  int sizelocvars;
  int sizeupvalues;
  int instatement;
  int lastclosurepc;  /* PC of last OP_CLOSURE */
  int firstclob;  /* first pc that clobbers register A */
  int firstclobnonparam;  /* first pc that clobbers non-parameter register A */
  int firstfree;
  int lastfirstfree;
  int lastcallexp;  /* exp index of last function call node */
  int upvalcount;  /* number of upvalues encountered so far */
  int nopencalls;  /* number of OpenExpr entries created */
  int nregnotes;  /* number of RegNote entries created */
} DFuncState;


#ifdef LUA_DEBUG
#define D(x) x

static void lprintf(const char *fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
  for (;;) {
    const char *e = strchr(fmt, '%');
    if (e == NULL) break;
    printf("%.*s", cast_int(e-fmt), fmt);
    switch (*(e+1)) {
      case 's': {
        const char *s = va_arg(argp, char *);
        if (s == NULL) s = "(null)";
        fputs(s, stdout);
        break;
      }
#ifdef HKSC_DECOMP_HAVE_PASS2
      case 'R': /* register flag */
        printf("%s", regflagname(va_arg(argp, int)));
        break;
#endif /* HKSC_DECOMP_HAVE_PASS2 */
      case 'I': /* instruction flag */
        printf("%s", insflagname(va_arg(argp, int)));
        break;
      case 'i': /* pc */
        printf("%d", va_arg(argp, int)+1);
        break;
      case 'o': /* opcode */
        printf("OP_%s", getOpName(va_arg(argp, OpCode)));
        break;
      case 'O': { /* opcode + operands */
        Instruction i = va_arg(argp, Instruction);
        OpCode o = GET_OPCODE(i);
        int a = GETARG_A(i);
        int b = GETARG_B(i);
        int c = GETARG_C(i);
        int bx = GETARG_Bx(i);
        int sbx = GETARG_sBx(i);
        printf("OP_%s ", getOpName(o));
        switch (getOpMode(o)) {
          case iABC:
            printf("%d", a);
            if (getBMode(o)!=OpArgN) {
              if (ISK(b)) printf(" K(%d)", INDEXK(b));
              else printf(" %d", b);
            }
            if (getCMode(o)!=OpArgN) {
              if (ISK(c)) printf(" K(%d)", INDEXK(c));
              else printf(" %d", c);
            }
            break;
          case iABx:
            printf("%d %d", a, bx);
            break;
          case iAsBx:
            if (o == OP_JMP) printf("%d",sbx);
            else printf("%d %d", a, sbx);
            break;
        }
        break;
      }
      case 'B': { /* BasicBlock * */
        BasicBlock *block = va_arg(argp, BasicBlock *);
        if (block != NULL)
          printf("(%s) (%d-%d)", bbltypename(block->type), block->startpc+1,
                 block->endpc+1);
        else
          printf("(NULL)");
        break;
      }
      case 'b': { /* BasicBlock * (type only) */
        BasicBlock *block = va_arg(argp, BasicBlock *);
        if (block != NULL)
          printf("%s", bbltypename(block->type));
        else
          printf("(NULL)");
        break;
      }
      case 'c':
        putchar(va_arg(argp, int));
        break;
      case 'd':
        printf("%d", va_arg(argp, int));
        break;
      case 'u':
        printf("%u", va_arg(argp, unsigned int));
        break;
      case 'p':
        printf("%p", va_arg(argp, void *));
        break;
      case '%':
        putchar('%');
        break;
      default:
        putchar('%');
        putchar(*(e+1));
        break;
    }
    fmt = e+2;
  }
  if (*fmt != '\0')
    printf("%s", fmt);
  va_end(argp);
}

static void printinsflags(DFuncState *fs, int pc, const char *preamble)
{
  int i;
  lprintf("%spc (%i):", preamble, pc);
  for (i = 0; i < MAX_INSFLAG; i++) {
    if (fs->a->insproperties[pc] & (1 << i))
      lprintf("  %I", i);
  }
  lprintf("\n");
}

#ifdef HKSC_DECOMP_HAVE_PASS2

static void printregflags(DFuncState *fs, int reg, const char *preamble)
{
  int i;
  lprintf("%sreg (%d):", preamble, reg);
  for (i = 0; i < MAX_REGFLAG; i++) {
    if (fs->a->insproperties[reg] & (1 << i))
      lprintf("  %R", i);
  }
  lprintf("\n");
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


#else /* !LUA_DEBUG */
#define D(x) ((void)0)
#define printinsflags(fs,pc,preamble) ((void)0)
#endif /* LUA_DEBUG */


#define CHECK(fs,c,msg) if (!(c)) badcode(fs,msg)

static void badcode(DFuncState *fs, const char *msg)
{
  const char *name = fs->D->name;
  luaD_setferror(fs->H, "%s: bad code in precompiled chunk: %s", name, msg);
  luaD_throw(fs->H, LUA_ERRSYNTAX);
}


static int ispcvalid(DFuncState *fs, int pc)
{
  return (pc >= 0 && pc < fs->f->sizecode);
}


static int isregvalid(DFuncState *fs, int reg)
{
  return (reg >= 0 && reg < fs->f->maxstacksize);
}


/*
** test_ins_property - check if flag PROP is set on the instruction at PC
*/
static int test_ins_property(DFuncState *fs, int pc, int prop)
{
  lua_assert(ispcvalid(fs, pc));
  return ((fs->a->insproperties[pc] & (1 << prop)) != 0);
}

#define check_ins_property(fs,pc,prop) lua_assert(test_ins_property(fs,pc,prop))


/*
** set_ins_property - set flag PROP for the instruction at PC
*/
static void set_ins_property(DFuncState *fs, int pc, int prop)
{
  lua_assert(ispcvalid(fs, pc));
#ifdef LUA_DEBUG
  lprintf("  marking pc (%i) as %I\n", pc, prop);
  lua_assert(pc >= 0 && pc < fs->f->sizecode);
  printinsflags(fs, pc, "  previous flags for ");
#endif /* LUA_DEBUG */
  fs->a->insproperties[pc] |= (1 << prop);
}


/*
** unset_ins_property - clear flag PROP for the instruction at PC
*/
static void unset_ins_property(DFuncState *fs, int pc, int prop)
{
  lua_assert(ispcvalid(fs, pc));
#ifdef LUA_DEBUG
  lprintf("  clearing flag %I for pc (%i)\n", prop, pc);
#endif /* LUA_DEBUG */
  fs->a->insproperties[pc] &= ~(1 << prop);
}


/*
** init_ins_property - set flag PROP on the instruction at PC, assert the flag
** was not previously set
*/
#define init_ins_property(fs,pc,prop) \
  (lua_assert(!test_ins_property(fs,pc,prop)), set_ins_property(fs,pc,prop))


#ifdef HKSC_DECOMP_HAVE_PASS2
/*
** set_reg_property - set flag PROP for register REG
*/
static void set_reg_property(DFuncState *fs, int reg, int prop)
{
  lua_assert(isregvalid(fs, reg));
#ifdef LUA_DEBUG
  lprintf("  marking register (%d) as %R\n", reg, prop);
  printregflags(fs, reg, "  previous flags for ");
#endif /* LUA_DEBUG */
  fs->a->regproperties[reg].flags |= (1 << prop);
}

/*
** unset_reg_property - clear flag PROP for register REG
*/
static void unset_reg_property(DFuncState *fs, int reg, int prop)
{
  lua_assert(isregvalid(fs, reg));
#ifdef LUA_DEBUG
  lprintf("  clearing flag %R for register (%d)\n", prop, reg);
#endif /* LUA_DEBUG */
  fs->a->regproperties[reg].flags &= ~(1 << prop);
}

/*
** test_reg_property - test flag PROP for register REG
*/
static int test_reg_property(DFuncState *fs, int reg, int prop)
{
  lua_assert(isregvalid(fs, reg));
  return ((fs->a->regproperties[reg].flags & (1 << prop)) != 0);
}

#define check_reg_property(fs,reg,val) lua_assert(test_reg_property(fs,reg,val))
#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*
** allocate a new block structure and return it
*/
static BasicBlock *newbbl(hksc_State *H, int startpc, int endpc, int type) {
  BasicBlock *bbl = luaM_new(H, BasicBlock);
  bbl->next = NULL;
  bbl->nextsibling = NULL;
  bbl->firstchild = NULL;
  bbl->startpc = startpc;
  bbl->endpc = endpc;
  bbl->type = type;
  bbl->isempty = (endpc < startpc);
  D(bbl->visited = 0);
  return bbl;
}


/*
** creates a new OpenExpr entry
*/
static OpenExpr *newopenexpr(DFuncState *fs, int firstreg, int startpc,
                             int endpc, int kind)
{
  OpenExpr *expr;
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  lua_assert(fs->nopencalls >= 0 && fs->nopencalls <= a->sizeopencalls);
  luaM_growvector(H, a->opencalls, fs->nopencalls, a->sizeopencalls, OpenExpr,
                  MAX_INT, "too many OpenExpr entries");
  expr = &a->opencalls[fs->nopencalls++];
  expr->kind = kind;
  expr->startpc = startpc;
  expr->endpc = endpc;
  expr->firstreg = firstreg;
  return expr;
}


/*
** create a new RegNote entry
*/
static RegNote *newregnote(DFuncState *fs, int note, int pc, int reg)
{
  RegNote *regnote;
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  lua_assert(fs->nregnotes >= 0 && fs->nregnotes <= a->sizeregnotes);
  luaM_growvector(H, a->regnotes, fs->nregnotes, a->sizeregnotes, RegNote,
                  MAX_INT, "too many RegNote entries");
  { /* the array shall be sorted by register first, then by pc, descending */
    int pos, low, high, mid, i;
    low = 0;
    high = fs->nregnotes;
    while (1) {
      int result;  /* comparison result */
      if (high <= low) {
        pos = low;
        break;
      }
      mid = (low+high)/2;
      /* comparison: RegNote a, b;
         a < b if a.reg < b.reg
         a < b if a.reg == b.reg and a.pc < b.pc */
      result = a->regnotes[mid].reg - reg;
      if (result == 0)
        result = a->regnotes[mid].pc - pc;
      if (result == 0) {
        pos = mid+1;
        break;
      }
      else if (result > 0)
        low = mid+1;
      else
        high = mid;
    }
    for (i = fs->nregnotes; i > pos; i--)
      a->regnotes[i] = a->regnotes[i-1];
    regnote = &a->regnotes[i];
  }
  fs->nregnotes++;
  regnote->note = note;
  regnote->pc = pc;
  regnote->reg = reg;
  return regnote;
}


#ifdef HKSC_DECOMP_HAVE_PASS2

/*
** returns the next free expression in the expression node stack and increment
** the number of stack elements in use
*/
static ExpNode *newexp(DFuncState *fs)
{
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  lua_assert(a->expstack.used >= 0 && a->expstack.used <= a->expstack.total);
  luaM_growvector(H, a->expstack.stk, a->expstack.used, a->expstack.total,
                  ExpNode, MAX_INT, "too many expression nodes");
  return &a->expstack.stk[a->expstack.used++];
}


#define prevexp(fs,exp) index2exp(fs, exp->previndex)

static int exp2index(DFuncState *fs, ExpNode *exp)
{
  if (exp == NULL)
    return 0;
  else {
    lua_assert(exp >= fs->a->expstack.stk &&
               exp < fs->a->expstack.stk+fs->a->expstack.used);
    return exp-fs->a->expstack.stk+1;
  }
}


static ExpNode *index2exp(DFuncState *fs, int index)
{
  if (index == 0)
    return NULL;
  else
    return fs->a->expstack.stk+(index-1);
}


#define checkfirstexp(fs) check_exp(getfirstexp(fs) != NULL, getfirstexp(fs))
#define checktopexp(fs) check_exp(gettopexp(fs) != NULL, gettopexp(fs))

static ExpNode *getfirstexp(DFuncState *fs)
{
  int used = fs->a->expstack.used;
  if (used == 0)
    return NULL;
  else {
    lua_assert(used > 0);
    return &fs->a->expstack.stk[0];
  }
}


static ExpNode *gettopexp(DFuncState *fs)
{
  int used = fs->a->expstack.used;
  if (used == 0)
    return NULL;
  else {
    lua_assert(used > 0);
    return &fs->a->expstack.stk[used-1];
  }
}


#ifdef LUA_DEBUG
static const char *getunoprstring(UnOpr op);
static const char *getbinoprstring(BinOpr op);

static void debugexp(DFuncState *fs, ExpNode *exp, int indent)
{
  if (exp == NULL) {
    lprintf("(ExpNode *)0\n");
    return;
  }
  lprintf("exp: %d", exp->info);
  if ((exp->kind == ENIL || exp->kind == EVARARG) && exp->aux != exp->info)
    lprintf("-%d", exp->aux);
  lprintf(" <- ");
  switch (exp->kind) {
    case EVOID:
      lprintf("(void)");
      break;
    case ENIL:
      lprintf("'nil'");
      break;
    case ETRUE:
      lprintf("'true'");
      break;
    case EFALSE:
      lprintf("'false'");
      break;
    case EVARARG:
      lprintf("'...'");
      break;
    case ELITERAL: {
      TValue *o = exp->u.k;
      switch (ttype(o)) {
        case LUA_TNIL:
          lprintf("'nil'");
          break;
        case LUA_TBOOLEAN:
          lprintf("'%s'", bvalue(o) ? "true" : "false");
          break;
        case LUA_TLIGHTUSERDATA: {
          char s[LUAI_MAXUI642STR+sizeof("0xhi")-1];
          sprintf(s, "0x%zxhi", cast(size_t, pvalue(o)));
          lprintf("%s", s);
          break;
        }
        case LUA_TNUMBER: {
          char s[LUAI_MAXNUMBER2STR];
          sprintf(s, "%g", nvalue(o));
          lprintf("%s", s);
          break;
        }
        case LUA_TSTRING:
          lprintf("%s", getstr(luaO_kstring2print(fs->H, rawtsvalue(o))));
          break;
        case LUA_TUI64: {
          char s[LUAI_MAXUI642STR+sizeof("0xhl")-1];
          lua_ui642str(s+2, ui64value(o));
          s[0] = '0'; s[1] = 'x';
          strcat(s, "hl");
          lprintf("%s", s);
          break;
        }
        default:
          lprintf("? type=%d", ttype(o));
          break;
      }
      break;
    }
    case ECALL:
    case ETAILCALL:
      lprintf("[CALL %d,  %d ret, %d arg]", exp->info, exp->u.call.nret,
              exp->u.call.narg);
      break;
    case ECONCAT:
      /*lprintf("[CONCAT %d..%d]", index2exp(fs, exp->u.concat.firstindex)->info,
              index2exp(fs, exp->u.concat.lastindex)->info);*/
      break;
    case EGLOBAL:
      lprintf("_G.%s", getstr(exp->u.name));
      break;
    case EUPVAL:
      lprintf("upval=%s", getstr(exp->u.name));
      break;
    case EBINOP:
      lprintf("[BINOP %s  %d, %d]", getbinoprstring(exp->u.binop.op),
              exp->u.binop.b, exp->u.binop.c);
      break;
    case EUNOP:
      lprintf("[UNOP %s  %d]", getunoprstring(exp->u.unop.op),
              exp->u.unop.b);
      break;
    case ECON:
      lprintf("'{}' %d, %d", exp->u.con.arrsize, exp->u.con.hashsize);
      break;
    case ESTORE:
      lprintf("STORE (from %d)", exp->u.store.srcreg);
      break;
    default:
      break;
  }
  lprintf("\n");
  if (prevexp(fs,exp) == NULL)
    return;
  indent++;
  {
    int i;
    for (i = 0; i < indent; i++)
      lprintf("  ");
  }
  lprintf("- ");
  debugexp(fs, prevexp(fs,exp), indent);
}
#else /* LUA_DEBUG */
#define debugexp(fs,exp,indent) ((void)(exp))
#endif /* LUA_DEBUG */



/*
** returns the SlotDesc entry for a given register
*/
static SlotDesc *getslotdesc(DFuncState *fs, int reg)
{
  SlotDesc *slot;
  Analyzer *a = fs->a;
  lua_assert(isregvalid(fs, reg));
  slot = &a->regproperties[reg];
  return slot;
}


/*
** populates the `upvalues' array with the names of the upvalues from PARENT
** that FS uses
*/
static void getupvaluesfromparent(DFuncState *fs, DFuncState *parent)
{
  const Instruction *code;
  int i, pc;
  lua_assert(parent != NULL && parent->f != NULL);
  code = parent->f->code;
  lua_assert(ispcvalid(parent, parent->lastclosurepc));
  lua_assert(GET_OPCODE(code[parent->lastclosurepc]) == OP_CLOSURE);
  pc = parent->lastclosurepc+1;
  i = 0;
  while (1) {
    TString *upvalue;
    int a, bx;
    Instruction insn = code[pc++];
    OpCode op = GET_OPCODE(insn);
    if (op != OP_DATA)
      break;
    lua_assert(i < fs->sizeupvalues);
    a = GETARG_A(insn);
    bx = GETARG_Bx(insn);
    if (a == 1) {
      lua_assert(isregvalid(parent, bx));
      lua_assert(test_reg_property(parent, bx, REG_LOCAL));
      upvalue = getslotdesc(parent, bx)->u.locvar->varname;
    }
    else {
      lua_assert(bx < parent->sizeupvalues);
      upvalue = parent->upvalues[bx];
    }
    fs->upvalues[i++] = upvalue;
  }
  CHECK(fs, i == fs->sizeupvalues, "not enough OP_DATA codes for number of "
        "upvalues");
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


static void open_func (DFuncState *fs, DecompState *D, const Proto *f) {
  hksc_State *H = D->H;
  Analyzer *a = luaA_newanalyzer(H);
  fs->a = a;
  fs->prev = D->fs;  /* linked list of funcstates */
  fs->D = D;
  fs->H = H;
  D->fs = fs;
  fs->f = f;
  fs->idx = D->funcidx++;
  fs->nlocvars = fs->nlocvarsdetected = 0;
  fs->instatement = 0;
  if (f->name && getstr(f->name))
    D(lprintf("-- Decompiling function (%d) named '%s'\n", D->funcidx,
             getstr(f->name)));
  else
    D(lprintf("-- Decompiling anonymous function (%d)\n", D->funcidx));
  if (D->usedebuginfo) { /* have debug info */
    D(lprintf("using debug info for function '%s'\n", f->name ? getstr(f->name):
             "(anonymous)"));
    fs->sizelocvars = f->sizelocvars;
    fs->locvars = f->locvars;
    fs->sizeupvalues = f->sizeupvalues;
    fs->upvalues = f->upvalues;
  }
  else {
    fs->sizelocvars = f->maxstacksize;
    a->sizelocvars = fs->sizelocvars;
    a->locvars = luaM_newvector(H, a->sizelocvars, struct LocVar);
    memset(a->locvars, 0, a->sizelocvars * sizeof(struct LocVar));
    fs->locvars = a->locvars;
    fs->sizeupvalues = f->nups;
    a->sizeupvalues = fs->sizeupvalues;
    a->upvalues = luaM_newvector(H, a->sizeupvalues, TString *);
    memset(a->upvalues, 0, a->sizeupvalues * sizeof(TString *));
    fs->upvalues = a->upvalues;
    fs->sizeupvalues = a->sizeupvalues;
  }
  fs->upvalcount = 0;
  fs->nopencalls = 0;
  fs->nregnotes = 0;
  fs->firstclob = -1;
  fs->firstclobnonparam = -1;
  fs->firstfree = -1;
  fs->lastfirstfree = -1;
  fs->lastcallexp = 0;
  /* allocate vectors for instruction and register properties */
  a->sizeinsproperties = f->sizecode; /* flags for each instruction */
  a->insproperties = luaM_newvector(H, a->sizeinsproperties, InstructionFlags);
  memset(a->insproperties, 0, f->sizecode * sizeof(InstructionFlags));
  a->sizeregproperties = f->maxstacksize; /* flags for each register */
  a->regproperties = luaM_newvector(H, f->maxstacksize, SlotDesc);
  memset(a->regproperties, 0, a->sizeregproperties * sizeof(SlotDesc));
  /* allocate stack for expression nodes */
  a->expstack.total = 4;
  a->expstack.used = 0;
  a->expstack.stk = luaM_newvector(H, a->expstack.total, ExpNode);
#ifdef HKSC_DECOMP_HAVE_PASS2
  if (fs->prev != NULL)
    getupvaluesfromparent(fs, fs->prev);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
}


static void close_func (DecompState *D) {
  DFuncState *fs = D->fs;
  D->funcidx--;
  UNUSED(fs->locvars);
  UNUSED(fs->sizelocvars);
  UNUSED(fs->D->usedebuginfo);
  D->fs = fs->prev;
  killtemp(obj2gco(fs->a)); /* make analyzer collectable */
  UNUSED(fs->a);
}


/*
** updates the first pc that clobbers a register
*/
static void updatefirstclob1(DFuncState *fs, int pc, int reg)
{
  lua_assert(isregvalid(fs, reg));
  fs->firstclob = pc;
  if (reg >= fs->f->numparams)
    fs->firstclobnonparam = pc;
}



static void addsibling1(BasicBlock *bbl1, BasicBlock *bbl2) {
  lua_assert(bbl1 != NULL);
  bbl1->nextsibling = bbl2;
}

static BasicBlock *addbbl1(DFuncState *fs, int startpc, int endpc, int type) {
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  BasicBlock *curr, *new;
  curr = a->bbllist.first;
  new = newbbl(H, startpc, endpc, type);
  new->next = curr;
  a->bbllist.first = new;
  if (curr == NULL)
    a->bbllist.last = new;
  D(lprintf("recording new basic block of type %b (%i-%i)\n", new, startpc,
            endpc));
  return new;
}


/*
** delete a BasicBlock node and return the new earliest block based on the
** arguments provided (promote all the block's children to siblings)
*/
static BasicBlock *rembbl1(DFuncState *fs, BasicBlock *bbl,
                           BasicBlock *prevsibling) {
  BasicBlock *child = bbl->firstchild;
  BasicBlock *earliestblock; /* return value */
  lua_assert(prevsibling == NULL || prevsibling->nextsibling == bbl);
  D(lprintf("deleting erroneous block %B\n", bbl));
  /* remove BBL from its sibling chain (promote any children) */
  if (child != NULL) { /* promote children */
    if (prevsibling != NULL) {
      prevsibling->nextsibling = child;
      earliestblock = prevsibling;
    }
    else {
      earliestblock = child;
    }
    while (child->nextsibling != NULL)
      child = child->nextsibling;
    /* connect the last child and the next sibilng */
    child->nextsibling = bbl->nextsibling;
  }
  else if (prevsibling != NULL) {
    /* connect the previous sibling and the next sibling */
    prevsibling->nextsibling = bbl->nextsibling;
    earliestblock = prevsibling;
  }
  else {
    earliestblock = bbl->nextsibling;
  }
  /* remove BBL from the linked list */
  if (fs->a->bbllist.first == bbl) { /* BBL is first in the list */
    fs->a->bbllist.first = bbl->next;
    if (fs->a->bbllist.last == bbl)
      fs->a->bbllist.last = fs->a->bbllist.first;
  }
  else {
    BasicBlock *iterblock = fs->a->bbllist.first;
    while (iterblock->next != bbl)
      iterblock = iterblock->next;
    lua_assert(iterblock->next == bbl);
    if (fs->a->bbllist.last == bbl) {
      fs->a->bbllist.last = iterblock;
      lua_assert(bbl->next == NULL);
    }
    iterblock->next = bbl->next;
  }
  luaM_free(fs->H, bbl);
  return earliestblock;
}


#if 0
static BasicBlock *addbbl2(DFuncState *fs, int startpc, int endpc, int type) {
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  BasicBlock *curr, *new;
  curr = a->bbllist.last;
  new = newbbl(H, startpc, endpc, type);
  /* the first pass should always create at least 1 block */
  lua_assert(a->bbllist.first != NULL);
  lua_assert(a->bbllist.last != NULL);
  curr->next = new;
  a->bbllist.last = new;
}
#endif


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

/*
static void DumpStringf(DecompState *D, const char *fmt, ...)
{
  va_list argp;
  const char *s;
  va_start(argp, fmt);
  s = luaO_pushvfstring(D->H, fmt, argp);
  DumpString(s,D);
  va_end(argp);
}*/


/*
** declarations for dump functions used by both passes
*/
static void DumpIndentation(DecompState *D);


#ifdef HKSC_DECOMP_HAVE_PASS2

/*
** dumps a TString data to output
*/
static void DumpTString(const TString *ts, DecompState *D)
{
  lua_assert(ts != NULL);
  DumpBlock(getstr(ts), ts->tsv.len, D);
}


/*
** prints a Lua object as it would appear in source code to output
*/
static void DumpTValue(const TValue *o, DecompState *D)
{
  switch (ttype(o))
  {
    case LUA_TNIL:
      DumpLiteral("nil",D);
      break;
    case LUA_TBOOLEAN:
      if (bvalue(o)) DumpLiteral("true",D);
      else DumpLiteral("false",D);
      break;
    case LUA_TLIGHTUSERDATA: {
      char s[LUAI_MAXUI642STR+sizeof("0xhi")-1];
      sprintf(s, "0x%zuhi", cast(size_t, pvalue(o)));
      DumpString(s,D);
      break;
    }
    case LUA_TNUMBER: {
      char s[LUAI_MAXNUMBER2STR];
      sprintf(s, "%g", nvalue(o));
      DumpString(s,D);
      break;
    }
    case LUA_TSTRING:
      DumpTString(luaO_kstring2print(D->H, rawtsvalue(o)), D);
      break;
    case LUA_TUI64: {
      char s[LUAI_MAXUI642STR+sizeof("oxhl")-1];
      lua_ui642str(s+2, ui64value(o));
      s[0] = '0'; s[1] = 'x';
      strcat(s, "hl");
      DumpString(s,D);
      break;
    }
    default:
      lua_assert(0);
      break;
  }
}


/*
** dumps a constant indexed by INDEX from the constant table
*/
static void DumpConstant(DFuncState *fs, int index, DecompState *D)
{
  const Proto *f = fs->f;
  const TValue *o = &f->k[index];
  DumpTValue(o,D);
}

/*
** dumps a semicolon to output
*/
static void DumpSemi(DecompState *D)
{
  DumpLiteral(";",D);
  D->needspace = 1;
}


/*
** dumps a comma to output
*/
static void DumpComma(DecompState *D)
{
  DumpLiteral(",",D);
  D->needspace = 1;
}

/*
** dumps a space to output
*/
static void DumpSpace(DecompState *D)
{
  DumpLiteral(" ",D);
  D->needspace = 0;
}


/*
** checks if a pending space is needed and discharges it
*/
static void CheckSpaceNeeded(DecompState *D)
{
  if (D->needspace)
    DumpSpace(D);
}


/*
** dumps N linefeeds to output and updates the current line counter
*/
static void beginline2(DFuncState *fs, int n, DecompState *D)
{
  static const char lf[] = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  const int buffsize = cast_int(sizeof(lf)-1);
  D->linenumber += n;
  D(lprintf("adding %d line%s, updating D->linenumber to (%d)\n",
            n, n == 1 ? "" : "s", D->linenumber));
  lua_assert(n > 0);
  while (n > buffsize) {
    DumpBlock(lf, buffsize, D);
    n -= buffsize;
  }
  lua_assert(n >= 0 && n <= buffsize);
  if (n != 0)
    DumpBlock(lf, n, D);
  DumpIndentation(D);
  D->needspace = 0;
  UNUSED(fs);
}


/*
** updates the line counter, dumping new lines to output if needed
*/
static void updateline2(DFuncState *fs, int line, DecompState *D)
{
  if (line > D->linenumber) {
    int lines_needed = line - D->linenumber;
    beginline2(fs, lines_needed, D);
    lua_assert(D->linenumber == line);
  }
}


static void maybebeginline2(DFuncState *fs, DecompState *D)
{
  lua_assert(fs->instatement >= 0);
  if (fs->instatement == 0)
    beginline2(fs, 1, D);
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*
** dumps indentation of to the current indentation level using tabs
*/
static void DumpIndentation(DecompState *D)
{
  static const char tabs[] = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" /* 16 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" /* 32 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" /* 48 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"; /* 64 */
  const int buffsize = cast_int(sizeof(tabs)-1);
  int indentlevel = D->indentlevel;
  lua_assert(indentlevel >= 0);
  while (indentlevel > buffsize) {
    DumpBlock(tabs, buffsize, D);
    indentlevel -= buffsize;
  }
  lua_assert(indentlevel >= 0 && indentlevel <= buffsize);
  if (indentlevel != 0)
    DumpBlock(tabs, indentlevel, D);
}


#ifdef LUA_DEBUG

static void debugbbl(DFuncState *fs, BasicBlock *bbl, int indent) {
  BasicBlock *child, *nextsibling;
  int i;
  lua_assert(bbl != NULL);
  child = bbl->firstchild;
  nextsibling = bbl->nextsibling;
  for (i = 0; i < indent; i++)
    lprintf("  ");
  if (indent) lprintf("- ");
  lprintf("(%i-%i) %b ", bbl->startpc, bbl->endpc, bbl);
  if (nextsibling != NULL)
    lprintf("(sibling (%i-%i) %b)\n", nextsibling->startpc, nextsibling->endpc,
            nextsibling);
  else
    lprintf("(NO sibling)\n");
  while (child != NULL) {
    debugbbl(fs, child, indent+1);
    child = child->nextsibling;
  }
}


static void debugbblsummary(DFuncState *fs)
{
  BasicBlock *first = fs->a->bbllist.first;
  lprintf("BASIC BLOCK SUMMARY\n"
         "-------------------\n");
  lua_assert(first != NULL);
  lua_assert(first->type == BBL_FUNCTION);
  lua_assert(first->nextsibling == NULL);
  debugbbl(fs, first, 0);
  lprintf("-------------------\n");
}


static void debugopenexpr(const OpenExpr *e)
{
  static const char *const typenames[] = {
    "PRECALL",
    "PRECONCAT",
    "PREFORNUM",
    "PREFORLIST",
    "PRESETLIST",
    "EMPTYTABLE",
    "PRERETURN"
  };
  lprintf("(%i-%i, reg %d) %s\n", e->startpc, e->endpc, e->firstreg,
          typenames[e->kind]);
}


static void debugopenexprsummary(DFuncState *fs)
{
  int i;
  lprintf("OPEN EXPRESSION SUMMARY\n"
         "-------------------\n");
  for (i = fs->nopencalls-1; i >= 0; i--)
    debugopenexpr(&fs->a->opencalls[i]);
  lprintf("-------------------\n");
}


static void debugregnotesummary(DFuncState *fs)
{
  static const char *const typenames[] = {
    "REG_NOTE_UPVALUE",
    "REG_NOTE_NONRELOC"
  };
  RegNote *regnote;
  int i;
  lprintf("REG NOTE SUMMARY\n"
          "-------------------\n");
  for (i = fs->nregnotes-1; i >= 0; i--) {
    int note;
    regnote = &fs->a->regnotes[i];
    note = regnote->note;
    lprintf("(pc %i, reg %d) %s\n", regnote->pc, regnote->reg, typenames[note]);
  }
  lprintf("-------------------\n");
}


static void debugpass1summary(DFuncState *fs)
{
  debugbblsummary(fs); lprintf("\n");
  debugopenexprsummary(fs); lprintf("\n");
  debugregnotesummary(fs);
}


static void checktreevisited(BasicBlock *bbl)
{
  BasicBlock *child;
  lua_assert(bbl != NULL);
  child = bbl->firstchild;
  lua_assert(bbl->visited);
  while (child != NULL) {
    checktreevisited(child);
    child = child->nextsibling;
  }
}


#else /* !LUA_DEBUG */

#define debugpass1summary(fs)  ((void)(fs))
#define checktreevisited(bbl)  ((void)(bbl))

#endif /* LUA_DEBUG */


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

#if 0
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
#endif

/*
** Check if the given instruction I begins a temporary expression in register
** FISRTREG. PC is the pc of I. JUMPLIMIT is a pc used to check if a preceding
** jump instruction jumps to somewhere within the temporary expression
** evaluation, meaning it is part of the temporary expression. CODE is the
** instruction array which contains I, used for single look-behinds.
*/
static int beginstempexpr(const Instruction *code, Instruction i, int pc,
                          int firstreg, int jumplimit, int *nextstart)
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
              if (prevop == OP_DATA) {
                pc--; goto checkprevreg;  /* skip OP_DATA */
              }
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
        if (beginseval(o, a, b, c, 1)) {
          if (nextstart != NULL && a >= firstreg) *nextstart = pc;
          if (a == firstreg) {
            if ((pc-1) >= 0 && GET_OPCODE(code[pc-1]) == OP_JMP)
              goto checkjumptarget;
            /* clobbers the first free register without using what was
               previously stored inside it, and there is no jump to precede this
               instruction; this is the start of a temporary expression. */
            return 1;
          }
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
  struct {
    int endpc, reg;
  } retpending;
  const Instruction *code;  /* f->code */
} CodeAnalyzer;


#ifdef LUA_DEBUG
#define printinsn1(pc,i) lprintf("pc %i: %O\n",pc,i)
#else /* !LUA_DEBUG */
#define printinsn1(pc,i) ((void)0)
#endif /* LUA_DEBUG */


static void dischargeretpending1(CodeAnalyzer *ca, DFuncState *fs, int pc)
{
  if (ca->retpending.reg != -1) {
    lua_assert(ca->retpending.endpc != -1);
    set_ins_property(fs, pc, INS_PRERETURN1);
    ca->retpending.endpc = ca->retpending.reg = -1;
  }
  else {
    lua_assert(ca->retpending.endpc == -1);
  }
}


/*
** finds and marks the beginning of an open expression of type KIND; recursing
** into nested open expressions is not necessary, as only the start/end pc of
** root open expressions are needed by the second pass analyzer
*/
static void openexpr1(CodeAnalyzer *ca, DFuncState *fs, int firstreg, int kind)
{
  const Instruction *code = ca->code;
  int endpc = ca->pc--;
  int nextpossiblestart = endpc;
  int pc;
  for (pc = ca->pc; pc >= 0; pc--) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    switch (o) {
      case OP_CALL:
      case OP_CALL_I:
      case OP_CALL_I_R1:
      case OP_CALL_C:
      case OP_CALL_M:
      case OP_CONCAT:
        /* a nested end-of-expression code can clobber FIRSTREG, but it does not
           begin an open expression... unless the previous code is OP_LOADNIL
           and OP_LOADNIL clobbers a register less than FIRSTREG; if that is the
           case, do not mark OP_LOADNIL as the start of this expression;
           instead, mark whatever NEXTPOSSIBLESTART was, which is why it is
           still updated here */
        nextpossiblestart = pc;
        continue;
      default:
        break;
    }
    if (kind == SETLISTPREP && o == OP_NEWTABLE && a == firstreg)
      break;  /* found start of table */
    if (o == OP_MOVE)
      /* OP_MOVE in an open expression is not a store */
      newregnote(fs, REG_NOTE_NONRELOC, pc, GETARG_B(i));
    if (beginstempexpr(code, i, pc, firstreg, endpc, &nextpossiblestart)) {
      break;  /* found the beginning */
    }
    else if (o == OP_LOADNIL && a < firstreg) {
      /* found an OP_LOADNIL that clobbers an earlier register; mark
         NEXTPOSSIBLESTART as the start */
      pc = nextpossiblestart;
      break;
    }
    if (pc == 0)
      break;
  }
  ca->pc = pc;
  newopenexpr(fs, firstreg, pc, endpc, kind);
}


/*
** finds the earliest possible endpc for a SETLIST open expression with only
** hash items (the exact number cannot be known if operand C is graeter than 16
** due to the conversion with `luaO_fb2int')
*/
static void scanforhashitems1(CodeAnalyzer *ca, DFuncState *fs, OpenExpr *e,
                              int minhashsize, int firstreg)
{
  const Instruction *code = ca->code;
  int pc;  /* iterator */
  int numhashitems = 0;  /* number of hash items found so far */
  lua_assert(minhashsize > 0);
  lua_assert(e != NULL);
  lua_assert(e->kind == SETLISTPREP);
  lua_assert(ispcvalid(fs, e->startpc));
  lua_assert(GET_OPCODE(code[e->startpc]) == OP_NEWTABLE);
  lua_assert(GETARG_B(code[e->startpc]) == 0);
  for (pc = e->startpc+1; pc < fs->f->sizecode-1; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    if (o == OP_MOVE)
      newregnote(fs, REG_NOTE_NONRELOC, pc, GETARG_B(i));
    /* check if this opcode sets a table index */
    else if (o == OP_SETFIELD ||
             o == OP_SETFIELD_R1 ||
             o == OP_SETTABLE ||
             o == OP_SETTABLE_BK ||
             o == OP_SETTABLE_N ||
             o == OP_SETTABLE_N_BK ||
             o == OP_SETTABLE_S ||
             o == OP_SETTABLE_S_BK) {
      if (a == firstreg) {
        numhashitems++;
        if (numhashitems == minhashsize)
          break;  /* found earliest possible end */
      }
    }
  }
  e->endpc = pc;
}


#ifdef LUA_DEBUG
#define encountered1(what,pc) lprintf("  encountered a " what " loop starting" \
" at (%d)\n", (pc)+1)
#else /* !LUA_DEBUG */
#define encountered1(what,pc) ((void)0)
#endif /* LUA_DEBUG */


#define loop1(ca,fs,startpc,blocktype) do { \
  struct BasicBlock *new; /* the new block */ \
  /* create the new block */ \
  bbl1(ca, fs, startpc, blocktype, NULL, NULL, nextsibling); \
  new = fs->a->bbllist.first; \
  lua_assert(new != nextsibling); \
  /* in the case of an erroneous while-loop detection, whatever block is first \
     will already have the correct nextsibling */ \
  if (new->nextsibling == NULL) addsibling1(new, nextsibling); \
  nextsibling = new; \
  /* if enclosed by a block, save a previous sibling for it, as the block may \
     really begin after this newly created block */ \
  if (block != NULL && block->state.prevsibling == NULL) \
    block->state.prevsibling = new; \
  if (branch != NULL && branch->elseprevsibling == NULL) \
    branch->elseprevsibling = new; \
  if (branch != NULL && branch->potentialblock != NULL && \
      branch->potentialblock->state.prevsibling == NULL) \
    branch->potentialblock->state.prevsibling = new; \
} while(0)


struct block1;


/*
** `branch1' holds the context for an if-else branch construct in the first pass
*/
struct branch1 {
  struct branch1 *prev;  /* enclosing branch context if nested */
  struct branch1 *root;  /* the first context in this group of contexts, i.e.,
                            the first in its group to be recursed into, and the
                            last to be returned from; it is only used to handle
                            adjacent groups of nested `if-false' blocks */
  struct block1 *parentblock;  /* enclosing block if nested */
  struct block1 *potentialblock;  /* if the if-part closes variables */
  /*BasicBlock *next;*/  /* the next branch block in this group */
  BasicBlock *ifblock;  /* first if-part */
  BasicBlock *elseblock;  /* the else-part */
  BasicBlock *elseprevsibling;  /* the preceding sibling of the else-part */
  BasicBlock *firstblock;  /* the first block in the branch context, used when
                              no if-part is found when returning, meaning the
                              detection was erroneous and this block needs to be
                              accounted for in the parent call */
  /* `if_false_root' is used when handling erroneous else-branch detection, and
     it is the root if-false block in the current context's group, where a
     group is a hierarchy of nested if-false blocks. Because the root context
     returns first in this case, the parent exists before the child. When the
     leaf if-false-block in its group is created, all parent contexts have been
     unwound, and this handle is needed to access the root of the hierarchy */
  BasicBlock *if_false_root;
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
  int nocommit; /* this flag is used to tell the parent `bbl1' caller to delete
                   the basic block that it created before recursing into the
                   branch context */
  int correctingwhile;  /* true if unwinding the call stack to correct an
                           erroneous while-loop detection */
  int correctingwhilepc;
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
  BasicBlock *ifblock;  /* if this block is actually an if-statement */
  int nextbranchtarget;
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
  int correctingwhile;  /* true if unwinding the call stack to correct an
                           erroneous while-loop detection */
  int correctingwhilepc;
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
  lua_assert(block != NULL);
  if (chain == NULL) chain = &block->nextsibling;
  nextsibling = firstsibling = *chain;
  D(lprintf("fixing up sibling chain for %B\n", block));
  D(lprintf("--\n"));
  /* if the next sibling starts before this block ends OR the next sibling is
     empty and its startpc is 1 after the end of this block, make it a child of
     this block */
  while (nextsibling &&
         (nextsibling->startpc <= endpc ||
          (nextsibling->isempty && nextsibling->startpc-1 == endpc))) {
    lastchild = nextsibling;
    D(lprintf("found child %B\n", nextsibling)); 
    nextsibling = nextsibling->nextsibling;
  }
  D(lprintf("--\n"));
  D(lprintf("next sibling changed to %B\n", nextsibling));
  D(lprintf("previous next sibling was %B\n", block->nextsibling));
  *chain = nextsibling;
  if (lastchild) {
    block->firstchild = firstsibling;
    if (firstsibling) {
      D(lprintf("first child is %B\n", firstsibling)); 
    }
    /* fix sibling chain for last child */
    lastchild->nextsibling = NULL;
    D(lprintf("last child is %B\n", lastchild)); 
  }
  else {
    D(lprintf("found no children for this block\n"));
  }
  /* chain may be &block->nextsibling to avoid having to update any extra
     variables such as NEXTSIBLING in bbl1 */
  if (chain != &block->nextsibling) {
    lua_assert(block->nextsibling == NULL || block->nextsibling==firstsibling);
    block->nextsibling = nextsibling;
  }
  return lastchild;
}


static void newbranch1(DFuncState *fs, struct branch1 *branch, int midpc,
                       int endpc, int jumptarget, BasicBlock **nextsibling)
{
  BasicBlock *block;
  struct branch1 *prev = branch->prev;
  branch->startpc = -1;  /* startpc of the if-part is unknown right now */
  branch->midpc = midpc;
  branch->endpc = endpc;
  branch->target1 = -1;
  if (prev && endpc+1 == prev->midpc) {
    branch->optimalexit = prev->optimalexit;
    branch->root = prev->root;
  }
  else {
    branch->optimalexit = jumptarget;
    branch->root = branch; /* root points to itself */
  }
  block = addbbl1(fs, midpc, endpc, BBL_ELSE);
  if (prev != NULL && prev->elseprevsibling == NULL)
    prev->elseprevsibling = block;
  lua_assert(block != NULL);
  D(lprintf("ELSE BLOCK - (%i-%i)\n", midpc, endpc));
  fixsiblingchain1(block, nextsibling);
  branch->elseblock = block;
  branch->ifblock = NULL;
  branch->elseprevsibling = NULL;
  branch->firstblock = NULL;
  branch->contextswitched = 0;
  branch->nocommit = 0;
  branch->correctingwhile = 0;
  branch->if_false_root = NULL;
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
    D(lprintf("entering new basic block of type (%s)\n", bbltypename(type)));
  }
  else {
    struct stat1 *outersaved;
    if (branch != NULL) { /* inside a branch construct */
      lua_assert(block == NULL);
      lua_assert(branch->elseblock != NULL);
      nextsibling = branch->elseblock; /* the else-block */
      outersaved = branch->outer;
      D(lprintf("entering new branch\n"));
    }
    else { /* inside a block */
      lua_assert(block != NULL);
      nextsibling = NULL;
      outersaved = block->outer;
      D(lprintf("entering new do-block\n"));
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
    int bx = GETARG_Bx(i);
    int sbx = GETARG_sBx(i);
    printinsn1(pc,i);
    switch (o) {
      case OP_JMP: {
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
        CHECK(fs, ispcvalid(fs, target), "jump target pc is invalid");
        if (test_ins_property(fs, pc+1, INS_FORLIST)) {
          openexpr1(ca, fs, GETARG_A(code[target]), FORLISTPREP);
          goto poststat;
        }
        if (pc == 0) {
          ca->prevTMode = 0; /* unconditional jump */
          prevop = OP_MAX;
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
              D(lprintf("nextbranch = %B\n", nextbranch));
              D(lprintf("cleaning up testset expression from (%i-%i)\n",
                     pc, target-1));
              D(lprintf("--\n"));
              /* now, any and all blocks that were detected within this testset
                 expression need to be deleted, as they are all false */
              for (testsetpc = pc; testsetpc < target; testsetpc++) {
                /* delete all blocks which start on TESTSETPC */
                while (nextsibling && nextsibling->startpc == testsetpc) {
                  BasicBlock *nextnextsibling = nextsibling->nextsibling;
                  /* todo: what if the block endpc is after target? */
                  rembbl1(fs, nextsibling, NULL);
                  nextsibling = nextnextsibling; /* update NEXTSIBLING */
                }
                /* make sure none of these markings are on this pc */
                unset_ins_property(fs, testsetpc, INS_BRANCHFAIL);
                unset_ins_property(fs, testsetpc+1, INS_BRANCHBEGIN);
                unset_ins_property(fs, testsetpc, INS_BRANCHPASS);
                unset_ins_property(fs, testsetpc, INS_BLOCKEND);
              }
              D(lprintf("--\n"));
              /* also update NEXTBRANCH */
              if (nextsibling && nextsibling->type == BBL_IF) {
                nextbranch = nextsibling;
                lua_assert(nextbranch->startpc-1 >= 0 &&
                           nextbranch->startpc-1 < fs->f->sizecode);
                lua_assert(GET_OPCODE(code[nextbranch->startpc-1]) == OP_JMP);
                nextbranchtarget = nextbranch->startpc-1 + 1 +
                                   GETARG_sBx(code[nextbranch->startpc-1]);
              }
              else {
                nextbranch = NULL;
                nextbranchtarget = -1;
              }
              D(lprintf("nextbranch = %B\n", nextbranch));
              if (nextbranchtarget != -1)
                D(lprintf("nextbranchtarget = (%i)\n", nextbranchtarget));
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
              else { /* a repeat-loop fail-jump */
                if (type == BBL_REPEAT && target == startpc)
                  lua_assert(test_ins_property(fs, target, INS_REPEATSTAT));
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
                 enclosing loop, in which case there is (usually) a nested
                 while-loop, e.g.:
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
            lua_assert(type != BBL_FUNCTION); /* pc is already checked */
            /* the current loop must have a sibling for it to be an optimized
               exit */
            if (futuresibling != NULL) {
              /* get the pc that would be the jump - if the sibling is an ELSE
                 block, than use the jump out of the preceding IF block */
              int siblingstartpc = futuresibling->startpc -
                                   (futuresibling->type == BBL_ELSE);
              siblingjump = code[siblingstartpc];
              if (GET_OPCODE(siblingjump) == OP_JMP) {
                siblingtarget = siblingstartpc+1+GETARG_sBx(siblingjump);
                if (target == siblingtarget) { /* optimized jump? */
                  if (ca->prevTMode || (pc == startpc && type == BBL_WHILE))
                    /* loop-exit */
                    init_ins_property(fs, pc, INS_LOOPFAIL);
                  else /* break */
                    init_ins_property(fs, pc, INS_BREAKSTAT);
                  break;
                }
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
              BasicBlock *lastsibling;
              BasicBlock *newblock;
              D(lprintf("fixing erroneous while-true-loop detection\n"));
              correctwhile1:
              if (block != NULL) {
                D(lprintf("returning from block context before correcting "
                          "while-loop\n"));
                block->firstsibling = nextsibling;
                block->correctingwhilepc = pc;
                block->correctingwhile = 1;
                return;
              }
              if (branch != NULL) {
                D(lprintf("returning from branch context before correcting "
                          "while-loop\n"));
                branch->firstblock = nextsibling;
                if (branch->potentialblock != NULL)
                  branch->potentialblock->firstsibling = nextsibling;
                branch->correctingwhilepc = pc;
                branch->correctingwhile = 1;
                return;
              }
              lastsibling = nextsibling;
              D(lprintf("the while-loop thought to end at (%i) actually ends "
                        "at (%i) and is not nested\n", endpc, endpc+1));
              init_ins_property(fs, pc, INS_LOOPFAIL);
              /* this block is really `if false then ... end' or equivalent */
              D(lprintf("finding the last sibling in this context: "));
              if (lastsibling != NULL) {
                int newblocktype;
                while (lastsibling->nextsibling != NULL)
                  lastsibling = lastsibling->nextsibling;
                D(lprintf("%B\n", lastsibling));
                lua_assert(lastsibling != NULL);
                if (lastsibling->type == BBL_IF &&
                    lastsibling->endpc == endpc-1) {
                  lastsibling->endpc++; /* if-block includes the exit-jump */
                  newblocktype = BBL_ELSE;
                }
                else
                  newblocktype = BBL_IF;
                /* insert the new block in the middle of the chain, so that
                   NEXTSIBLING gets set correctly after returning */
                newblock = newbbl(fs->H, endpc+1, endpc, newblocktype);
                {
                  BasicBlock *nextinchain = lastsibling->next;
                  lastsibling->next = newblock;
                  lastsibling->nextsibling = newblock;
                  newblock->next = nextinchain;
                }
              }
              else { /* no preceding sibling blocks */
                D(lprintf("(NULL)\n")); /* no last sibling */
                newblock = addbbl1(fs, endpc+1, endpc, BBL_IF);
                nextsibling = newblock;
              }
              D(lprintf("created new block %B\n", newblock));
              D(lprintf("first block in chain is %B\n",
                        fs->a->bbllist.first));
              ca->testset.endpc = ca->testset.reg = -1;
              ca->curr = outer;
              /* check if there is OP_CLOSE before the `if-false' starts; this
                 would have been skipped over earlier due to being followed by
                 a jump, so it needs to handled now before returning -
                 the following code is handled here:
                    while a do
                        local a = 12;
                        do
                            local b = 34;
                            local function c()
                                b = b + 1;
                            end
                        end
                        if false then
                        end
                    end
                 */
              if (newblock->type == BBL_IF &&
                  (lastsibling == NULL || lastsibling->endpc < endpc-1) &&
                  GET_OPCODE(code[endpc-1]) == OP_CLOSE) {
                int closereg = GETARG_A(code[endpc-1]);
                int blockpc;
                int nextblockchildend;
                int wasbetweenchildren;
                BasicBlock *doblock;
                BasicBlock *blockchild = nextsibling;
                BasicBlock *blockprevsibling = NULL;
                nextblockchildend = blockchild ? blockchild->endpc : -1;
                CHECK(fs, closereg >= 0 && closereg < fs->f->maxstacksize,
                      "invalid register operand in OP_CLOSE");
                /* find the start of the block */
                for (blockpc = pc; blockpc < endpc-1; blockpc++) {
                  Instruction blockins = code[blockpc];
                  OpCode blockop = GET_OPCODE(blockins);
                  int blocka = GETARG_A(blockins);
                  int blockb = GETARG_B(blockins);
                  int blockc = GETARG_C(blockins);
                  /* update previous sibling for the block */
                  if (nextblockchildend+1 == blockpc) {
                    lua_assert(blockchild != NULL);
                    blockprevsibling = blockchild;
                    blockchild = blockchild->nextsibling;
                    nextblockchildend = blockchild ? blockchild->endpc : -1;
                  }
                  if (blocka == closereg &&
                      beginseval(blockop, blocka, blockb, blockc, 0)) {
                    break;
                  }
                }
                /* if the start wasn't found, start the block at PC */
                if (blockpc == endpc-1)
                  blockpc = pc;
                /* make sure the block doesn't start in the middle of a child */
                if (blockchild != NULL && blockpc > blockchild->startpc) {
                  blockpc = blockchild->startpc;
                  wasbetweenchildren = 0;
                }
                else
                  wasbetweenchildren = 1;
                /* `addbbl1' will put this block first in the chain which is ok
                   because it is the first child of the parent */
                if (blockprevsibling == NULL) {
                  doblock = addbbl1(fs, blockpc, endpc-1, BBL_DO);
                }
                else {
                  /* add the block into the chain explicitly to preserve the
                     first child of the parent */
                  BasicBlock *nextinchain = blockprevsibling->next;
                  doblock = newbbl(fs->H, blockpc, endpc-1, BBL_DO);
                  blockprevsibling->next = doblock;
                  blockprevsibling->nextsibling = doblock;
                  doblock->next = nextinchain;
                }
                if (wasbetweenchildren)
                  doblock->nextsibling = blockchild;
                else
                  doblock->firstchild = blockchild;
                set_ins_property(fs, blockpc, INS_DOSTAT);
                set_ins_property(fs, endpc-1, INS_BLOCKEND);
              }
              return;
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
                    /* NEXTBRANCH is used to check if this branch and the next
                       branch have the same target and endpc, in which case they
                       can be merged as one if-statement with multiple
                       conditions connected with `and' or `or' */
                    if (nextbranch != NULL) {
                      BasicBlock *next = nextbranch;
                      lua_assert(nextbranchtarget != -1);
                      if (next->endpc == branchendpc &&
                          nextbranchtarget == target) {
                        new_block = next;
                        D(lprintf("merging this branch with the next block "
                                  "%B\n", next));
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
                    D(lprintf("this branch is a child block of the top-level "
                             "one\n"));
                  }
                  new_block = addbbl1(fs, branchstartpc, branchendpc, BBL_IF);
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
                  D(lprintf("BRANCH BLOCK - %B\n", new_block));
                  if (issingle || branch == NULL ||
                      nextsibling != branch->elseblock)
                    fixsiblingchain1(new_block, &nextsibling);
                  else
                    /* connect the if and else parts, nothing else needed */
                    new_block->nextsibling = branch->elseblock;
                  nextsibling = new_block;
                  if (branch && !issingle) {
                    if (nextbranch == NULL) {
                      branch->ifblock = new_block;
                      branch->firstblock = new_block;
                      ca->testset.endpc = ca->testset.reg = -1;
                      /*dischargeretpending1(ca, fs, pc);*/
                      return;
                    }
                    else {
                      nextbranch = new_block;
                      nextbranchtarget = target;
                    }
                  }
                  /* check if this single if-statement has upvalues, and make
                     sure an extra block is not created with it */
                  else if (block != NULL && block->endpc == branchendpc) {
                    block->ifblock = new_block;
                    block->firstsibling = new_block;
                    return; /* pop the block context */
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
              struct block1 potentialblock;
              int noblock; /* true if the analyzer marked this branch as a
                              do-block and needs to correct */
              if (branch != NULL &&
                  (target == branch->optimalexit || target == branch->endpc+1))
                branchendpc = branch->midpc-1;
              else
                branchendpc = target-1;
              elsebranch:
              if (branch != NULL) {
                D(lprintf("nested else-branch ends at (%i), the enclosing "
                          "if-branch ends at (%i)\n", branchendpc,
                          branch->midpc-1));
              }
              /* something is wrong if this new else-branch ends after an
                 enclosing if-branch ends  */
              if (branch != NULL && branchendpc >= branch->midpc) {
                /* the problem: a new branch has been encountered and a context
                   for it must be created, but simultaneously, the enclosing
                   branch context is false and needs to be returned from */
                /* the idea: switch out the branch context for the enclosing
                   one, return from the enclosing context, fix the erroneous
                   block, and recurse into the new branch context */
                D(lprintf("existing branch-context is erroneous\n"));
                lua_assert(branch->contextswitched == 0);
                /* Check if this new branch ends in the middle of the parent
                   branch. Even if the parent branch is erroneous, it is still a
                   valid block and if this branch jumps to the middle of that
                   block, neither this context nor the parent context should be
                   committed as basic blocks. This addresses the following case:
                      local a = true or 5;
                      local b = false or 5;
                      local c = 1 or 5;
                   */
                if (branchendpc >= branch->midpc &&
                    branchendpc < branch->endpc) {
                  D(lprintf("avoiding impossible parent-child relationship "
                            "between this new context and the parent context -"
                            " the parent context (%i-%i) will be ignored\n",
                            branch->midpc, branch->endpc));
                  branch->nocommit = 1;
                  branch->firstblock = nextsibling;
                  return;
                }
                D(lprintf("performing context-switch and returning from context"
                          " before recursing\n"));
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
                D(lprintf("encountered an else-block which closes variables, "
                       "correcting erroneous do-block detection\n"));
                /* the OP_CLOSE at the end of this branch is not for a block */
                noblock = 1;
                /* need to close all the child blocks that are contained inside
                   the else-part as well */
                D(lprintf("childblock initialized to %B\n", childblock));
                for (; bl != NULL; bl = bl->prev) {
                  D(lprintf("i = (%d), bl = (%p)\n", i, cast(void *, bl)));
                  lastbl = bl;
                  if (bl->endpc < branchendpc) {
                    BasicBlock *currblock;
                    currblock = addbbl1(fs,bl->state.startpc,bl->endpc,BBL_DO);
                    lua_assert(currblock != NULL);
                    set_ins_property(fs, currblock->startpc, INS_DOSTAT);
                    set_ins_property(fs, currblock->endpc, INS_BLOCKEND);
                    D(lprintf("found an inner block %B\n", currblock));
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
                D(lprintf("nextsibling set to %B\n", nextsibling));
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
              /* if the if-part closes variables, keep track of the block
                 context in case there is no if-part */
              if (prevop == OP_CLOSE)
                new_branch.potentialblock = &potentialblock;
              else
                new_branch.potentialblock = NULL;
              newbranch1(fs, &new_branch, branchstartpc, branchendpc, target,
                         &nextsibling);
              bbl1(ca, fs, startpc, type, &new_branch, NULL, futuresibling);
              {
                BasicBlock *new_block;
                BasicBlock *ifblock = new_branch.ifblock;
                BasicBlock *elseblock = new_branch.elseblock;
                /*lua_assert(ifblock != NULL);*/
                lua_assert(elseblock != NULL);
                lua_assert(elseblock->type == BBL_ELSE);
                lua_assert(new_branch.midpc == elseblock->startpc);
                if (new_branch.nocommit) {
                  /* the elseblock that was created needs to be deleted -
                     promote all its children and update NEXTSIBLING */
                  BasicBlock *earliestblock =
                    rembbl1(fs, elseblock, new_branch.elseprevsibling);
                  /* update NEXTSIBLING to the earliest block so far */
                  lua_assert(new_branch.firstblock != NULL);
                  /* if FIRSTBLOCK was updated to something earlier than
                     ELSEBLOCK, than that is the earliest block */
                  if (new_branch.firstblock != elseblock)
                    earliestblock = new_branch.firstblock;
                  nextsibling = earliestblock;
                  elseblock = NULL;
                  new_block = NULL; /* no new block */
                }
                else if (ifblock != NULL) {
                  lua_assert(ifblock->type == BBL_IF);
                  lua_assert(new_branch.target1 != -1);
                  D(lprintf("ifblock = %B\n", ifblock));
                  /* make sure this if-block is really the sibling of the
                     else-block: the following must be true:
                     - the if-block ends right before the else-block starts
                     - the else-block is the immediate next sibling of the
                       if-block
                     - the jump-target out of the if-block condition is the
                       startpc of the else-block, however:
                       * there can be an exception to this, for example, if the
                         elseblock is empty and at the end of a while-loop, the
                         if-part will not fail-jump to the start of the
                         else-part; instead, it will jump to the start of the
                         loop, but this will never be the case here, because an
                         unconditional backward jump right before ENDPC that
                         jumps to the start of the loop is not detected as an
                         else-branch, but rather a nested while-loop, and a
                         correction gets made elsewhere if the jump really was
                         for an empty else-branch at the end of the while-loop
                     */
                  if (ifblock->endpc != new_branch.midpc-1 ||
                      ifblock->nextsibling != elseblock /*||
                      new_branch.target1 != elseblock->startpc*/) {
                    goto badelsebranch;
                  }
                  else
                    new_block = nextsibling = ifblock;
                }
                else {
                  BasicBlock *elseprevsibling;
                  badelsebranch:
                  /* ELSEBLOCK is not actually an else-block */
                  D(lprintf("handling erroneous else-branch detection %B\n",
                            elseblock));
                  D(lprintf("new_branch.potentialblock = %p\n",
                            new_branch.potentialblock));
                  /* correct the else-block that has already been created */
                  lua_assert(elseblock->startpc == pc+1);
                  D(lprintf("branchendpc = (%i)\n"
                            "branch->midpc = (%i)\n",
                            branchendpc, branch ? branch->midpc: -1));
                  if (branch != NULL && branchendpc+1 == branch->midpc &&
                      target == new_branch.optimalexit &&
                      new_branch.optimalexit > elseblock->endpc &&
                      branch->potentialblock == NULL) {
                    D(lprintf("using new_branch.optimalexit-1 (%i) as the endpc"
                              " instead of (%i)\n", new_branch.optimalexit-1,
                              elseblock->endpc));
                    elseblock->endpc = new_branch.optimalexit-1;
                  }
                  else {
                    if (branch != NULL && target == branch->midpc) {
                      elseblock->type = BBL_IF;
                      branch->ifblock = elseblock;
                      branch->firstblock = new_branch.firstblock;
                      branch->startpc = elseblock->startpc;
                      branch->target1 = target;
                      return;
                    }
                    /* disconnect this branch from the parent, since the
                       optimal exit is not being used as the endpc */
                    new_branch.root = &new_branch;
                  }
                  elseblock->type = BBL_IF;
                  /* if OP_CLOSE precedes this if-false block, create a block
                     for it using the `potentialblock' context */
                  if (new_branch.potentialblock != NULL &&
                      /* only create the block if there is no previous sibling
                         or if the previous sibling ends before the block would
                         end, because otherwise, the previous sibling already
                         closes the variables */
                      (new_branch.elseprevsibling == NULL ||
                       new_branch.elseprevsibling->endpc <
                       new_branch.potentialblock->endpc)) {
                    BasicBlock *doblock;
                    BasicBlock *child;
                    struct block1 *pblock = new_branch.potentialblock;
                    D(lprintf("creating do-block from the preceding OP_CLOSE of"
                              " the if-false-block\n"));
                    lua_assert(prevop == OP_CLOSE);
                    doblock = addbbl1(fs, pblock->state.startpc, pblock->endpc,
                                      BBL_DO);
                    lua_assert(doblock != NULL);
                    set_ins_property(fs, doblock->startpc, INS_DOSTAT);
                    set_ins_property(fs, doblock->endpc, INS_BLOCKEND);
                    if (pblock->state.firstchild != elseblock)
                      doblock->firstchild = pblock->state.firstchild;
                    D(lprintf("doblock->firstchild is %B\n",
                              doblock->firstchild));
                    D(lprintf("doblock->nextsibling is %B\n", elseblock));
                    doblock->nextsibling = elseblock;
                    D(lprintf("clearing new_branch.elseprevsibling, was %B\n",
                              new_branch.elseprevsibling));
                    new_branch.elseprevsibling = NULL;
                    if (pblock->state.prevsibling) {
                      D(lprintf("connecting previous doblock sibling %B\n",
                                pblock->state.prevsibling));
                      pblock->state.prevsibling->nextsibling = doblock;
                    }
                    child = doblock->firstchild;
                    if (child != NULL) {
                      while (child->nextsibling != NULL &&
                             child->nextsibling != elseblock)
                        child = child->nextsibling;
                      D(lprintf("doblock last child is %B\n"
                                "  clearing last child next sibling, was %B\n",
                                child, child->nextsibling));
                      child->nextsibling = NULL;
                    }
                    /* make sure firstblock is actually the earliest block */
                    if (pblock->state.prevsibling == NULL) {
                      new_branch.firstblock = doblock;
                    }
                    else {
                      lua_assert(new_branch.firstblock != elseblock);
                    }
                    D(lprintf("new_branch.firstblock = %B\n",
                              new_branch.firstblock));
                  }
                  /* the if-block endpc may have defaulted to the pc just before
                     the original startpc of the elseblock, but now the
                     elseblock startpc has changed, and the if-block endpc needs
                     to be adjusted in case it overlaps with the elseblock */
                  if (ifblock != NULL && ifblock->endpc+1 >= elseblock->startpc)
                  {
                    ifblock->endpc = elseblock->startpc-2;
                    ifblock->isempty = (ifblock->startpc > ifblock->endpc);
                  }
                  D(lprintf("else-block corrected to %B\n", elseblock));
                  D(lprintf("elseblock->nextsibling = %B\n",
                            elseblock->nextsibling));
                  fixsiblingchain1(elseblock, NULL);
                  unset_ins_property(fs, pc, INS_BLOCKEND);
                  elseprevsibling = new_branch.elseprevsibling;
                  D(lprintf("elseprevsibling = %B\n", elseprevsibling));
                  if (elseprevsibling != NULL) {
                    /* make sure the sibling relation exists */
                    elseprevsibling->nextsibling = elseblock;
                    if (elseprevsibling->type == BBL_ELSE &&
                        elseprevsibling->endpc == pc) {
                      D(lprintf("changing elseprevsibling->endpc from (%i) to"
                                " (%i)\n", pc, branchendpc));
                      elseprevsibling->endpc = branchendpc;
                    }
                    fixsiblingchain1(elseprevsibling, NULL);
                    /*lua_assert(new_branch.elseprevsibling->nextsibling ==
                               elseblock);*/
                    /* the analyzer thought ELSEBLOCK was an else-block that
                       started after the jump at PC, but really it starts ON the
                       jump at PC, and said jump is a break (maybe?), so the
                       previous sibling of ELSEBLOCK can't contain the jump, as
                       it should be inside to ELSEBLOCK */
                    /*lua_assert(elseprevsibling->endpc+1 < elseblock->startpc);
                    */
                  }
                  /* this is the new block resulting from the branch context */
                  new_block = elseblock;
                  /* because parent contexts return first in the case of nested
                     erroneous branch contexts, the root basic block in the
                     group nesting needs to be forwarded to parent contexts,
                     which actually represent child blocks, for example, given
                     the source code:
                        if false then
                            ...
                            if false then
                                ...
                                if false then
                                    ...
                                end
                            end
                        end
                     the context for the inner block is pushed first and returns
                     last, and within a given context, the BRANCH variable
                     points to the immediate child block. This creates a problem
                     when there are multiple, independent, adjcent nestings of
                     if-false blocks, such as:
                        if false then
                            ...
                            if false then
                                ...
                                if false then
                                    ...
                                end
                            end
                        end
                        local a = 12;
                        if false then
                            ...
                            if false then
                                ...
                                if false then
                                    ...
                                end
                            end
                        end
                     There are six blocks and six branch contexts here, and they
                     are pushed from last to first, and return first to last.
                     When the context for the first block returns, it will save
                     the basic block that it creates as an `if_false_root' for
                     its group. It will then `forward' this root block to the
                     parent (again, which actually represents the child), as
                     long as the parent context and this context share the same
                     root context (meaning they are in the same group). This
                     way, `elseprevsibling' will be updated properly for the
                     parent context - if the parent is part of a different
                     group, its `elseprevsibling' will point to the
                     `if_false_root' block so as to avoid attaching the parent
                     as a sibling to the innermost child the preceding group;
                     instead, it will be attached as a sibling of the root
                     block of the previous group */
                  /* set the root BasicBlock of this group */
                  if (new_branch.if_false_root == NULL) {
                    D(lprintf("setting if_false_root for this group to %B\n",
                              new_block));
                    new_branch.if_false_root = new_block;
                  }
                  /* forward `if_false_root' to the parent if it is part of the
                     same group */
                  if (branch != NULL && branch->root == new_branch.root) {
                    D(lprintf("forwarding if_false_root %B to parent branch "
                              "context\n", new_branch.if_false_root));
                    branch->if_false_root = new_branch.if_false_root;
                  }
                  /* otherwise, correct `elseprevsibling' to point to the root
                     block of the previous group instead of the leaf */
                  else if (branch != NULL) {
                    D(lprintf("comparing nextsibling to branch->elseblock\n"
                              "  nextsibling = %B\n"
                              "  branch->elseblock = %B\n",
                              nextsibling, branch->elseblock));
                    if (branch->elseblock == nextsibling) {
                      branch->elseprevsibling = new_branch.if_false_root;
                    }
                    else {
                      D(lprintf("setting new_branch.if_false_root->nextsibling "
                              "to %B\n", nextsibling));
                      new_branch.if_false_root->nextsibling = nextsibling;
                      branch->elseprevsibling = NULL;
                    }
                  }
                  else {
                    /* make sure the root block gets assigned the next sibling
                       before updating NEXTSIBLING */
                    D(lprintf("if_false_root = %B\n",new_branch.if_false_root));
                    D(lprintf("setting nextsibling for if_false_root to %B\n",
                              nextsibling));
                    new_branch.if_false_root->nextsibling = nextsibling;
                  }
                  /* firstblock should never be NULL as it gets set to the value
                     of NEXTSIBLING when returning, and NEXTSIBLING is
                     intialized to the else-part (ELSEBLOCK when recursing into
                     a branch context) */
                  lua_assert(new_branch.firstblock != NULL);
                  D(lprintf("new_branch.firstblock = %B\n",
                            new_branch.firstblock));
                  /* set NEXTSIBLING to the earliest block; if
                     new_branch.firstblock is not new_branch.if_false_root,
                     than the former must come earlier than the latter */
                  if (new_branch.firstblock != new_branch.if_false_root)
                    nextsibling = new_branch.firstblock;
                  else
                    nextsibling = new_branch.if_false_root;
                  D(lprintf("updated nextsibling to %B\n", nextsibling));
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
                    nextsibling->nextsibling = new_branch.if_false_root;
                    /* NEXTSIBLING is the outer block that needs to be made the
                       parent. NEW_BLOCK is the inner block that is currently
                       the next sibling, but needs to be made the child. Use a
                       dummy variable so `fixsiblingchain' doesn't change these
                       variables */
                    fixsiblingchain1(nextsibling, NULL);
                  }
                  if (new_branch.contextswitched) {
                    D(lprintf("handling else-branch context-switch for pc "
                              "(%i)\n", ca->pc));
                    pc = ca->pc;
                    branchendpc = new_branch.endpc;
                    target = pc + 1 + GETARG_sBx(code[pc]);
                    D(lprintf("update variables before recursing with new "
                              "context:\n"
                              "  pc = (%i)\n"
                              "  branchendpc = (%i)\n"
                              "  target = (%i)\n"
                              "  nextsibling = %B\n",
                              pc, branchendpc, target, nextsibling));
                    nextbranch = NULL;
                    nextbranchtarget = -1;
                    if (block != NULL) {
                      block->state = block->possiblestate;
                    }
                    goto elsebranch;
                  }
                }
                D(lprintf("exited block %B\n", new_block));
                lua_assert(new_block != NULL || new_branch.nocommit);
                if (new_block == ifblock) {
                  /* this is ok even if NEW_BLOCK and IFBLOCK are both NULL */
                  nextbranch = ifblock;
                  D(lprintf("setting nextbranch to %B\n", ifblock));
                  nextbranchtarget = new_branch.target1;
                  D(lprintf("setting nextbranchtarget to (%i)\n",
                            nextbranchtarget));
                }
                else {
                  nextbranch = NULL;
                  nextbranchtarget = -1;
                  if (block != NULL) {
                    block->state = block->possiblestate;
                    if (new_branch.nocommit) {
                      if (block->state.prevsibling == NULL &&
                          nextsibling != elseblock->nextsibling) {
                        BasicBlock *prevsibling = nextsibling;
                        BasicBlock *elsenextsibling = elseblock->nextsibling;
                        lua_assert(prevsibling != NULL);
                        while (prevsibling->nextsibling != NULL &&
                               prevsibling->nextsibling != elsenextsibling) {
                          prevsibling = prevsibling->nextsibling;
                        }
                        block->state.prevsibling = prevsibling;
                      }
                      else {
                        lua_assert(block->state.prevsibling ==
                                   elseblock->nextsibling);
                      }
                    }
                    else if (block->state.startpc <= branchendpc) {
                      /* make sure the block starts before the if-block */
                      D(lprintf("block->state.startpc = (%i)\n",
                                block->state.startpc));
                      D(lprintf("new_branch.elseprevsibling = %B\n",
                                new_branch.elseprevsibling));
                      if (new_branch.elseprevsibling)
                      D(lprintf("elseprevsibling->nextsibling = %B\n",
                                new_branch.elseprevsibling->nextsibling));
                      D(lprintf("nextsibling = %B\n", nextsibling));
                      {
                        BasicBlock *sibling = nextsibling;
                        BasicBlock *if_false_root = new_branch.if_false_root;
                        lua_assert(sibling != NULL);
                        while (sibling->nextsibling != if_false_root &&
                               sibling->nextsibling != NULL)
                          sibling = sibling->nextsibling;
                        D(lprintf("sibling = %B\n", sibling));
                        if (block->state.startpc > if_false_root->startpc-1 ||
                            (sibling != NULL &&
                             block->state.startpc > sibling->startpc-1)) {
                          block->state.startpc =
                            new_branch.if_false_root->startpc-1;
                        }
                      }
                      block->state.firstchild = new_branch.if_false_root;
                      D(lprintf("comparing nextsibling %B to if_false_root "
                                "%B\n", nextsibling, new_branch.if_false_root));
                      if (nextsibling != new_branch.if_false_root) {
                        /* find the previous sibling of if_false_root */
                        BasicBlock *prevsibling = nextsibling;
                        BasicBlock *if_false_root = new_branch.if_false_root;
                        lua_assert(prevsibling != NULL);
                        while (prevsibling->nextsibling != if_false_root &&
                               prevsibling->nextsibling != NULL) {
                          prevsibling = prevsibling->nextsibling;
                        }
                        D(lprintf("setting block->state.prevsibling to %B\n",
                                  prevsibling));
                        block->state.prevsibling = prevsibling;
                      }
                      else
                        block->state.prevsibling = NULL;
                    }
                    else {
                      if (block->state.prevsibling == NULL)
                        block->state.prevsibling = nextsibling;
                    }
                  }
                }
                if (noblock) { /* close the erroneous block and pass data */
                  lua_assert(block != NULL);
                  if (new_block == ifblock) {
                    block->nextbranch = nextbranch;
                    block->nextbranchtarget = nextbranchtarget;
                  }
                  else {
                    block->ifblock = new_branch.if_false_root;
                  }
                  block->firstsibling = nextsibling;
                  block->correctingwhile = new_branch.correctingwhile;
                  block->correctingwhilepc = new_branch.correctingwhilepc;
                  return; /* return from the do-block context */
                }
                if (new_branch.correctingwhile) {
                  goto correctwhile1;
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
      case OP_FORLOOP: { /* a numeric for-loop */
        int target = pc + 1 + sbx; /* start of for-loop */
        init_ins_property(fs, target, INS_FORNUM);
        init_ins_property(fs, pc, INS_LOOPEND);
        loop1(ca, fs, target, BBL_FORNUM);
        break;
      }
      case OP_CLOSE: /* check for a block-ending if the next op is not a jump */
        if (branch != NULL && pc+2 == branch->midpc &&
            branch->potentialblock != NULL) {
          /* initialize the potential block context for the branch context */
          struct block1 *pblock = branch->potentialblock;
          pblock->prev = branch->parentblock;
          pblock->ifblock = NULL;
          pblock->nextbranch = NULL;
          pblock->nextbranchtarget = -1;
          pblock->nextsibling = nextsibling;
          pblock->firstsibling = NULL;
          pblock->state.prevsibling = NULL;
          pblock->state.firstchild = NULL;
          pblock->possiblestate = pblock->state;
          pblock->pass = 0;
          pblock->outer = &outer; /* keep the same loop context */
          pblock->state.startpc = pc;
          pblock->endpc = pc;
          pblock->reg = a;
        }
        else if (GET_OPCODE(code[pc+1]) != OP_JMP) {
          /* when multiple, possibly nested OP_CLOSE are encountered, it is
             important to compare the argA of each. A nested block would not
             close up to register <= to that of its enclosing block. So, if
             there is a previous OP_CLOSE context, check its register. If it is
             greater than or equal to this A, it cannot logically be nested. */
          if (block != NULL && block->reg >= a) {
            /* this block is adjacent to the previous, switch out the context
               instead of pushing a new one */
            BasicBlock *new_block;
            D(lprintf("encountered adjacent do-block\n"));
            new_block = addbbl1(fs, block->state.startpc, block->endpc, BBL_DO);
            lua_assert(new_block != NULL);
            set_ins_property(fs, new_block->startpc, INS_DOSTAT);
            set_ins_property(fs, new_block->endpc, INS_BLOCKEND);
            new_block->firstchild = block->state.firstchild;
            D(lprintf("setting first child for block to %B\n",
                      block->state.firstchild));
            new_block->nextsibling = block->nextsibling;
            D(lprintf("setting next sibling for block to %B\n",
                      block->nextsibling));
            /* link the last sibling before the adjacent block to it */
            if (block->state.prevsibling) {
              block->state.prevsibling->nextsibling = new_block;
              D(lprintf("connecting previous sibling %B\n",
                        block->state.prevsibling));
            }
            else
              nextsibling = new_block;
            /* re-initialize the context and continue without recursing */
            block->state.prevsibling = NULL;
            block->nextsibling = nextsibling;
            block->firstsibling = NULL;
            block->state.firstchild = NULL;
            block->ifblock = NULL;
            block->nextbranch = NULL;
            block->nextbranchtarget = -1;
            block->pass = 0;
            block->correctingwhile = 0;
            block->state.startpc = pc;
            block->endpc = pc;
            block->reg = a;
            nextsibling = NULL; /* set to NULL as if recursing */
          }
          else {
            struct block1 new_block;
            new_block.prev = block;
            new_block.ifblock = NULL;
            new_block.nextbranch = NULL;
            new_block.nextbranchtarget = -1;
            new_block.nextsibling = nextsibling;
            new_block.firstsibling = NULL;
            new_block.state.prevsibling = NULL;
            new_block.state.firstchild = NULL;
            new_block.pass = 0;
            new_block.correctingwhile = 0;
            new_block.outer = &outer; /* keep the same loop context */
            new_block.state.startpc = pc;
            new_block.endpc = pc;
            new_block.reg = a;
            bbl1(ca, fs, startpc, type, NULL, &new_block, futuresibling);
            D(lprintf("new_block.prevsibling = %B\n",
                      new_block.state.prevsibling));
            /* check if the block was inside an else-branch */
            if (new_block.nextbranch != NULL) {
              nextbranch = new_block.nextbranch;
              nextbranchtarget = new_block.nextbranchtarget;
              nextsibling = new_block.firstsibling;
              /* check if the parent was also inside the else-branch */
              if (new_block.pass) { /* also pass this data to the parent */
                lua_assert(block != NULL);
                block->nextbranch = nextbranch;
                block->nextbranchtarget = nextbranchtarget;
                block->firstsibling = nextsibling;
                return; /* return for each block that was in the else-branch */
              }
            }
            else if (new_block.ifblock != NULL) {
              BasicBlock *ifblock = new_block.ifblock;
              D(lprintf("nextsibling = %B\n", new_block.nextsibling));
              ifblock->nextsibling = new_block.nextsibling;
              nextsibling = new_block.firstsibling;
              if (block != NULL && block->state.prevsibling == NULL)
                block->state.prevsibling = ifblock;
              if (branch != NULL && branch->elseprevsibling == NULL)
                branch->elseprevsibling = ifblock;
              if (new_block.pass) { /* also pass this data to the parent */
                lua_assert(block != NULL);
                block->ifblock = ifblock;
                block->firstsibling = nextsibling;
                return; /* return for each block that was in the else-branch */
              }
              break; /* the block was actually an if-statement */
            }
            else { /* create a new basic block entry */
              BasicBlock *doblock;
              lua_assert(new_block.pass == 0);
              doblock = addbbl1(fs, new_block.state.startpc, new_block.endpc,
                                BBL_DO);
              lua_assert(doblock != NULL);
              set_ins_property(fs, doblock->startpc, INS_DOSTAT);
              set_ins_property(fs, doblock->endpc, INS_BLOCKEND);
              doblock->firstchild = new_block.state.firstchild;
              doblock->nextsibling = new_block.nextsibling;
              if (new_block.state.prevsibling) {
                D(lprintf("connecting previous sibling %B\n",
                          new_block.state.prevsibling));
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
              if (new_block.correctingwhile) {
                pc = new_block.correctingwhilepc;
                D(lprintf("jumping back to while-loop correction\n"
                          "  nextsibling = %B\n"
                          "  pc = (%i)\n", nextsibling, pc));
                goto correctwhile1;
              }
            }
          }
        }
        break;
      case OP_CALL:
      case OP_TAILCALL:
      case OP_CALL_I:
      case OP_CALL_I_R1:
      case OP_CALL_C:
      case OP_CALL_M:
      case OP_TAILCALL_I:
      case OP_TAILCALL_I_R1:
      case OP_TAILCALL_C:
      case OP_TAILCALL_M:  /* a function call expression */
        openexpr1(ca, fs, a, CALLPREP);
        goto poststat;
      case OP_CONCAT:  /* a concat expression */
        openexpr1(ca, fs, b, CONCATPREP);
        goto poststat;
      case OP_RETURN: {  /* a return expression */
        int nret = b-1;
        /*dischargeretpending1(ca, fs, pc);*/
        /*if (nret == 1) {
          ca->retpending.reg = a;
          ca->retpending.endpc = pc;
        }
        else*/
        if (nret == 1) {  /* returns a single register */
          if (pc > 0)
            /* the second pass is interested in knowing when it is about to
               encounter a single-return */
            set_ins_property(fs, pc-1, INS_PRERETURN1);
        }
        else if (nret != 0) {  /* returns an open expression */
          openexpr1(ca, fs, a, RETPREP);
        }
        goto poststat;
      }
      case OP_FORPREP: {  /* mark the start of preparation code */
        int target = pc + 1 + sbx; /* end of for-loop */
        CHECK(fs, ispcvalid(fs, target), "invalid target pc in OP_FORPREP");
        CHECK(fs, GET_OPCODE(code[target]) == OP_FORLOOP, "unexpected target "
              "code for OP_FORPREP (expected to jump to OP_FORLOOP)");
        lua_assert(test_ins_property(fs, target, INS_LOOPEND));
        openexpr1(ca, fs, a, FORNUMPREP);
        goto poststat;
      }
      case OP_SETLIST:
        openexpr1(ca, fs, a, SETLISTPREP);
        goto poststat;
      case OP_TESTSET: /* a testset expression */
        if (ca->testset.reg == -1)
          ca->testset.reg = b;
        else {
          if (ca->testset.reg != b) {
            /* bad code */
          }
        }
        updatefirstclob1(fs, pc, a);
        break;
      case OP_CLOSURE: {
        int nup, nupn;
        lua_assert(bx >= 0 && bx < fs->f->sizep);
        nupn = nup = fs->f->p[bx]->nups;
        lua_assert(ispcvalid(fs, pc+nupn));
        for (; nupn > 0; nupn--) {
          Instruction data = code[pc+nupn];
          int upvalindex = GETARG_Bx(data);
          lua_assert(GET_OPCODE(data) == OP_DATA);
          if (GETARG_A(data) == 1) {
            CHECK(fs, isregvalid(fs, upvalindex), "OP_DATA indexes invalid "
                  "register");
            newregnote(fs, REG_NOTE_UPVALUE, pc+nupn, upvalindex);
          }
          else {
            CHECK(fs, upvalindex >= 0 && upvalindex < fs->f->nups, "OP_DATA "
                  "indexes invalid upvalue");
          }
        }
        break;
      }
      case OP_DATA:
        break;
      case OP_NEWTABLE:
        /* table constructors with only hash-items will not emit OP_SETLIST, nor
           empty table constructors, so they are detected here */
        CHECK(fs, b == 0, "no OP_SETLIST found for OP_NEWTABLE even though "
                "OP_NEWTABLE has non-zero amount of list items");
        if (c)
          /* walk back up the code vector to find the earliest possible end of
             this constructor */
          scanforhashitems1(ca, fs, newopenexpr(fs, a, pc, -1, SETLISTPREP),
                            luaO_fb2int(c-1)+1, a);
        else
          newopenexpr(fs, a, pc, pc, EMPTYTABLE);
        /* fallthrough */
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
          updatefirstclob1(fs, pc, a);
          if (branch != NULL && branch->parentblock != NULL) {
            bl = branch->parentblock;
            blstate = &bl->possiblestate;
            stateunsure = 1;
            goto updateblockstate1;
          }
          else if (branch != NULL && branch->potentialblock != NULL) {
            bl = branch->potentialblock;
            blstate = &bl->state;
            stateunsure = (bl->prev == branch->parentblock);
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
                D(lprintf("%supdating .firstclob for R(%d) to (%i), %d block%s "
                          "removed\n", stateunsure ? "(possibly) " : "",bl->reg,
                          pc, i, i == 1 ? "" : "s"));

              }
              i++;
            }
          }
        }
        /*if (ca->retpending.reg != -1 &&
            beginstempexpr(code,i,pc,ca->retpending.reg,ca->retpending.endpc,
                           NULL)) {
          dischargeretpending1(ca, fs, pc);
        }*/
        (void)dischargeretpending1;
        break;
    }
    if (ca->pc == startpc)
      break;
  }
  /* make sure branch-blocks are corrected if their relationship doesn't make
     sense; NEXTBRANCH should have an false-exit that jumps to the start of the
     else-block */
  if (nextbranch != NULL && nextbranch->startpc != 0 &&
      nextbranch->nextsibling != NULL &&
      nextbranch->nextsibling->type == BBL_ELSE) {
    Instruction jmp;
    int jmpval;
    int jmptarget;
    BasicBlock *nextif = nextbranch;
    BasicBlock *nextelse = nextbranch->nextsibling;
    lua_assert(nextif->type == BBL_IF);
    lua_assert(nextelse->type == BBL_ELSE);
    jmp = code[nextif->startpc-1];
    lua_assert(GET_OPCODE(jmp) == OP_JMP);
    jmpval = GETARG_sBx(jmp);
    jmptarget = nextif->startpc + jmpval;
    if (jmptarget != nextelse->startpc) {
      BasicBlock *bbl = nextif->firstchild;
      nextelse->type = BBL_IF;
      nextif->endpc = nextelse->endpc;
      if (bbl != NULL) {
        while (bbl->nextsibling != NULL)
          bbl = bbl->nextsibling;
        bbl->nextsibling = nextelse;
      }
      else
        nextif->firstchild = nextelse;
      nextif->nextsibling = nextelse->nextsibling;
    }
  }
  ca->testset.endpc = ca->testset.reg = -1;
  /*dischargeretpending1(ca, fs, ca->pc);*/
  if (branch == NULL && block == NULL) {
    ca->curr = outer; /* pop old values */
  }
  else {
    if (branch != NULL) {
      D(lprintf("forced to return from branch without finding an if-part\n"));
      branch->firstblock = nextsibling;
      if (branch->potentialblock != NULL)
        branch->potentialblock->firstsibling = nextsibling;
      /*lua_assert(branch->next != NULL && branch->next->type == BBL_IF)*/;
    }
    else {
      D(lprintf("setting block->firstsibling to %B\n", nextsibling));
      block->firstsibling = nextsibling;
    }
    return;
  }
  /* create a new basic block node */
  /* `nextsibling' will end up being the first child of this new block */
  addbbl1(fs, startpc, endpc, type)->firstchild = nextsibling;
  D(lprintf("ca->pc = (%d)\n", ca->pc));
}


/*
** The code analyzer works in the first pass and detects the beginnings and ends
** of loops (and determines the type of each loop), the ends of blocks, branch
** jumps, break jumps, and the beginnings of for-loop control variable
** evaluations. It walks through the code backwards, which makes differentiating
** branch tests from loop tests a simpler task.
*/
static void pass1(const Proto *f, DFuncState *fs)
{
  CodeAnalyzer ca;
  ca.curr.start = ca.curr.end = ca.curr.type = -1;
  ca.pc = f->sizecode - 1; /* will be decremented again to skip final return */
  ca.prevTMode = 0;
  ca.testset.endpc = ca.testset.reg = -1;
  ca.retpending.endpc = ca.retpending.reg = -1;
  ca.code = f->code;
  bbl1(&ca, fs, 0, BBL_FUNCTION, NULL, NULL, NULL);
  lua_assert(fs->a->bbllist.first != NULL);
  lua_assert(fs->a->bbllist.first->nextsibling == NULL);
  lua_assert(fs->a->bbllist.first->type == BBL_FUNCTION);
  if (f->sizecode > 1)
    set_ins_property(fs, f->sizecode-2, INS_BLOCKEND);
  D(lprintf("ca.pc == (%d)\n", ca.pc));
  luaM_reallocvector(fs->H, fs->a->opencalls, fs->a->sizeopencalls,
                     fs->nopencalls, OpenExpr);
  fs->a->sizeopencalls = fs->nopencalls;
  luaM_reallocvector(fs->H, fs->a->regnotes, fs->a->sizeregnotes, fs->nregnotes,
                     RegNote);
  fs->a->sizeregnotes = fs->nregnotes;
  lua_assert(ca.pc <= 0);
  lua_assert(ca.testset.endpc == -1 && ca.testset.reg == -1);
  lua_assert(ca.retpending.endpc == -1 && ca.retpending.reg == -1);
}


typedef struct StackAnalyzer {
  const Instruction *code;
  BasicBlock *currbbl;
  int laststore;  /* exp index of last store node */
  int pc;
  int sizecode;
  int maxstacksize;
  int nextpclimit;  /* the next PC that causes the current block to change */
  int openexprnildebt;  /* number of registers that contain nil values, but have
                           no actual expression nodes for them because they
                           share a previous OP_LOADNIL code that is before the
                           start of the open expression; if non-zero, the first
                           register to load nils into is NEXTOPENREG */
  int openexprkind;
  OpenExpr *nextopenexpr;  /* next open expression */
  int nextopenreg;  /* first register of next open expression */
  int lastexpindex;
  lu_byte intailemptyblock;
  lu_byte inheadercondition;  /* evaluating the condition of a while-loop or 
                                  an if-statement */
} StackAnalyzer;


static void DecompileFunction(DecompState *D, const Proto *f);


#ifdef HKSC_DECOMP_HAVE_PASS2


/*
** `varstartsatpc2' returns how many variables start at the given PC (if debug
** info is not being used, the return value is always -1)
*/
static int varstartsatpc2(DFuncState *fs, int pc)
{
  DecompState *D = fs->D;
  struct LocVar *var;
  int i = fs->nlocvarsdetected;
  int n = 0;
  lua_assert(ispcvalid(fs, pc));
  if (D->usedebuginfo == 0)
    return -1;
  while (i < fs->sizelocvars && (var = &fs->locvars[i])->startpc == pc) {
    i = ++fs->nlocvarsdetected;
    n++;
    D(lprintf("variable '%s' begins at (%i)\n", getstr(var->varname), pc));
  }
  return n;
}


static LocVar *addlocvar2(DFuncState *fs, int startpc, int endpc, int param)
{
  struct LocVar *var;
  int i = fs->nlocvars++;
  lua_assert(i >= 0 && i < fs->sizelocvars);
  var = &fs->locvars[i];
  if (fs->D->usedebuginfo) { /* variable information already exists */
    lua_assert(ispcvalid(fs, var->startpc));
    lua_assert(ispcvalid(fs, var->endpc));
    lua_assert(var->varname != NULL);
    if (param) {
      lua_assert(var->startpc == startpc);
      lua_assert(var->endpc == endpc);
    }
    lua_assert(fs->nlocvars <= fs->nlocvarsdetected);
  }
  else { /* generate a variable name */
    char buff[sizeof("f_local") + (2 *INT_CHAR_MAX_DEC)];
    const char *fmt = param ? "f%d_arg%d" : "f%d_local%d";
    var->startpc = startpc;
    var->endpc = endpc;
    sprintf(buff, fmt, fs->idx, i);
    var->varname = luaS_new(fs->H, buff);
  }
  D(lprintf("added new %s named '%s'\n", getstr(var->varname)));
  return var;
}


static void addlocvar2reg2(DFuncState *fs, int startpc, int endpc, int param,
                           int reg)
{
  struct LocVar *var = addlocvar2(fs, startpc, endpc, param);
  lua_assert(isregvalid(fs, reg));
  /* only REG_PENDING should be set */
  lua_assert(test_reg_property(fs, reg, REG_PENDING) ||
             getslotdesc(fs, reg)->flags == 0);
  unset_reg_property(fs, reg, REG_PENDING);
  set_reg_property(fs, reg, REG_LOCAL);
  getslotdesc(fs, reg)->u.locvar = var;  /* REG holds VAR */
}


/*
** find the initial first free register for a function and initialize register
** flags for the non-free registers
*/
static void initfirstfree2(DFuncState *fs, const Proto *f)
{
  int i;
  int firstfree;
  int firstclobnonparam = fs->firstclobnonparam;
  lua_assert(firstclobnonparam == -1 || ispcvalid(fs, firstclobnonparam));
  /* in regular Lua, initial OP_LOADNILs are removed, so the real first free
     register is whatever is clobbered first after the parameters, or the
     number of parameters by default */
  if (firstclobnonparam != -1)
    firstfree = GETARG_A(f->code[firstclobnonparam]);
  else
    firstfree = f->numparams;
  /* should be CHECKed earlier */
  lua_assert(firstfree <= f->maxstacksize);
  lua_assert(firstfree >= f->numparams);
  /* set these explicitly to avoid a debug message for each set */
#ifdef LUA_DEBUG
  lprintf("marking %d parameter%s as %R\n", f->numparams, 
            firstfree == 1 ? "" : "s", REG_LOCAL);
  if (firstfree > f->numparams) {
    int numlocvars = firstfree - f->numparams;
    lprintf("marking %d more local variable%s as %R\n", numlocvars,
            numlocvars == 1 ? "" : "s", REG_LOCAL);
  }
#endif /* LUA_DEBUG */
  for (i = 0; i < f->numparams; i++)
    addlocvar2reg2(fs, 0, f->sizecode-1, 1, i); /* add param */
  for (; i < firstfree; i++)
    addlocvar2reg2(fs, 0, f->sizecode-1, 0, i); /* add local variable */
  fs->firstfree = firstfree;
}


static void updatenextopenexpr2(StackAnalyzer *sa, DFuncState *fs)
{
  OpenExpr *next;
  lua_assert(fs->nopencalls >= 0);
  if (fs->nopencalls == 0) {
    sa->nextopenexpr = NULL;
    sa->nextopenreg = -1;
    return;
  }
  next = &fs->a->opencalls[--fs->nopencalls];
  sa->nextopenexpr = next;
  sa->nextopenreg = next->firstreg;
  lua_assert(ispcvalid(fs, sa->nextopenexpr->startpc));
  lua_assert(ispcvalid(fs, sa->nextopenexpr->endpc));
  lua_assert(isregvalid(fs, sa->nextopenreg));
}


#else
#define initfirstfree2(fs,f) ((fs)->firstfree = 0)
#define updatenextopenexpr2(sa,fs)  \
  ((sa)->nextopenexpr = NULL, (sa)->nextopenreg = -1)
#endif /* HKSC_DECOMP_HAVE_PASS2 */


#ifdef LUA_DEBUG
static void debugenterblock2(StackAnalyzer *sa, BasicBlock *bbl)
{
  D(lprintf("pass2: entered %sblock %b (%i-%i) at pc (%i)\n", 
       bbl->isempty ? (sa->intailemptyblock ? "tail-empty " : "empty ") : "",
       bbl, bbl->startpc, bbl->endpc, sa->pc));
}

static void debugleaveblock2(StackAnalyzer *sa, BasicBlock *bbl)
{
D(lprintf("pass2: leaving %sblock %b (%i-%i) at pc (%i)\n", bbl->isempty ?
          (sa->intailemptyblock ? "tail-empty " : "empty") : "", bbl,
          bbl->startpc, bbl->endpc, sa->pc));
}
#else /* !LUA_DEBUG */
#define debugenterblock2(sa,bbl) ((void)0)
#define debugleaveblock2(sa,bbl) ((void)0)
#endif /* LUA_DEBUG */


#ifdef HKSC_DECOMP_DEBUG_PASS1
static void enterblock2(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bbl)
{
  if (bbl->type == BBL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  DumpIndentation(fs->D);
  DumpString(bbltypename(bbl->type),fs->D);
  DumpLiteral("\n",fs->D);
  UNUSED(sa);
}


static void leaveblock2(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bbl)
{
  if (bbl->type == BBL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  if (bbl->type != BBL_IF || bbl->nextsibling == NULL ||
      bbl->nextsibling->type != BBL_ELSE) { /* block ends with `END' */
    DumpIndentation(fs->D);
    DumpLiteral("END\n",fs->D);
  }
  UNUSED(sa);
}
#else /* !HKSC_DECOMP_DEBUG_PASS1 */

#define enterblock2(sa,fs,bbl) (void)0
#define leaveblock2(sa,fs,bbl) (void)0

#endif /* HKSC_DECOMP_DEBUG_PASS1 */

#ifdef LUA_DEBUG
static void assertbblvalid(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bbl)
{
  int startpc = bbl->startpc;
  int endpc = bbl->endpc;
  int type = bbl->type;
  BasicBlock *nextsibling = bbl->nextsibling;
  lua_assert(startpc <= endpc || bbl->isempty);
  lua_assert(type >= 0 && type < MAX_BBLTYPE);
  if (sa->intailemptyblock) {
    lua_assert(sa->pc+1 == startpc);
    lua_assert(bbl->nextsibling == NULL);
  }
  else
    lua_assert(sa->pc == startpc);
  if (type == BBL_FUNCTION) {
    lua_assert(sa->pc == 0);
    lua_assert(endpc == sa->sizecode-1);
  }
  if (type < BBL_DO && type != BBL_FUNCTION) {
    check_ins_property(fs, endpc, INS_LOOPEND);
    if (type == BBL_WHILE)
      check_ins_property(fs, startpc, INS_WHILESTAT);
    else if (type == BBL_REPEAT)
      check_ins_property(fs, startpc, INS_REPEATSTAT);
    else if (type == BBL_FORNUM)
      check_ins_property(fs, startpc, INS_FORNUM);
    else if (type == BBL_FORLIST)
      check_ins_property(fs, startpc, INS_FORLIST);
  }
  else if (type == BBL_DO) {
    check_ins_property(fs, startpc, INS_DOSTAT);
    check_ins_property(fs, endpc, INS_BLOCKEND);
  }
  else if (type == BBL_IF && nextsibling != NULL &&
           nextsibling->type == BBL_ELSE) {
    lua_assert(bbl->endpc+1 == nextsibling->startpc);
  }
}

static void printinsn2(DFuncState *fs, BasicBlock *bbl, int pc, Instruction i)
{
  int type = bbl->type;
  lprintf("pc = (%i), %O    %s\n", pc, i, bbltypename(type));
  UNUSED(fs);
}
#else /* !LUA_DEBUG */
#define assertbblvalid(sa,fs,bbl) ((void)(sa),(void)(fs),(void)(bbl))
#define printinsn2(fs,bbl,pc,i) ((void)(fs),(void)(bbl),(void)(pc),(void)(i))
#endif /* LUA_DEBUG */


static void visitinsn2(DFuncState *fs, BasicBlock *bbl, int pc, Instruction i)
{
#ifdef LUA_DEBUG
  printinsn2(fs, bbl, pc, i);
  printinsflags(fs, pc, "  flags for ");
  /* make sure this instruction hasn't already been visited */
  lua_assert(!test_ins_property(fs, pc, INS_VISITED));
  /* set this directly to avoid printing debug message every time */
  fs->a->insproperties[pc] |= (1 << INS_VISITED);
#endif /* LUA_DEBUG */
  UNUSED(fs); UNUSED(bbl); UNUSED(pc); UNUSED(i);
}


/*
** update pc variables based on CHILD; if it is not NULL, update
** NEXTCHILDSTARTPC to its startpc and NEXTPCLIMIT to 1 before its startpc,
** otherwise, set NEXTCHILDSTARTPC to (-1) and NEXTPCLIMIT to 1 before the endpc
** of PARENT
*/
static void initnextchild2(StackAnalyzer *sa, BasicBlock *parent,
                           BasicBlock *child, int *childstartpc)
{
  lua_assert(parent != NULL);
  lua_assert(childstartpc != NULL);
  if (child != NULL) {
    *childstartpc = child->startpc;
    sa->nextpclimit = child->startpc-1;
  }
  else {
    *childstartpc = -1;
    sa->nextpclimit = parent->endpc-1;
  }
}


static BasicBlock *updatenextchild2(StackAnalyzer *sa, BasicBlock *parent,
                                    BasicBlock *child, int *childstartpc)
{
  BasicBlock *nextchild;
  lua_assert(child != NULL);
  nextchild = child->nextsibling;
  initnextchild2(sa, parent, nextchild, childstartpc);
  return nextchild;
}

#ifdef HKSC_DECOMP_HAVE_PASS2

/*
** adds a new store node to the pending chain
*/
static void updatelaststore2(StackAnalyzer *sa, DFuncState *fs, ExpNode *exp)
{
  ExpNode *prevcall;
  int prevlaststore = sa->laststore;
  lua_assert(exp != NULL);
  lua_assert(exp->kind == ESTORE);
  exp->previndex = sa->laststore;
  sa->laststore = exp2index(fs, exp);
  /* when preserving line info, if the last expression has a different line
     than this store, add extra parens around it to make it end on the line that
     this store is mapped to */
  if (fs->D->matchlineinfo && prevlaststore == 0) {
    ExpNode *prev = index2exp(fs, exp2index(fs, exp)-1);
    if (prev != NULL && prev->line != exp->line)
      prev->closeparenline = exp->line;
  }
  /* this store may reference a register that holds the second or greater
     return value of the last function call; the actual expression node that is
     referenced by AUX may point to the wrong value in this case (if the
     function uses the register for one of its arguments), sp make sure AUX
     points to NULL instead of one of the function arguments */
  prevcall = index2exp(fs, fs->lastcallexp);
  lua_assert(prevcall == NULL || (prevcall >= fs->a->expstack.stk &&
             prevcall < &fs->a->expstack.stk[fs->a->expstack.used]));
  if (prevcall != NULL && prevcall->kind == ECALL) {
    int reg = exp->u.store.srcreg;
    int firstretreg = prevcall->info;
    int lastretreg = firstretreg + prevcall->u.call.nret;
    if (!ISK(reg) && reg > firstretreg && reg <= lastretreg)
      /* make source NULL because it uses results from a previous call */
      exp->aux = 0;
  }
}


#if 0
static void updateline2(DFuncState *fs, int pc)
{
  DecompState *D = fs->D;
  int line;
  lua_assert(ispcvalid(fs, pc));
  line = getline(fs->f,pc);
  D(lprintf("updating decompilation line\n"));
  D(lprintf("  opcode line = (%d)\n"
            "  decomp line = (%d)\n", line, D->linenumber));
  if (line > 0 && D->matchlineinfo) {
    int lines_needed = line - D->linenumber;
    beginline2(fs, lines_needed, D);
  }
  else {
    /* without using line info, see if a new line is needed */
    maybebeginline2(fs, D);
  }
}
#endif


/*static struct LocVar *newlocvaratreg2(DFuncState *fs, int reg)
{
  ExpNode *exp;
  struct LocVar *var;
  lua_assert(isregvalid(fs, reg));
  exp = getslotdesc(fs, reg)->u.exp;
  while (exp->prev)
    exp = exp->prev;

}
*/


static const char *getunoprstring(UnOpr op)
{
  static const char *const unoprstrings[] = {
    "-", "not", "#"
  };
  lua_assert(op != OPR_NOUNOPR);
  return unoprstrings[op];
}


static const char *getbinoprstring(BinOpr op)
{
  static const char *const binoprstrings[] = { /* ORDER OPR */
    "+", "-", "*", "/", "%", "^", "..",
#ifdef LUA_CODT7
    "<<", ">>", "&", "|",
#endif /* LUA_CODT7 */
    "~=", "==", "<", "<=", ">", ">=",
    "and", "or"
  };
  lua_assert(op != OPR_NOBINOPR);
  return binoprstrings[op];
}


static void DumpBinOpr(BinOpr op, DecompState *D)
{
  DumpString(getbinoprstring(op),D);
}


static const struct {
  lu_byte left;  /* left priority for each binary operator */
  lu_byte right; /* right priority */
} priority[] = {  /* ORDER OPR */
   {6, 6}, {6, 6}, {7, 7}, {7, 7}, {7, 7},  /* `+' `-' `/' `%' */
   {10, 9}, {5, 4},                 /* power and concat (right associative) */
#ifdef LUA_CODT7 /* T7 extensions */
   {5, 5}, {5, 5},                  /* shift left and shift right */
   {4, 4}, {4, 4},                  /* '&' and '|' */
#endif /* LUA_CODT7 */
   {3, 3}, {3, 3},                  /* equality and inequality */
   {3, 3}, {3, 3}, {3, 3}, {3, 3},  /* order */
   {2, 2}, {1, 1}                   /* logical (and/or) */
};

#define UNARY_PRIORITY  8  /* priority for unary operators */
#define SUBEXPR_PRIORITY  11  /* priority for called value */


static void checklineneeded2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  int line = exp->line;
  lua_assert(line >= D->linenumber);
  updateline2(fs, line, D);
  (void)maybebeginline2;
}

/******************************************************************************/
/* functions for dumping ExpNodes */

/*
** `HoldItem' stores a string that is created by a parent expression node, to
** be printed with a child expression node. The decompiler state holds onto it
** until the time comes to dump it.
*/
struct HoldItem {
  struct HoldItem *next;
  const char *str;
  size_t len;
  lu_byte addtrailingspace;
};


#define addliteralholditem2(D,i,s,a) addholditem2(D,i,"" s,sizeof(s)-1,a)
/*
** `addholditem2' appends a hold item to the chain
*/
static void addholditem2(DecompState *D, struct HoldItem *item,
                         const char *str, size_t len, lu_byte addtrailingspace)
{
  item->str = str;
  if (len == 0)
    return;  /* don't add the string if it is empty */
  lua_assert(str != NULL);
  item->len = len;
  item->addtrailingspace = addtrailingspace;
  item->next = NULL;
  if (D->holdlast != NULL)
    D->holdlast->next = item;
  else
    D->holdfirst = item;
  D->holdlast = item;
}


/*
** `dischargeholditems2' dumps all hold item strings and clears the list in the
** DecompState
*/
static void dischargeholditems2(DecompState *D)
{
  struct HoldItem *item = D->holdfirst;
  while (item != NULL) {
    CheckSpaceNeeded(D);
    DumpBlock(item->str, item->len, D);
    D->needspace = item->addtrailingspace;
    item = item->next;
  }
  D->holdfirst = D->holdlast = NULL;
}


/*
**
*/
#ifdef LUA_DEBUG
static int holdchainempty2(DecompState *D)
{
  return (D->holdfirst == NULL && D->holdlast == NULL);
}
#endif /* LUA_DEBUG */


/*
** `predumpexp2' - all expression dump functions call this before dumping
*/
static void predumpexp2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  /* add new lines if needed */
  checklineneeded2(D,fs,exp);
  /* dump all strings that were held until the line was updated */
  dischargeholditems2(D);
  CheckSpaceNeeded(D);
}


/*
** `postdumpexp2' - all expression dump functions call this after dumping
*/
static void postdumpexp2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  D->needspace = 1;
  UNUSED(fs); UNUSED(exp);
}


/* dump a constant (literal) expression */
static void dumpexpk2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  predumpexp2(D,fs,exp);
  DumpTValue(exp->u.k,D);
  postdumpexp2(D,fs,exp);
}


/* dump a global or upvalue name as an R-value */
static void dumpexpvar2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  predumpexp2(D,fs,exp);
  DumpTString(exp->u.name, D);
  postdumpexp2(D,fs,exp);
}


static void dumplocvar2(DecompState *D, DFuncState *fs, int reg);


/* dump a local variable name as an R-value */
static void dumpexplocvar2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  predumpexp2(D,fs,exp);
  dumplocvar2(D,fs,exp->aux); /* aux is the source register in OP_MOVE */
  postdumpexp2(D,fs,exp);
}


/* dump `nil' */
static void dumpexpnil2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  predumpexp2(D,fs,exp);
  DumpLiteral("nil",D);
  postdumpexp2(D,fs,exp);
}


/* dump `...' */
static void dumpexpva2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  predumpexp2(D,fs,exp);
  DumpLiteral("...",D);
  postdumpexp2(D,fs,exp);
}


/* dump a child function */
static void dumpexpfunc2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  /* do not update the line */
  dischargeholditems2(D);
  CheckSpaceNeeded(D);
  fs->lastclosurepc = exp->aux;
  DecompileFunction(D, exp->u.p);
  postdumpexp2(D,fs,exp);
}


#define checkexpinreg2(fs,reg)  \
  check_exp(getexpinreg2(fs,reg) != NULL, getexpinreg2(fs,reg))

/*
** `getexpinreg2' returns the expression node that is currently stored in REG.
** If REG contains an active local varible or REG is encoded as a K-index, NULL
** is returned
*/
static ExpNode *getexpinreg2(DFuncState *fs, int reg)
{
  if (ISK(reg))
    return NULL;
  lua_assert(isregvalid(fs, reg));
  if (test_reg_property(fs, reg, REG_LOCAL))
    return NULL;
  if (reg >= fs->firstfree)
    return NULL;
  return index2exp(fs, getslotdesc(fs, reg)->u.expindex);
}


/*
** append a new expression node to a slot descriptor chain
*/
static void pushexp2(DFuncState *fs, int reg, ExpNode *exp, int linkprev)
{
  lua_assert(isregvalid(fs, reg));
  lua_assert(!test_reg_property(fs, reg, REG_LOCAL));
  lua_assert(exp->previndex == 0);
  if (linkprev) {
    lua_assert(getslotdesc(fs, reg)->u.expindex != exp2index(fs, exp));
    exp->previndex = getslotdesc(fs, reg)->u.expindex;
  }
  getslotdesc(fs, reg)->u.expindex = exp2index(fs, exp);
}


static void flushpendingexp2(DFuncState *fs)
{
  int oldfirstfree = fs->firstfree;
  int newfirstfree = fs->nlocvars;
#ifdef LUA_DEBUG
  int i;
  for (i = 0; i < fs->a->expstack.used; i++) {
    ExpNode *exp = &fs->a->expstack.stk[i];
    lua_assert(exp->pending == 0);
  }
#endif /* LUA_DEBUG */
  fs->a->expstack.used = 0;
  D(lprintf("resetting fs->firstfree from %d to %d\n", fs->firstfree,
            fs->nlocvars));
  lua_assert(oldfirstfree >= newfirstfree);
  /* firstfree does not need to be an accessible slot as it can be equal to
     maxstacksize */
  if (isregvalid(fs, newfirstfree))
  /* clear all pending data */
  memset(getslotdesc(fs, newfirstfree), 0,
         (oldfirstfree-newfirstfree) * sizeof(SlotDesc));
  fs->firstfree = newfirstfree;
  fs->lastcallexp = 0;
}


static ExpNode *popexp2(DFuncState *fs, int reg)
{
  ExpNode *exp;
  lua_assert(isregvalid(fs, reg));
  lua_assert(!test_reg_property(fs, reg, REG_LOCAL));
  exp = index2exp(fs, getslotdesc(fs, reg)->u.expindex);
  getslotdesc(fs, reg)->u.expindex = exp->previndex;
  return exp;
}


/*
** `dumplocvar2' dumps the name of the local variable in REG
*/
static void dumplocvar2(DecompState *D, DFuncState *fs, int reg)
{
  lua_assert(holdchainempty2(D));
  lua_assert(isregvalid(fs, reg));
  lua_assert(test_reg_property(fs, reg, REG_LOCAL));
  CheckSpaceNeeded(D); /* print leading space if needed */
  DumpTString(getslotdesc(fs, reg)->u.locvar->varname, D);
  D->needspace = 1;
}


/*
** `dumpRK2' dumps either the local variable in REG or the constant value
** indexed by INDEXK(REG). Do not call this function before making sure REG
** does not hold a pending expression
*/
static void dumpRK2(DecompState *D, DFuncState *fs, int reg, ExpNode *op)
{
  if (op != NULL)
    predumpexp2(D,fs,op);
  else
    dischargeholditems2(D);
  if (ISK(reg)) {
    CheckSpaceNeeded(D);
    DumpConstant(fs, INDEXK(reg), D);
  }
  else {
    lua_assert(test_reg_property(fs, reg, REG_LOCAL));
    dumplocvar2(D, fs, reg);
  }
  if (op != NULL)
    postdumpexp2(D,fs,op);
}


static void dumpexp2(DecompState *D, DFuncState *fs, ExpNode *exp,
                     unsigned int limit);


/*
** dumps an operand for a pending binary or unary operation
*/
static void dumpexpoperand2(DecompState *D, DFuncState *fs, ExpNode *operand,
                            ExpNode *op, unsigned int limit)
{
  lua_assert(op != NULL);
  /* The operand can be NULL if the operation is the first code in the program,
     for example:
        local a = -nil;
     The code above produces OP_UNM as the first instruction */
  if (operand == NULL) {
    ExpNode dummy;
    dummy.kind = ENIL;
    dummy.line = op->line;
    dumpexpnil2(D, fs, &dummy);
  }
  else {
    dumpexp2(D, fs, operand, limit);
  }
}


static void dumpcloseparen2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  int saveline = exp->line;
  lua_assert(exp->line <= exp->closeparenline);
  exp->line = exp->closeparenline;
  /* the line that the closing paren is on matters when matching */
  checklineneeded2(D,fs,exp);
  exp->line = saveline;
  DumpLiteral(")",D);
}


/*
** `ExpListIterator' helps properly iterate through a complete expression list
*/
struct ExpListIterator {
  DFuncState *fs;
  ExpNode *lastexp;  /* the last ExpNode that was dumped */
  ExpNode *nextexp;  /* the next ExpNode in the list */
  int firstreg;  /* the start register of the expression list */
};


/*
** initializes an expression list iterator, starting at FIRSTREG with FIRSTEXP
*/
static void initexplistiter2(struct ExpListIterator *iter, DFuncState *fs,
                             int firstreg, ExpNode *firstexp)
{
  lua_assert(firstexp != NULL);
  lua_assert(isregvalid(fs, firstreg));
  iter->fs = fs;
  iter->lastexp = firstexp;
  iter->nextexp = firstexp;
  iter->firstreg = firstreg;
}


/*
** updates the iterator state to the next expression in the list and returns it
*/
static ExpNode *getnextexpinlist2(struct ExpListIterator *iter, int i)
{
  ExpNode *nextexp;
  lua_assert(iter->nextexp != NULL);
  iter->lastexp = iter->nextexp;
  /* first try to follow the next expression chained to the current one */
  nextexp = index2exp(iter->fs, iter->nextexp->nextregindex);
  if (nextexp == NULL)
    /* if NULL, the previous expression is more than one register removed; see
       if there is a pending expression in the next register explicitly */
    nextexp = getexpinreg2(iter->fs, iter->firstreg+i);
  if (nextexp == NULL) {  /* multiple nil's */
    /* if NULL, there is a previous nil expression that occupies this register
       */
    lua_assert(iter->lastexp != NULL);
    nextexp = iter->lastexp;
    /* this expression has already been dumped and will be dumped again */
    nextexp->pending = 1;
  }
  iter->nextexp = nextexp;
  return nextexp;
}


/*
** dumps an expression node to the output
*/
static void dumpexp2(DecompState *D, DFuncState *fs, ExpNode *exp,
                     unsigned int limit)
{
  /* NEEDPARENFORLINEINFO is for using extra parens to preserve line info */
  int needparenforlineinfo = (exp->line != exp->closeparenline);
  struct HoldItem holdparen;
  lua_assert(exp != NULL);
  lua_assert(exp->pending);
  exp->pending = 0;
  if (index2exp(fs, exp->prevregindex))
    index2exp(fs, exp->prevregindex)->nextregindex = exp->previndex;
  if (index2exp(fs, exp->nextregindex))
    index2exp(fs, exp->nextregindex)->prevregindex = exp->previndex;
  switch (exp->kind) {
    case EUNOP: { /* unary operation */
      ExpNode *o; /* the operand if it is a pending expression */
      UnOpr op = exp->u.unop.op;
      int b = exp->u.unop.b; /* arg B from the instruction */
      int needparen = (UNARY_PRIORITY < limit) || exp->u.unop.needinnerparen ||
                      needparenforlineinfo;
      struct HoldItem holdop;
      const char *unopstring = getunoprstring(op);
      if (needparen)
        addliteralholditem2(D, &holdparen, "(", 0);
      addholditem2(D, &holdop, unopstring, strlen(unopstring), op == OPR_NOT);
      /* b is set to (-1) to tell this function to use `bindex' instead to index
         the pending expression in the expression stack */
      o = (b == -1) ? index2exp(fs, exp->u.unop.bindex) : NULL;
      /* see explanation `case EBINOP' */
      if (op == OPR_MINUS && o != NULL && o->kind == EUNOP &&
          o->u.unop.op == OPR_MINUS)
        o->u.unop.needinnerparen = 1;
      if (o != NULL && o->line != exp->line)
        o->closeparenline = exp->line;
      /* dump operand */
      if (b == -1) /* dump pending expressions */
        dumpexpoperand2(D, fs, o, exp, UNARY_PRIORITY);
      else /* dump constant or local variable name */
        dumpRK2(D, fs, b, exp);
      if (needparen)
        DumpLiteral(")",D);
      D->needspace = 1;
      break;
    }
    case EBINOP: { /* binary operation */
      ExpNode *o1, *o2;
      BinOpr op = exp->u.binop.op;
      int b = exp->u.binop.b;
      int c = exp->u.binop.c;
      int needparen;  /* for preserving order of operations */
      if (exp->leftside)
        needparen = (priority[op].right < limit);
      else
        needparen = (priority[op].left <= limit);
      o1 = (b == -1) ? index2exp(fs, exp->u.binop.bindex) : NULL;
      o2 = (c == -1) ? index2exp(fs, exp->u.binop.cindex) : NULL;
      if (o1 != NULL)
        o1->leftside = 1;
      /* If the second operand's code does not map to the same line as this
         expression's code, parens need to wrap around this binary operation,
         with the close paren emitted on a later line than the second operand to
         preserve line mappings when recompiling the output. Here is an example
         of what I'm talking about:
            [1] local a = (a + 12) * (b + 12
            [2] )
         This generates the following code and line info:
            1 [1] GETGLOBAL 0 -1  ; a
            2 [1] ADD       0 0 -2  ; - 12
            3 [1] GETGLOBAL 1 -3  ; b
            4 [1] ADD       1 1 -2  ; - 12
            5 [2] MUL       0 0 1
            6 [2] RETURN    0 1
         Notice how OP_MUL is mapped to line 2; this is because the last paren
         is on line 2 */
      if (o2 != NULL && o2->line != exp->line)
        o2->closeparenline = exp->line;
      if (needparen || needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      if (b == -1)
        /* discharge pending expression in B */
        dumpexpoperand2(D, fs, o1, exp, priority[op].left);
      else
        /* dump constant INDEXK(B) or local variable in B */
        dumpRK2(D, fs, b, exp); /* first operand */
      DumpSpace(D);
      DumpBinOpr(op,D); /* operator */
      D->needspace = 1;
      if (c == -1)
        /* discharge pending expression in C */
        dumpexpoperand2(D, fs, o2, exp, priority[op].right);
      else
        /* dump constant INDEXK(C) or local variable in C */
        dumpRK2(D, fs, c, exp); /* second operand */
      if (needparen || needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
      D->needspace = 1;
      break;
    }
    case EINDEXED: {
      ExpNode *tab, *key;
      int b = exp->u.indexed.b;
      int c = exp->u.indexed.c;
      int isfield = exp->u.indexed.isfield;
      struct HoldItem holditem; /* dot or open-brace */
      tab = (b == -1) ? index2exp(fs, exp->u.indexed.bindex) : NULL;
      key = (c == -1) ? index2exp(fs, exp->u.indexed.cindex) : NULL;
      if (needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      if (b == -1)
        dumpexpoperand2(D, fs, tab, exp, SUBEXPR_PRIORITY);
      else
        dumpRK2(D, fs, b, exp);
      if (isfield)
        addliteralholditem2(D, &holditem, ".", 0);
      else
        addliteralholditem2(D, &holditem, "[", 0);
      D->needspace = 0;
      if (c == -1) {
        lua_assert(isfield == 0);
        dumpexpoperand2(D, fs, key, exp, 0);
      }
      else {
        if (isfield) {
          TString *field = rawtsvalue(&fs->f->k[INDEXK(c)]);
          predumpexp2(D, fs, exp);
          DumpTString(field,D);
          postdumpexp2(D, fs, exp);
        }
        else {
          dumpRK2(D, fs, c, exp);
        }
      }
      if (isfield == 0)
        DumpLiteral("]",D);
      if (needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
      break;
    }
    case ECALL: {
      struct ExpListIterator iter;
      ExpNode *firstexp;
      int narg = exp->u.call.narg;
      int i;
      firstexp = index2exp(fs, exp->previndex);
      lua_assert(firstexp != NULL);  /* there must be an expression to call */
      /* initialize the expression list iterator for dumping arguments */
      initexplistiter2(&iter, fs, exp->info+1, firstexp);
      if (needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      dumpexp2(D, fs, firstexp, SUBEXPR_PRIORITY);  /* called expression */
      DumpLiteral("(",D);  /* start of arguments */
      D->needspace = 0;
      for (i = 0; i < narg; i++) {
        if (i != 0)
          DumpLiteral(",",D);
        dumpexp2(D, fs, getnextexpinlist2(&iter, i), 0);
      }
      DumpLiteral(")",D);  /* end of arguments */
      if (needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
      D->needspace = 1;  /* will not always be set */
      break;
    }
    case ECONCAT: {
      struct ExpListIterator iter;
      ExpNode *firstexp;
      int firstindex = exp->u.concat.firstindex;
      int lastindex = exp->u.concat.lastindex;
      int nexps = exp->aux;
      int i;
      int needparen;
      if (exp->leftside)
        needparen = (priority[OPR_CONCAT].right < limit);
      else
        needparen = (priority[OPR_CONCAT].left <= limit);
      needparen = (needparen || needparenforlineinfo);
      if (needparen)
        addliteralholditem2(D, &holdparen, "(", 0);
      firstexp = index2exp(fs, firstindex);
      lua_assert(firstexp != NULL);
      /* intialize the expression list iterator starting at the first register
         used in the concatenation */
      initexplistiter2(&iter, fs, firstexp->info, firstexp);
      dumpexp2(D, fs, firstexp, priority[OPR_CONCAT].left);
      D(lprintf("nexps = %d (firstindex = %d, lastindex = %d)\n", nexps,
                firstindex, lastindex));
      lua_assert(nexps >= 2);
      for (i = 1; i < nexps; i++) {
        CheckSpaceNeeded(D);
        DumpBinOpr(OPR_CONCAT,D);
        D->needspace = 1;  /* space between `..' and next expression */
        dumpexp2(D, fs, getnextexpinlist2(&iter, i), priority[OPR_CONCAT].left);
      }
      if (needparen)
        dumpcloseparen2(D, fs, exp);
      break;
    }
    case EUPVAL:
    case EGLOBAL:
      if (needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      dumpexpvar2(D,fs,exp);
      if (needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
      break;
    case ELOCAL:
      if (needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      dumpexplocvar2(D,fs,exp);
      if (needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
      break;
    case ESTORE:
      lua_assert(0);
      break;
    case ELITERAL:
    case ENIL:
    case EVARARG:
    case ECLOSURE: {
      int needparen = (limit == SUBEXPR_PRIORITY || needparenforlineinfo);
      if (needparen)
        addliteralholditem2(D, &holdparen, "(", 0);
      if (exp->kind == ELITERAL)
        dumpexpk2(D,fs,exp); /* dump literal */
      else if (exp->kind == ENIL)
        dumpexpnil2(D,fs,exp); /* dump nil */
      else if (exp->kind == EVARARG)
        dumpexpva2(D,fs,exp); /* dump vararg */
      else if (exp->kind == ECLOSURE)
        dumpexpfunc2(D,fs,exp); /* decompile child function */
      else
        lua_assert(0);
      if (needparen)
        dumpcloseparen2(D, fs, exp);
      break;
    }
    default:
      DumpLiteral("[UNHANDLED EXP KIND]",D);
      break;
  }
}


/* discharge whatever is in REG, writing its printable form to the output */
static void dischargefromreg2(DFuncState *fs, int reg, unsigned int priority)
{
  DecompState *D = fs->D;
  lua_assert(isregvalid(fs, reg));
  if (test_reg_property(fs, reg, REG_LOCAL)) { /* dump local variable name */
    dumplocvar2(D, fs, reg);
  }
  else { /* dump pending expression in REG */
    dumpexp2(D, fs, checkexpinreg2(fs, reg), priority);
  }
  (void)popexp2;
}


/*
** emits a function call as a statement
*/
static void emitcallstat2(DFuncState *fs, ExpNode *call)
{
  DecompState *D = fs->D;
  lua_assert(call != NULL);
  lua_assert(call->kind == ECALL);
  lua_assert(call->u.call.nret == 0);
  dumpexp2(D, fs, call, 0);
  DumpSemi(D);
  flushpendingexp2(fs);
}


/*
** emits a break statement
*/
static void emitbreakstat2(DFuncState *fs, int pc)
{
  DecompState *D = fs->D;
  int line = getline(fs->f, pc);
  int needblock = (test_ins_property(fs, pc, INS_BLOCKEND) == 0);
  updateline2(fs, line, D);
  CheckSpaceNeeded(D);
  if (needblock) DumpLiteral("do ",D);
  DumpLiteral("break",D);
  DumpSemi(D);
  if (needblock) DumpLiteral(" end",D);
}


/*
** emits a return statement if necessary (the return won't be emitted if it is
** the final augmented return)
*/
static void emitretstat2(DFuncState *fs, int pc, int reg, int nret)
{
  DecompState *D = fs->D;
  int line = getline(fs->f, pc);
  int needblock;
  lua_assert(isregvalid(fs, reg));
  if (D->matchlineinfo && nret != 0) {
    ExpNode *top = gettopexp(fs);
    /* if this return code is mapped to a different line than the last
       expression to be returned, wrap that expression in parens and put the
       closing paren on the line that the return is mapped to; this preserves
       line info when recompiling */
    if (top != NULL && line != top->line)
      top->closeparenline = line;
  }
  needblock = (test_ins_property(fs, pc, INS_BLOCKEND) == 0);
  if (pc == fs->f->sizecode-1)
    return;  /* final return is augmented by the compiler */
  if (nret == 0) {
    updateline2(fs, line, D);
    CheckSpaceNeeded(D);
    if (needblock) DumpLiteral("do ",D);
    DumpLiteral("return",D);
  }
  else if (nret == 1 && test_reg_property(fs, reg, REG_LOCAL)) {
    updateline2(fs, line, D);
    CheckSpaceNeeded(D);
    if (needblock) DumpLiteral("do ",D);
    DumpLiteral("return",D);
    DumpLiteral(" ",D);
    dumplocvar2(D, fs, reg);
  }
  else {
    struct ExpListIterator iter;
    struct HoldItem holdreturn;
    ExpNode *firstexp;
    int i;
    if (needblock)
      addliteralholditem2(D, &holdreturn, "do return", 1);
    else
      addliteralholditem2(D, &holdreturn, "return", 1);
    firstexp = getexpinreg2(fs, reg);  /* first expression to return */
    lua_assert(firstexp != NULL);
    dumpexp2(D, fs, firstexp, 0);  /* dump first expression in list */
    /* initialize the expression list iterator starting at REG */
    initexplistiter2(&iter, fs, reg, firstexp);
    for (i = 1; i < nret; i++) {
      DumpLiteral(",",D);
      dumpexp2(D, fs, getnextexpinlist2(&iter, i), 0);
    }
  }
  DumpSemi(D);
  if (needblock)
    DumpLiteral(" end",D);
  flushpendingexp2(fs);
}


/*
** emits extra values in an assigned expression list
*/
static void emitresidualexp2(DFuncState *fs, int reg, ExpNode *lastexp)
{
  ExpNode *firstexp = lastexp;
  DecompState *D = fs->D;
  int firstfree = fs->firstfree;
  int i = reg;
  lua_assert(lastexp != NULL);
  /* dump any extra expressions that won't be assigned to anything; this is
     needed in a case such as the following example:
        [1] local e;
        [2] e = 1, (a * 3 / (b+4)
        [3] );
     which produces the following code:
        1 [1] LOADNIL   0 0
        2 [2] LOADK     1 -1  ; 1
        3 [2] GETGLOBAL 2 -2  ; a
        4 [2] MUL       2 2 -3  ; - 3
        5 [2] GETGLOBAL 3 -4  ; b
        6 [2] ADD       3 3 -5  ; - 4
        7 [2] DIV       2 2 3
        8 [3] MOVE      0 1
        9 [3] RETURN    0 1
     This specific example also explains the need for a LIMIT, so that the last
     expression can be known before it is dumped. This is necessary for
     preserving line info: notice the OP_MOVE is mapped to line 3 because of the
     final close paren being on that line. Because the expression generated from
     OP_MOVE and the expression generated from OP_DIV are not related to each
     other, the line-mapping differences have to be accounted for here, and
     handled before dumping the final expression */
  for (; i < firstfree; i++) {
    ExpNode *exp = getexpinreg2(fs, i);
    if (exp == NULL) {
      if (lastexp->kind == ENIL && lastexp->aux >= i) {
        exp = lastexp;
        exp->pending = 1;
        goto dumpresidual;
      }
      break; /* todo: is there any other case where EXP can be NULL? */
    }
    else if (exp->pending) {
      dumpresidual:
      if (i+1 == firstfree) { /* this is the last expression */
        /* this is the last expression, check if parens need to be added to
           preserve line info */
        lua_assert(firstexp != NULL && exp != NULL);
        if (firstexp->line != exp->line)
          exp->closeparenline = firstexp->line;
      }
      DumpComma(D);
      dumpexp2(D, fs, exp, 0);
      lastexp = exp;
    }
  }
#if 0
  if (currexp != NULL) {
    lua_assert(currexp->pending);
    /* this is the last expression, check if parens need to be added to preserve
       line info */
    printf("currexp->pending = %d\n", currexp->pending);
    if (firstexp->line != currexp->line)
      currexp->closeparenline = firstexp->line;
    DumpComma(D);
    dumpexp2(D, fs, currexp, 0);
  }
#endif
}



/*
** marks a register REG as holding an active local variable, pointed to by VAR
*/
static void setreglocal2(DFuncState *fs, int reg, struct LocVar *var)
{
  lua_assert(isregvalid(fs, reg));
  lua_assert(!test_reg_property(fs, reg, REG_LOCAL));
  unset_reg_property(fs, reg, REG_PENDING);
  set_reg_property(fs, reg, REG_LOCAL);
  getslotdesc(fs, reg)->u.locvar = var;
}


#define addliteral2buff(H,b,str) addstr2buff(H,b,"" str, sizeof(str)-1)
#define addspace2buff(H,b,D)  \
  if ((D)->needspace && luaZ_bufflen(b)) addliteral2buff(H,b," ")

static void addstr2buff(hksc_State *H, Mbuffer *b, const char *str, size_t len)
{
  size_t size = luaZ_sizebuffer(b);
  size_t pos = luaZ_bufflen(b);
#ifdef LUA_DEBUG
  len++;  /* add NULL if debugging to the read contents of the buffer easier */
#endif /* LUA_DEBUG */
  if (pos + len > size) {
    size = pos + len;
    luaZ_resizebuffer(H, b, size);
  }
#ifdef LUA_DEBUG
  len--;
  *(luaZ_buffer(b)+pos+len) = '\0';
#endif /* LUA_DEBUG */
  memcpy(luaZ_buffer(b)+pos, str, len);
  pos += len;
  luaZ_bufflen(b) = pos;
}


static void addkvalue2buff(hksc_State *H, Mbuffer *b, TValue *o)
{
  switch (ttype(o))
  {
    case LUA_TNIL:
      addliteral2buff(H, b, "nil");
      break;
    case LUA_TBOOLEAN:
      if (bvalue(o)) addliteral2buff(H, b,"true");
      else addliteral2buff(H, b,"false");
      break;
    case LUA_TLIGHTUSERDATA: {
      char s[LUAI_MAXUI642STR+sizeof("0xhi")-1];
      sprintf(s, "0x%zuhi", cast(size_t, pvalue(o)));
      addstr2buff(H,b,s,strlen(s));
      break;
    }
    case LUA_TNUMBER: {
      char s[LUAI_MAXNUMBER2STR];
      sprintf(s, "%g", nvalue(o));
      addstr2buff(H,b,s,strlen(s));
      break;
    }
    case LUA_TSTRING: {
      TString *str = luaO_kstring2print(H, rawtsvalue(o));
      addstr2buff(H,b,getstr(str),str->tsv.len);
      break;
    }
    case LUA_TUI64: {
      char s[LUAI_MAXUI642STR+sizeof("oxhl")-1];
      lua_ui642str(s+2, ui64value(o));
      s[0] = '0'; s[1] = 'x';
      strcat(s, "hl");
      addstr2buff(H,b,s,strlen(s));
      break;
    }
    default:
      lua_assert(0);
      break;
  }
}


/*
** `LHSStringBuilder' is an auxiliary structure used by `dischargestores2' to
** ensure all L-values are emitted in the correct order and with even spacing
** between items
*/
struct LHSStringBuilder {
  hksc_State *H;
  DFuncState *fs;
  DecompState *D;
  Mbuffer *buff;
  int needspace;
  unsigned int currpriority;
  lu_byte empty;
};


static void initstringbuilder2(struct LHSStringBuilder *sb, DecompState *D)
{
  sb->H = D->H;
  sb->fs = D->fs;
  sb->D = D;
  sb->buff = &D->buff;
  sb->needspace = 0;
  sb->currpriority = 0;
  sb->empty = 1;
  luaZ_resetbuffer(sb->buff);
}


static void addtolhsbuff2(struct LHSStringBuilder *sb, const char *str,
                         size_t len)
{
  lua_assert(len != 0);
  if (sb->needspace)
    addliteral2buff(sb->H, sb->buff, " ");
  addstr2buff(sb->H, sb->buff, str, len);
  sb->needspace = 1;
  sb->empty = 0;
}


static void flushlhsbuff2(struct LHSStringBuilder *sb)
{
  luaZ_resetbuffer(sb->buff);
  sb->needspace = 0;
  sb->empty = 1;
}


static void addkvalue2lhs(struct LHSStringBuilder *sb, TValue *o)
{
  if (sb->empty == 0 && sb->needspace)
    addliteral2buff(sb->H, sb->buff, " ");
  addkvalue2buff(sb->H, sb->buff, o);
  sb->needspace = 1;
  sb->empty = 0;
}


static void addchartolhs2(struct LHSStringBuilder *sb, int c)
{
  char x = (char)c;
  lua_assert(c != 0);
  addtolhsbuff2(sb, &x, 1);
}

#define addvarnametolhs2(sb,name) addtstolhs2(sb,name)
static void addtstolhs2(struct LHSStringBuilder *sb, TString *ts)
{
  lua_assert(ts != NULL);
  addtolhsbuff2(sb, getstr(ts), ts->tsv.len);
}


static void addcommatolhs2(struct LHSStringBuilder *sb)
{
  sb->needspace = 0;
  if (sb->empty) {
    /* no spaces before commas */
    sb->D->needspace = 0;
    DumpComma(sb->D);
    sb->D->needspace = 1;
  }
  else
    addchartolhs2(sb, ',');
}


static void completelhsbuilder2(struct LHSStringBuilder *sb)
{
  DecompState *D = sb->D;
  if (sb->empty) {
    CheckSpaceNeeded(D);
    DumpLiteral("=",D);
    D->needspace = 1;
    sb->needspace = 0;
  }
  else {
    addchartolhs2(sb, '=');
  }
}


static void addregtolhs2(struct LHSStringBuilder *sb, int reg)
{
  lua_assert(isregvalid(sb->fs, reg));
  if (test_reg_property(sb->fs, reg, REG_LOCAL)) {
    struct LocVar *var = getslotdesc(sb->fs, reg)->u.locvar;
    lua_assert(var != NULL && var->varname != NULL);
    /* add local variable name */
    addvarnametolhs2(sb, var->varname);
  }
  else {  /* a temporary expression */
    struct HoldItem holdcurr;
    addholditem2(sb->D, &holdcurr, luaZ_buffer(sb->buff),
                 luaZ_bufflen(sb->buff), sb->needspace);
    dischargefromreg2(sb->fs, reg, sb->currpriority);
    flushlhsbuff2(sb);
  }
}


static void addktolhs2(struct LHSStringBuilder *sb, int k)
{
  lua_assert(ISK(k));
  addkvalue2lhs(sb,&sb->fs->f->k[INDEXK(k)]);
}


static void addindextolhs2(struct LHSStringBuilder *sb, int reg, int isfield)
{
  DecompState *D = sb->D;
  int wasempty = sb->empty;
  int needspace = sb->needspace;
  if (wasempty) {
    D->needspace = needspace;
    CheckSpaceNeeded(D);
    if (isfield) DumpLiteral(".",D);
    else DumpLiteral("[",D);
  }
  else {
    addchartolhs2(sb, isfield ? '.' : '[');
    sb->needspace = needspace;  /* if there was no space between the table and
                                   `[', than also omit space between `[' and the
                                   index */
  }
  if (ISK(reg)) {
    if (isfield)
      addtstolhs2(sb, rawtsvalue(&sb->fs->f->k[INDEXK(reg)]));
    else
      addktolhs2(sb, reg);
  }
  else {
    addregtolhs2(sb, reg);
  }
  if (!isfield) {
    sb->needspace = needspace;
    addchartolhs2(sb, ']');
  }
  if (wasempty) {
    DumpBlock(luaZ_buffer(sb->buff), luaZ_bufflen(sb->buff), D);
    flushlhsbuff2(sb);
    D->needspace = 1;
  }
}


#define checkdischargestores2(sa,fs) if((sa)->laststore)dischargestores2(sa,fs)


/*
** dumps the pending assignment list (check if there are pending stores before
** calling)
*/
static void dischargestores2(StackAnalyzer *sa, DFuncState *fs)
{
  ExpNode dummy;  /* dummy (nil) node to use when there is no pending source */
  DecompState *D = fs->D;
  struct LHSStringBuilder sb;
  struct HoldItem lhs;  /* LHS names */
  ExpNode *exp;  /* iterator for traversing the store-chain */
  ExpNode *lastsrc;  /* last source expression */
  int lastsrcreg;  /* greatest source register that is referenced in a store */
  int i = 0;
  exp = index2exp(fs, sa->laststore);
  lua_assert(exp != NULL);
  initstringbuilder2(&sb, D);
  while (1) {
    OpCode rootop;  /* opcode family for the current store */
    ExpNode *prev;
    rootop = exp->u.store.rootop;
    if (exp->kind != ESTORE || rootop == OP_MOVE) {
      /* assigning to local variable */
      D(lprintf("OP_MOVE from reg %d\n", exp->info));
      addregtolhs2(&sb, exp->info);
    }
    else if (rootop == OP_SETFIELD || rootop == OP_SETTABLE) {
      sb.currpriority = SUBEXPR_PRIORITY;
      addregtolhs2(&sb, exp->u.store.aux1);  /* add table variable */
      sb.needspace = 0;  /* no space between table and `[' */
      sb.currpriority = 0;
      /* add index */
      addindextolhs2(&sb, exp->u.store.aux2, rootop == OP_SETFIELD);
    }
    else if (rootop == OP_SETGLOBAL || rootop == OP_SETUPVAL) {
      TString *varname = rawtsvalue(&fs->f->k[exp->u.store.aux1]);
      addvarnametolhs2(&sb, varname);
    }
    else
      lua_assert(0);
    prev = index2exp(fs, exp->previndex);  /* get previous in chain */
    if (prev == NULL) { /* end of chain */
      /* LASTSRCREG can be a K value */
      lastsrcreg = (exp->kind == ESTORE) ? exp->u.store.srcreg : exp->info;
      break;
    }
    addcommatolhs2(&sb);  /* add comma before next variable */
    exp = prev;
  }
  completelhsbuilder2(&sb);
  addholditem2(D, &lhs, luaZ_buffer(sb.buff), luaZ_bufflen(sb.buff),
               sb.needspace);
  i = 0; /* reset counter */
  exp = index2exp(fs, sa->laststore);  /* go back to begining of chain */
  /* Make LASTSRC point to DUMMY initially; if there is no first source
     expression, than the store must have relied on a previous OP_LOADNIL,
     which has already been discharged, for example, the following code:
        local a, b, c, d;
        a, b, c = nil;
     The above code generates a single OP_LOADNIL, followed by 3 OP_MOVE's, and
     by the time the OP_MOVE's are being discharged in this function, the
     expression node created for OP_LOADNIL has already been discharged. */
  lastsrc = &dummy;
  lastsrc->kind = ENIL;
  lastsrc->aux = lastsrcreg;
  lastsrc->info = exp->info;
  D(lprintf("lastsrcreg = %d\n", lastsrcreg));
  while (1) { /* tarverse the chain to dump RHS values */
    /* get expression to assigm */
    ExpNode *src = (exp->kind == ESTORE) ? index2exp(fs, exp->aux) : exp;
    if (exp->u.store.srcreg == -1)
      lua_assert(src != NULL);
    if (src != NULL) {
      /* for N remaining variables, if there are exactly N expressions
         remaining and they are all nil, do not write them, the entire
         assignment is just one `nil', which needs to be written */
      if (i != 0 && src->kind == ENIL && src->aux == lastsrcreg)
        src->pending = 0;  /* skip */
      else {
        if (i != 0)
          DumpComma(D);
        dumpexp2(D, fs, src, 0);
      }
      lastsrc = src;
    }
    else if (ISK(exp->u.store.srcreg)) {
      if (i != 0)
        DumpComma(D);
      dumpRK2(D, fs, exp->u.store.srcreg, exp);
    }
    else if (exp->u.store.srcreg != -1 &&
             test_reg_property(fs, exp->u.store.srcreg, REG_LOCAL)) {
      if (i != 0)
        DumpComma(D);
      dumplocvar2(D,fs,exp->u.store.srcreg);
    }
    else {
      /* in case DUMMY is what LASTSRC points to, set the needed values */
      if (lastsrc == &dummy) {
        dummy.line = exp->line;
        dummy.closeparenline = exp->closeparenline;
        dummy.pending = 1;
      }
      lua_assert(lastsrc != NULL);
      if (lastsrc->kind == EVARARG && lastsrc->info+lastsrc->aux-1 >= i)
        ; /* vararg has already been emitted */
      else if (i != 0 && lastsrc->kind == ENIL && lastsrc->aux == lastsrcreg)
        ; /* invisible nil's */
      else if (lastsrc->kind == ECALL || lastsrc->kind == ETAILCALL)
        ;
      else if (lastsrc->kind == ENIL) {
        lastsrc->pending = 1;
        if (i != 0)
          DumpComma(D);
        dumpexp2(D, fs, lastsrc, 0);
      }
    }
    exp = index2exp(fs, exp->previndex);
    if (exp == NULL) /* end of chain */
      break;
    i++;
  }
  if (!ISK(lastsrcreg))
    emitresidualexp2(fs, lastsrcreg+1, lastsrc);
  DumpSemi(D); /* `;' */
  flushpendingexp2(fs); /* flush everything */
  sa->laststore = 0;
}


/*
** activates new local variables and emits a declaration/initialization
** statement for them
*/
static void initlocvars2(DFuncState *fs, int firstreg, int nvars)
{
  ExpNode dummy;
  ExpNode *firstexp, *lastexp;
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  int i, lastreg;
  int seenfirstexp = 0;
  Mbuffer *b;
  struct HoldItem lhs;
  lua_assert(isregvalid(fs, firstreg));
  lua_assert(isregvalid(fs, firstreg+nvars-1));
  lua_assert(nvars > 0);
  b = &D->buff;
  luaZ_resetbuffer(b);
  addliteral2buff(H, b, "local ");
  lastreg = firstreg+nvars-1;
  for (i = firstreg; i <= lastreg; i++) {
    struct LocVar *var;
    size_t len;
    lua_assert(fs->nlocvars < fs->sizelocvars);
    var = &fs->locvars[fs->nlocvars+(i-firstreg)];
    lua_assert(var->varname != NULL);
    len = var->varname->tsv.len;
    addstr2buff(H, b, getstr(var->varname), len);
    if (i != lastreg)
      addliteral2buff(H, b, ", ");
  }
  firstexp = getexpinreg2(fs, firstreg);
  lastexp = NULL;
  /* FIRSTEXP can be NULL if this assignment relies on a previous OP_LOADNIL
     which has already been handled and for which no pending expression exists
     anymore  */
  if (firstexp == NULL) {
    firstexp = &dummy;
    firstexp->kind = ENIL;
    firstexp->line = D->linenumber;
    firstexp->closeparenline = firstexp->line;
    firstexp->info = firstreg;
    firstexp->aux = lastreg;
    lastexp = firstexp;
  }
  /*lua_assert(firstexp == checkexpinreg2(fs, firstreg));*/
  lua_assert(firstexp->info == firstreg);
  /* if only assigning nil's, avoid writing the RHS altogether, unless it is
     just one variable */
  if (nvars == 1 || firstexp->kind != ENIL || firstexp->aux != lastreg)
    addliteral2buff(H, b, " = ");
  addholditem2(D, &lhs, luaZ_buffer(b), luaZ_bufflen(b), 0);
  D(printf("added hold item for decl: `%.*s'\n", cast_int(luaZ_bufflen(b)),
           luaZ_buffer(b)));
  D(lprintf("firstreg = %d, lastreg = %d\n", firstreg, lastreg));
  for (i = firstreg; i <= lastreg; i++) {
    ExpNode *exp = getexpinreg2(fs, i); /* the pending expression in REG */
    setreglocal2(fs, i, &fs->locvars[fs->nlocvars++]);
    if (nvars > 1 && firstexp->kind == ENIL && firstexp->aux == lastreg)
      continue; /* don't write  */
    else if (exp != NULL) {
      if (nvars > 1 && exp->kind == ENIL && exp->aux == lastreg)
        exp->pending = 0;
      else {
        if (seenfirstexp)
          DumpComma(D);
        dumpexp2(D, fs, exp, 0);
        seenfirstexp = 1;
      }
      lastexp = exp;
    }
    else {
      lua_assert(lastexp != NULL);
      if (lastexp->kind == EVARARG &&
          (lastexp->info+lastexp->aux-1) >= i) {
        /*int lastvareg = lastexp->info+lastexp->aux-1;
        CHECK(fs, lastvareg >= i, "")*/
        continue; /* vararg has already been emitted */
      }
      else if (nvars > 1 && lastexp->kind == ENIL && lastexp->aux == lastreg) {
        continue; /* only nil's remain; don't print them */
      }
      else if (lastexp->kind == ENIL) {
        /*CHECK(fs, lastexp->aux >= i, "");*/
        if (seenfirstexp)
          DumpComma(D);
        lastexp->pending = 1;
        dumpexp2(D, fs, lastexp, 0);
      }
      else {
        DumpLiteral("UNHANDLED CASE in initlocvar2\n",D);
      }
    }
    /*needc initlocvar2(fs, i, &fs->locvars[fs->nlocvars++]);*/
  }
  if (seenfirstexp == 0) {
    /* no expressions have been dumped, but the items in the hold need to be
       discharged and the line needs to be updated */
    firstexp->pending = 0;
    predumpexp2(D,fs,firstexp);
    postdumpexp2(D,fs,firstexp);
  }
  else {
    int firstfree = fs->firstfree;
    lua_assert(lastexp != NULL);
    /* dump any extra expressions that won't be assigned to anything; this is
       needed an a case such as the following example:
          local a, b, c = 1, 2, 3, 4;
       which produces the following code:
          1 [1]  LOADK     0 -1  ; 1
          2 [1]  LOADK     1 -2  ; 2
          3 [1]  LOADK     2 -3  ; 3
          4 [1]  LOADK     3 -4  ; 4
          5 [1]  RETURN    0 1
       The fourth OP_LOADK is still generated, and needs to be reflected in the
       decomp */
    for (; i < firstfree; i++) {
      ExpNode *exp = getexpinreg2(fs, i);
      if (exp == NULL) {
        if (lastexp->kind == ENIL && lastexp->aux >= i) {
          exp = lastexp;
          exp->pending = 1;
          goto dumpresidual;
        }
        break; /* todo: is there any other case where EXP can be NULL? */
      }
      else if (exp->pending) {
        dumpresidual:
        DumpComma(D);
        dumpexp2(D, fs, exp, 0);
        lastexp = exp;
      }
    }
  }
  DumpSemi(D); /* `;' */
  flushpendingexp2(fs); /* discharge everything */
}


/*
** commits an expression to slot REG, either emitting an assignment if REG
** holds an active local variable or appending the new expression to the pending
** chain
*/
static ExpNode *addexptoreg2(StackAnalyzer *sa, DFuncState *fs, int reg,
                             ExpNode *exp, int *splitnil)
{
  lua_assert(isregvalid(fs, reg));
  lua_assert(exp->kind != ESTORE);
  if (!test_reg_property(fs, reg, REG_LOCAL)) {
    int link, lastreg;
    if (exp->kind == ENIL)
      lastreg = exp->aux;
    else if (exp->kind == EVARARG && exp->aux > 1)
      lastreg = exp->info + exp->aux-2;
    else
      lastreg = reg;
    lua_assert(isregvalid(fs, lastreg));
    link = fs->firstfree <= lastreg;
    if (link) {
      D(lprintf("updating fs->firstfree from %d to %d\n",
                fs->firstfree, lastreg+1));
      fs->firstfree = lastreg+1;
    }
    if (exp->kind == ECALL || exp->kind == ETAILCALL)
      link = 1;  /* linking is needed to access the called expression */
    else if (exp->kind == ENIL) {
      /* check if the next open expression uses this nil expression */
      if (sa->openexprkind == -1 && sa->nextopenexpr != NULL &&
          sa->nextopenexpr->startpc == sa->pc+1 &&
          exp->info < sa->nextopenreg && exp->aux >= sa->nextopenreg) {
        /* set the nil debt for the next open expression */
        sa->openexprnildebt = exp->aux+1-sa->nextopenreg;
        /* make this nil node end 1 before the open register */
        exp->aux = sa->nextopenreg-1;
      }
      /* check if there is a single return that uses this nil expression */
      else if (test_ins_property(fs, sa->pc, INS_PRERETURN1)) {
        int retreg;
        lua_assert(ispcvalid(fs, sa->pc+1));
        lua_assert(GET_OPCODE(sa->code[sa->pc+1]) == OP_RETURN);
        retreg = GETARG_A(sa->code[sa->pc+1]);
        if (exp->info < retreg && exp->aux >= retreg) {
          /* set the nil debt for the next return */
          sa->openexprnildebt = exp->aux+1-retreg;
          /* make this nil node end 1 before the returned register */
          exp->aux = retreg-1;
        }
      }
    }
    pushexp2(fs, reg, exp, link);
    if (splitnil != NULL) *splitnil = 0;
    return exp;
  }
  else {
    lua_assert(fs->nlocvars > 0);
    lua_assert(exp->info < fs->nlocvars);
    exp->previndex = sa->laststore;
    sa->laststore = exp2index(fs, exp);
    if (exp->kind == ENIL && exp->aux >= fs->nlocvars) {
      ExpNode *new;
      ExpNode node = *exp;  /* the node starts as local; the stack is about to
                               be discharged; add it to the stack after */
      node.info = fs->nlocvars;  /* start this node at the first pending */
      exp->aux = fs->nlocvars-1;  /* end the original node at the last local */
      dischargestores2(sa, fs);  /* store some of the nil's */
      new = newexp(fs);  /* put the rest of the nil's in a new pending node */
      *new = node;
      /* if this node loads nil into the first open register, change its line so
         that it gets emitted with the first opcode of the open expression,
         which is likely how it was in the source code; this has to be done here
         since the node has already been split and is no longer shared, so there
         will be no OPENEXPRNILDEBT when entering the open expression in
         `openexpr2'; otherwise, OPENEXPRNILDEBT is set and the new nil node
         will be created in `openexpr2', and its line will be set then */
      if (sa->openexprkind == -1 && sa->nextopenexpr != NULL &&
          sa->nextopenexpr->startpc == sa->pc+1 &&
          fs->nlocvars == sa->nextopenreg) {
        lua_assert(ispcvalid(fs, sa->pc+1));
        new->line = getline(fs->f, sa->pc+1);
        new->closeparenline = new->line;
      }
      if (splitnil != NULL) *splitnil = 1;
      lua_assert(!test_reg_property(fs, fs->nlocvars, REG_LOCAL));
      return addexptoreg2(sa, fs, new->info, new, NULL);
    }
    if (splitnil != NULL) *splitnil = 0;
    return exp;
  }
}


/*
** discharges the current nil debt by adding a new nil expression into REG
*/
static void dischargenildebt2(StackAnalyzer *sa, DFuncState *fs, int reg)
{
  ExpNode *exp;
  lua_assert(sa->openexprnildebt > 0);
  exp = newexp(fs);
  exp->kind = ENIL;
  exp->previndex = exp->prevregindex = exp->nextregindex = 0;
  exp->info = reg;
  exp->aux = exp->info + sa->openexprnildebt-1;
  exp->line = getline(fs->f, sa->pc);
  exp->closeparenline = exp->line;
  exp->dependondest = 0;
  exp->leftside = 0;
  exp->pending = 1;
  addexptoreg2(sa, fs, exp->info, exp, NULL);
  sa->openexprnildebt = 0;
}


static ExpNode *addexp2(StackAnalyzer *sa, DFuncState *fs, int pc, OpCode o,
                        int a, int b, int c, int bx)
{
  ExpNode node;
  ExpNode *exp = &node;
  const Proto *f = fs->f;
  lua_assert(ispcvalid(fs, pc));
  exp->info = a;
  exp->aux = 0;
  exp->previndex = exp2index(fs, NULL);
  if (a == 0)
    exp->prevregindex = exp2index(fs, NULL);
  else
    exp->prevregindex = exp2index(fs, getexpinreg2(fs, a-1));
  exp->nextregindex = exp2index(fs, NULL);
  exp->line = getline(f,pc);
  exp->closeparenline = exp->line;
  exp->leftside = 0;
  exp->pending = 1;
  switch (o) {
    case OP_GETGLOBAL:
      exp->kind = EGLOBAL;
      /* todo: if generating variable names, make sure this global name doesnt
         conflict with any of the variable names generated so far, create a test
         like the following and do a no-debug test:
            local a = 12;  -- will generate `local f0_local0 = 12;'
            local b = f0_local0; -- needs to generate OP_GETGLOBAL

            local function c()
                return f0_local0; -- needs to generate OP_GETGLOBAL
            end
         */
      exp->u.name = rawtsvalue(&f->k[bx]);
      break;
    case OP_LOADBOOL:
      exp->kind = b ? ETRUE : EFALSE;
      exp->aux = c;
      break;
    case OP_LOADK:
      exp->kind = ELITERAL;
      exp->u.k = &f->k[bx];
      break;
    case OP_LOADNIL:
      exp->kind = ENIL;
      exp->aux = b;
      break;
    case OP_GETUPVAL:
      exp->kind = EUPVAL;
      lua_assert(b >= 0 && b < f->nups);
      lua_assert(fs->upvalues[b] != NULL);
      exp->u.name = fs->upvalues[b];
      break;
    case OP_NEWTABLE:
      exp->kind = ECON;
      exp->u.con.arrsize = luaO_fb2int(b);
      exp->u.con.hashsize = luaO_fb2int(c);
      exp->u.con.est = (exp->u.con.arrsize == 0 && exp->u.con.hashsize > 16);
      break;
    case OP_CLOSURE:
      exp->kind = ECLOSURE;
      exp->aux = pc;
      exp->u.p = f->p[bx];
      break; /* todo */
    case OP_VARARG:
      exp->kind = EVARARG;
      exp->aux = b;
      break;
    case OP_MOVE:
      if (!test_reg_property(fs, b, REG_LOCAL)) {
        CHECK(fs, sa->openexprkind == -1, "unexpected OP_MOVE that uses a "
              "temporary register as its source in an open expression");
        exp->kind = ESTORE;
        exp->aux = getslotdesc(fs, b)->u.expindex;
        exp->u.store.rootop = OP_MOVE;
        exp->u.store.srcreg = b;
      }
      else {
        exp->kind = ELOCAL;
        exp->aux = b; /* source register */
      }
      break;
    case OP_UNM:
      exp->kind = EUNOP;
      exp->u.unop.b = b;
      exp->u.unop.op = OPR_MINUS;
      break;
    case OP_NOT: case OP_NOT_R1:
      exp->kind = EUNOP;
      exp->u.unop.b = b;
      exp->u.unop.op = OPR_NOT;
      break;
    case OP_LEN:
      exp->kind = EUNOP;
      exp->u.unop.b = b;
      exp->u.unop.op = OPR_LEN;
      break;
    case OP_TESTSET:
      /* todo */
      break;
    case OP_SELF:
      /* todo */
      break;
    case OP_GETFIELD: case OP_GETFIELD_R1:
      exp->kind = EINDEXED;
      exp->u.indexed.b = b;
      exp->u.indexed.c = RKASK(c);
      /* write as a field unless the field is a reserved word */
      exp->u.indexed.isfield = (rawtsvalue(&fs->f->k[c])->tsv.reserved == 0);
      break;
    case OP_GETTABLE_S:
    case OP_GETTABLE_N:
    case OP_GETTABLE:
      exp->kind = EINDEXED;
      exp->u.indexed.b = b;
      exp->u.indexed.c = c;
      exp->u.indexed.isfield = 0;
      break;
    case OP_CALL:
    case OP_CALL_I:
    case OP_CALL_I_R1:
    case OP_CALL_C:
    case OP_CALL_M:
      exp->kind = ECALL;
      goto setcallinfo;
    case OP_TAILCALL:
    case OP_TAILCALL_I:
    case OP_TAILCALL_I_R1:
    case OP_TAILCALL_C:
    case OP_TAILCALL_M:
      exp->kind = ETAILCALL;
      CHECK(fs, sa->openexprkind == -1, "unexpected OP_TAILCALL in open "
            " expression");
      setcallinfo:
      exp->u.call.op = o;
      exp->u.call.nret = c-1;
      exp->u.call.narg = b-1;
      if (exp->u.call.narg == -1) {
        ExpNode *lastexp = index2exp(fs, sa->lastexpindex);
        if (lastexp == NULL)
          exp->u.call.narg = 0;
        else
          exp->u.call.narg = lastexp->info-a;
      }
      if (exp->u.call.nret == 0) {
        CHECK(fs, sa->openexprkind == -1, "unexpected call statement in open "
              "expression (call returns 0 values)");
      }
      exp->aux = exp2index(fs, getexpinreg2(fs, a+exp->u.call.narg));
      break;
    case OP_CONCAT:
      exp->kind = ECONCAT;
      exp->u.concat.firstindex = exp2index(fs, getexpinreg2(fs, b));
      exp->u.concat.lastindex =exp2index(fs, getexpinreg2(fs, c));
      exp->aux = c+1-b;  /* number of expressions in concat */
      break;
    /* arithmetic operations */
    case OP_ADD: case OP_ADD_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_ADD;
      break;
    case OP_SUB: case OP_SUB_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_SUB;
      break;
    case OP_MUL: case OP_MUL_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_MUL;
      break;
    case OP_DIV: case OP_DIV_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_DIV;
      break;
    case OP_MOD: case OP_MOD_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_MOD;
      break;
    case OP_POW: case OP_POW_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_POW;
      break;
#ifdef LUA_CODT7
    case OP_LEFT_SHIFT: case OP_LEFT_SHIFT_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_LEFT_SHIFT;
      break;
    case OP_RIGHT_SHIFT: case OP_RIGHT_SHIFT_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_RIGHT_SHIFT;
      break;
    case OP_BIT_AND: case OP_BIT_AND_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_BIT_AND;
      break;
    case OP_BIT_OR: case OP_BIT_OR_BK:
      exp->kind = EBINOP;
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = OPR_BIT_OR;
      break;
#endif /* LUA_CODT7 */
    /* the remaining operations do not clobber A */
    default:
      return NULL;
  }
  if (exp->kind == EBINOP) {
    exp->dependondest = (a == b || a == c);
    if (!ISK(b) && !test_reg_property(fs, b, REG_LOCAL)) {
      /* B references a pending expression in register B, save the index of the
         current expression in B */
      exp->u.binop.b = -1; /* put an invalid value */
      exp->u.binop.bindex = getslotdesc(fs, b)->u.expindex;
    }
    if (!ISK(c) && !test_reg_property(fs, c, REG_LOCAL)) {
      /* C references a pending expression in register C, save the index of the
         current expression in C */
      exp->u.binop.c = -1; /* put an invalid value */
      exp->u.binop.cindex = getslotdesc(fs, c)->u.expindex;
    }
  }
  else if (exp->kind == EUNOP) {
    exp->dependondest = (a == b);
    exp->u.unop.needinnerparen = 0;
    /* B must be a register */
    if (!test_reg_property(fs, b, REG_LOCAL)) {
      exp->u.unop.b = -1;
      exp->u.unop.bindex = getslotdesc(fs, b)->u.expindex;
    }
  }
  else if (exp->kind == EINDEXED) {
    exp->dependondest = (a == b || a == c);
    CHECK(fs, isregvalid(fs, b), "invalid register operand for indexed table");
    if (!test_reg_property(fs, b, REG_LOCAL)) {
      exp->u.indexed.b = -1;
      exp->u.indexed.bindex = getslotdesc(fs, b)->u.expindex;
    }
    if (!ISK(exp->u.indexed.c) &&
        !test_reg_property(fs, exp->u.indexed.c, REG_LOCAL)) {
      exp->u.indexed.c = -1;
      exp->u.indexed.cindex = getslotdesc(fs, c)->u.expindex;
    }
  }
  else
    exp->dependondest = 0;
  /* discharge stores now before pushing a new expression node */
  if (exp->kind != ESTORE)
    checkdischargestores2(sa,fs);
  exp = newexp(fs);
  *exp = node;
  sa->lastexpindex = exp2index(fs, exp);
  if (index2exp(fs, exp->prevregindex))
    index2exp(fs, exp->prevregindex)->nextregindex = sa->lastexpindex;
  if (exp->kind == ESTORE) {
    exp->pending = 0;
    updatelaststore2(sa, fs, exp);
  }
  else if (exp->kind == ECALL || exp->kind == ETAILCALL)
    fs->lastcallexp = exp2index(fs, exp);  /* update last call node */
  return exp;
}


static int addstore2(StackAnalyzer *sa, DFuncState *fs, int pc, OpCode o, int a,
                     int b, int c, int bx)
{
  ExpNode *exp;
  int srcreg;
  int info;
  switch (o) {
    case OP_SETFIELD: case OP_SETFIELD_R1:
      exp = newexp(fs);
      /* OP_SETFIELD is used as the root if it should be written as a field */
      if (rawtsvalue(&fs->f->k[b])->tsv.reserved > 0)
        exp->u.store.rootop = OP_SETTABLE;
      else
        exp->u.store.rootop = OP_SETFIELD;
      b = RKASK(b);
      goto addstoretable;
    case OP_SETTABLE:
    case OP_SETTABLE_BK:
    case OP_SETTABLE_S:
    case OP_SETTABLE_S_BK:
    case OP_SETTABLE_N:
    case OP_SETTABLE_N_BK:
      exp = newexp(fs);
      exp->u.store.rootop = OP_SETTABLE;
      addstoretable:
      srcreg = c;
      info = a;
      exp->u.store.aux1 = a;  /* table register */
      exp->u.store.aux2 = b;  /* index register */
      break;
    case OP_SETGLOBAL:
      CHECK(fs, isregvalid(fs, a),
            "invalid register referenced in OP_SETGLOBAL");
      exp = newexp(fs);
      srcreg = a;
      info = -1;
      exp->u.store.rootop = OP_SETGLOBAL;
      exp->u.store.aux1 = bx;
      exp->u.store.aux2 = 0;
      break;
    case OP_SETUPVAL:
    case OP_SETUPVAL_R1:
      CHECK(fs, isregvalid(fs, a),
            "invalid register referenced in OP_SETUPVAL");
      exp = newexp(fs);
      srcreg = a;
      info = -1;
      exp->u.store.rootop = OP_SETUPVAL;
      exp->u.store.aux1 = b;
      exp->u.store.aux2 = 0;
      break;
    default:
      return 0;
  }
  exp->kind = ESTORE;
  exp->info = info;
  if (ISK(srcreg) || test_reg_property(fs, srcreg, REG_LOCAL))
    exp->aux = 0;
  else
    exp->aux = getslotdesc(fs, srcreg)->u.expindex;
  exp->line = getline(fs->f, pc);
  exp->closeparenline = exp->line;
  exp->previndex = exp->prevregindex = exp->nextregindex = 0;
  exp->dependondest = 0;
  exp->leftside = 0;
  exp->pending = 0;
  exp->u.store.srcreg = srcreg;
  updatelaststore2(sa, fs, exp);
  return 1;
}


static void openexpr2(StackAnalyzer *sa, DFuncState *fs)
{
  const OpenExpr *e = sa->nextopenexpr;
  lua_assert(e != NULL);
  lua_assert(sa->nextopenreg != -1);
  lua_assert(e->endpc >= sa->pc);
  lua_assert(ispcvalid(fs, e->endpc));
  sa->openexprkind = e->kind;
  lua_assert(sa->openexprnildebt >= 0);
  if (sa->openexprnildebt)
    dischargenildebt2(sa, fs, sa->nextopenreg);
  updatenextopenexpr2(sa, fs);  /* discharge this one now */
  for (; sa->pc < e->endpc; sa->pc++) {
    int pc = sa->pc;
    Instruction i = sa->code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    int bx = GETARG_Bx(i);
    int sbx = GETARG_sBx(i);
    if (sa->nextopenexpr != NULL && sa->nextopenexpr->startpc == pc) {
      /* there is no need to recrusively call itself; just upfate the next
         OpenExpr entry so it is not a child of this one */
      /*openexpr2(sa, fs);*/
      updatenextopenexpr2(sa, fs);
    }
    visitinsn2(fs, sa->currbbl, pc, i);
    /* todo: may want to check for unexpected opcodes such as OP_RETURN, but
       I only want to do that if it would prevent a crash, otherwise I won't
       bother */
    if (testAMode(o)) { /* A is a register */
      ExpNode *exp = addexp2(sa, fs, pc, o, a, b, c, bx);
      if (exp != NULL) {
        D(lprintf("created new expression node\n"));
        D(lprintf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(lprintf("---------------------------\n"));
        addexptoreg2(sa, fs, a, exp, NULL);
      }
    }
    UNUSED(sbx);
  }
  sa->openexprkind = -1;
}


#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*
** Second pass `basic block' handler - the function's registers and stack are
** analyzed and local variables are detected. With information about the local
** variables, any undetected do-end blocks can be detected in this pass.
*/
static void bbl2(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bbl)
{
  DecompState *D = fs->D;
  const Instruction *code = sa->code;
  BasicBlock *nextchild = bbl->firstchild;
  int nextchildstartpc;
  /*int startpc = bbl->startpc;*/
  int endpc = bbl->endpc;
  /*int type = bbl->type;*/
  assertbblvalid(sa,fs,bbl);
  debugenterblock2(sa, bbl);
  enterblock2(sa, fs, bbl);
  /* mark this block as visited */
  lua_assert(bbl->visited == 0);
  D(bbl->visited = 1);
  if (bbl->isempty) /* block has no instructions */
    goto block2finished;
  D->indentlevel++;
  /* initialize NEXTCHILDSTARTPC and NEXTPCLIMIT */
  initnextchild2(sa, bbl, nextchild, &nextchildstartpc);
  /* main instruction loop */
  for (; sa->pc < sa->sizecode; sa->pc++) {
    int pc, a, b, c, bx, sbx;
    Instruction i;
    OpCode o;
    int numvars; /* number of variables which start at PC+1 */
    /* check if the next child starts here */
    if (sa->pc == nextchildstartpc) {
      /* save NEXTCHILD->ISEMPTY to use after updating NEXTCHILD */
      int waschildempty;
      processnextchild:
      lua_assert(nextchild != NULL);
      waschildempty = nextchild->isempty;
      bbl2(sa, fs, nextchild);
      nextchild = updatenextchild2(sa, bbl, nextchild, &nextchildstartpc);
      if (sa->intailemptyblock) {
        sa->intailemptyblock = 0;
        goto loopfinished;
      }
      else if (!waschildempty) {
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
    bx = GETARG_Bx(i);
    sbx = GETARG_sBx(i);
#ifdef HKSC_DECOMP_DEBUG_PASS1
    visitinsn2(fs, bbl, pc, i); /* visit this instruction */
    /* make sure to run the first pass on all nested closures */
    if (o == OP_CLOSURE) { /* nested closure? */
      const Proto *f = fs->f->p[bx];
      DecompileFunction(D,f);
    }
    UNUSED(numvars);
#else /* !HKSC_DECOMP_DEBUG_PASS1 */
    numvars = ispcvalid(fs, pc+1) ? varstartsatpc2(fs, pc+1) : -1;
    /* todo: need to check if this instruction begins preparation code:
       the following properties need to be checked:
       INS_PRECALL
       INS_PRECONCAT
       INS_PREFORLIST
       INS_PREFORNUM
       INS_PRERETURN
       INS_PRERETURN1 */
    if (sa->nextopenexpr != NULL && sa->nextopenexpr->startpc == pc) {
      sa->currbbl = bbl;
      openexpr2(sa, fs);
      pc = sa->pc;
      i = code[pc];
      o = GET_OPCODE(i);
      a = GETARG_A(i);
      b = GETARG_B(i);
      c = GETARG_C(i);
      bx = GETARG_Bx(i);
      sbx = GETARG_sBx(i);
      numvars = ispcvalid(fs, pc+1) ? varstartsatpc2(fs, pc+1) : -1;
    }
    visitinsn2(fs, bbl, pc, i); /* visit this instruction */
    if (testAMode(o)) { /* A is a register */
      ExpNode *exp;
      lua_assert(!test_ins_property(fs, pc, INS_BREAKSTAT));
      exp = addexp2(sa, fs, pc, o, a, b, c, bx);
      if (exp != NULL) {
        int splitnil;
        D(lprintf("created new expression node\n"));
        D(lprintf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(lprintf("---------------------------\n"));
        if (exp->kind != ESTORE)
          exp = addexptoreg2(sa, fs, a, exp, &splitnil);
        if (exp->kind == ECALL && exp->u.call.nret == 0)
          emitcallstat2(fs, exp);
        /* if the final return the next code and it is mapped to a different
           line than this expression, wrap this expression in parens and put
           the closing paren on the line that the return is mapped to; this
           preserves line info when recompiling */
        if (D->matchlineinfo && pc+1 == fs->f->sizecode-1) {
          int retline = getline(fs->f, pc+1);
          lua_assert(GET_OPCODE(code[pc+1]) == OP_RETURN);
          if (retline != exp->line) {
            D(lprintf("splitnil = %d\n", splitnil));
            if (splitnil) {
              lua_assert(exp->kind == ENIL);
              exp->line = exp->closeparenline = retline;
            }
            else
              exp->closeparenline = retline;
          }
        }
        if (numvars > 0) {
          checkdischargestores2(sa, fs);
          D(lprintf("NEW LOCAL VARIABLE\n"));
          (void)getfirstexp;
          initlocvars2(fs, fs->nlocvars, numvars);
        }
      }
      else {  /* an A-mode instruction that doesn't clobber A */
        if (addstore2(sa, fs, pc, o, a, b, c, bx) == 0) { /* not a store */
          /* the current store chain has ended; discharge it */
          checkdischargestores2(sa, fs);
          if (o == OP_RETURN) {
            if (sa->openexprnildebt)
              dischargenildebt2(sa, fs, a);
            emitretstat2(fs, pc, a, b-1);
          }
        }
      }
    }
    else {
      checkdischargestores2(sa, fs);
      if (test_ins_property(fs, pc, INS_BREAKSTAT)) {
        lua_assert(o == OP_JMP);
        lua_assert(bbl->type >= BBL_WHILE && bbl->type <= BBL_FORLIST);
        emitbreakstat2(fs, pc);
      }
    }
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
    if (pc == endpc)
      break;
  }
  /* check for an empty child block at the end of this block */
  if (nextchildstartpc != -1) {
    lua_assert(nextchild != NULL);
    lua_assert(nextchild->isempty != 0);
    lua_assert(nextchild->nextsibling == NULL);
    lua_assert(sa->intailemptyblock == 0);
    sa->intailemptyblock = 1;
    goto processnextchild;
  }
  loopfinished:
  D->indentlevel--;
  block2finished:
  leaveblock2(sa, fs, bbl);
  debugleaveblock2(sa, bbl);
}


static void pass2(const Proto *f, DFuncState *fs)
{
  BasicBlock *functionblock = fs->a->bbllist.first;
  StackAnalyzer sa;
  sa.pc = 0;
  sa.code = f->code;
  sa.sizecode = f->sizecode;
  sa.maxstacksize = f->maxstacksize;
  sa.intailemptyblock = 0;
  sa.inheadercondition = 0;
  sa.lastexpindex = 0;
  sa.laststore = 0;
  sa.openexprkind = -1;
  sa.openexprnildebt = 0;
  lua_assert(functionblock != NULL);
  lua_assert(functionblock->type == BBL_FUNCTION);
  initfirstfree2(fs, f); /* set the first free reg for this function */
  fs->lastfirstfree = fs->firstfree;
  updatenextopenexpr2(&sa, fs);
  bbl2(&sa, fs, functionblock);
#ifdef LUA_DEBUG
  { /* debug: make sure all instructions were visited */
    int pc;
    for (pc = 0; pc < f->sizecode; pc++)
      check_ins_property(fs, pc, INS_VISITED);
    checktreevisited(functionblock);
  }
#endif /* LUA_DEBUG */
}


static void DecompileFunction(DecompState *D, const Proto *f)
{
  DFuncState new_fs;
  open_func(&new_fs, D, f);
  pass1(f,&new_fs);
  debugpass1summary(&new_fs);
  pass2(f,&new_fs);
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
  D.name = H->currinputname;
  D.status=0;
  D.funcidx=0;
  D.indentlevel=-1;
  D.linenumber=1;
  D.needspace=0;
  D.holdfirst = NULL;
  D.holdlast = NULL;
  D.usedebuginfo = (!Settings(H).ignore_debug && f->sizelineinfo > 0);
  D.matchlineinfo = (Settings(H).match_line_info && D.usedebuginfo);
  luaZ_initbuffer(H, &D.buff);
  luaZ_resetbuffer(&D.buff);
  sd.D=&D;
  sd.f=f;
  status = luaD_pcall(H, f_decompiler, &sd);
  luaZ_freebuffer(H, &D.buff);
  if (status) D.status = status;
  return D.status;
}

#endif /* HKSC_DECOMPILER */
