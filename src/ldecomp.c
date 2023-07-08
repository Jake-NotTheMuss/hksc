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

#define DEFBLTYPE(e)  #e,
static const char *const bltypenames [] = {
  BLTYPE_TABLE
  "MAX_BLTYPE"
};
#undef DEFBLTYPE

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

#define bltypename(v) (bltypenames[v])
#define insflagname(v) (insflagnames[v])
#define regflagname(v) (regflagnames[v])

#else /* !LUA_DEBUG */

#define bltypename(v) ("")
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
  short nlocvars;  /* number of local variables created so far */
  lu_byte nactvar;
  int sizelocvars;
  int sizeupvalues;
  int lastclosurepc;  /* PC of last OP_CLOSURE */
  int firstclob;  /* first pc that clobbers register A */
  int firstclobnonparam;  /* first pc that clobbers non-parameter register A */
  int firstfree;
  int lastcallexp;  /* exp index of last function call node */
  int curr_constructor;  /* exp index of current table constructor */
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
      case 'R': /* register flag */
        printf("%s", regflagname(va_arg(argp, int)));
        break;
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
      case 'B': { /* BlockNode * */
        BlockNode *block = va_arg(argp, BlockNode *);
        if (block != NULL)
          printf("(%s) (%d-%d)", bltypename(block->type), block->startpc+1,
                 block->endpc+1);
        else
          printf("(NULL)");
        break;
      }
      case 'b': { /* BlockNode * (type only) */
        BlockNode *block = va_arg(argp, BlockNode *);
        if (block != NULL)
          printf("%s", bltypename(block->type));
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


/* empty blocks start 1 pc after they end; the startpc to use for comparisons is
   the emptiness subtracted from the startpc (consider a tail-empty child block
   which has an actual startpc that is greater than its parent block's endpc, or
   similarly, a previous empty sibling block that ends immediately before its
   next sibling block, which would have a startpc that is equal to its sibling
   block's startpc) */
#define blstartpc(bl)  check_exp(bl, cast_int((bl)->startpc - (bl)->isempty))

#define isforloop(bl)  ((bl)->type == BL_FORNUM || (bl)->type == BL_FORLIST)

/*
** allocate a new block structure and return it
*/
static BlockNode *newblnode(hksc_State *H, int startpc, int endpc, int type) {
  BlockNode *node = luaM_new(H, BlockNode);
  node->next = NULL;
  node->nextsibling = NULL;
  node->firstchild = NULL;
  node->startpc = startpc;
  node->endpc = endpc;
  node->type = type;
  node->isempty = (endpc < startpc);
  node->augmentedbyp1 = 0;
  D(node->visited = 0);
  return node;
}


/*
** re-calculate the emptiness of a block
*/
static void recalcemptiness(BlockNode *node) {
  node->isempty = (node->endpc < node->startpc);
}


static int haselsepart(const BlockNode *node) {
  lua_assert(node != NULL);
  lua_assert(node->type == BL_IF);
  return (node->nextsibling != NULL && node->nextsibling->type == BL_ELSE);
}


static int getforloopbase(const Instruction *code, const BlockNode *node) {
  lua_assert(isforloop(node));
  if (node->type == BL_FORNUM)
    return GETARG_A(code[node->endpc]);
  else
    return GETARG_A(code[node->endpc-1]);
}


/*
** returns the next free expression in the expression node stack and increment
** the number of stack elements in use
*/
static BlockState *newblockstate(DFuncState *fs)
{
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  lua_assert(a->pendingstk.used >= 0 &&
             a->pendingstk.used <= a->pendingstk.total);
  luaM_growvector(H, a->pendingstk.u.s1, a->pendingstk.used,
      a->pendingstk.total, BlockState, MAX_INT, "too many block contexts");
  return &a->pendingstk.u.s1[a->pendingstk.used++];
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
  lua_assert(a->pendingstk.used >= 0 &&
             a->pendingstk.used <= a->pendingstk.total);
  luaM_growvector(H, a->pendingstk.u.s2, a->pendingstk.used, a->pendingstk.total,
                  ExpNode, MAX_INT, "too many expression nodes");
  return &a->pendingstk.u.s2[a->pendingstk.used++];
}


#define prevexp(fs,exp) index2exp(fs, exp->previndex)

static int exp2index(DFuncState *fs, ExpNode *exp)
{
  if (exp == NULL)
    return 0;
  else {
    lua_assert(exp >= fs->a->pendingstk.u.s2 &&
               exp < fs->a->pendingstk.u.s2+fs->a->pendingstk.used);
    return exp-fs->a->pendingstk.u.s2+1;
  }
}


static ExpNode *index2exp(DFuncState *fs, int index)
{
  if (index == 0)
    return NULL;
  else
    return fs->a->pendingstk.u.s2+(index-1);
}


#define checkfirstexp(fs) check_exp(getfirstexp(fs) != NULL, getfirstexp(fs))
#define checktopexp(fs) check_exp(gettopexp(fs) != NULL, gettopexp(fs))

static ExpNode *getfirstexp(DFuncState *fs)
{
  int used = fs->a->pendingstk.used;
  if (used == 0)
    return NULL;
  else {
    lua_assert(used > 0);
    return &fs->a->pendingstk.u.s2[0];
  }
}


static ExpNode *gettopexp(DFuncState *fs)
{
  int used = fs->a->pendingstk.used;
  if (used == 0)
    return NULL;
  else {
    lua_assert(used > 0);
    return &fs->a->pendingstk.u.s2[used-1];
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
          luaO_ptr2str(s, pvalue(o));
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
    case ECOMP:
    case EBINOP:
      lprintf("[BINOP %s  %d, %d]", getbinoprstring(exp->u.binop.op),
              exp->u.binop.b, exp->u.binop.c);
      break;
    case EUNOP:
      lprintf("[UNOP %s  %d]", getunoprstring(exp->u.unop.op),
              exp->u.unop.b);
      break;
    case ECONSTRUCTOR:
      lprintf("'{}' %d, %d", exp->u.con.arrsize, exp->u.con.hashsize);
      break;
    case ESTORE:
      lprintf("STORE (from %d)", exp->u.store.srcreg);
      break;
    case ECONDITIONAL: {
      int i;
      lprintf("[CONDITIONAL '%s']\n", getbinoprstring(exp->u.cond.goiftrue ?
                                                      OPR_AND : OPR_OR));
      for (i=0;i<=indent;i++)
        lprintf("  ");
      lprintf("- ");
      debugexp(fs, index2exp(fs, exp->u.cond.e1), indent+1);
      for (i=0;i<=indent;i++)
        lprintf("  ");
      lprintf("- ");
      debugexp(fs, index2exp(fs, exp->u.cond.e2), indent+1);
      return;
    }
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


static int varisaugmented(struct LocVar *var) {
  TString *name = var->varname;
  lua_assert(name != NULL);
  return (*getstr(name) == '(');
}


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
  fs->nlocvars = 0;
  fs->nactvar = 0;
  if (f->name)
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
    fs->sizelocvars = 0;
    a->sizelocvars = fs->sizelocvars;
    a->locvars = NULL;
    fs->locvars = a->locvars;
    fs->sizeupvalues = f->nups;
    a->sizeupvalues = fs->sizeupvalues;
    a->upvalues = luaM_newvector(H, a->sizeupvalues, TString *);
    memset(a->upvalues, 0, a->sizeupvalues * sizeof(TString *));
    fs->upvalues = a->upvalues;
    fs->sizeupvalues = a->sizeupvalues;
  }
  a->sizeactvar = f->maxstacksize;
  a->actvar = luaM_newvector(H, a->sizeactvar, unsigned short);
  memset(a->actvar, 0, a->sizeactvar * sizeof(unsigned short));
  fs->nopencalls = 0;
  fs->nregnotes = 0;
  fs->firstclob = -1;
  fs->firstclobnonparam = -1;
  fs->firstfree = 0;
  fs->lastcallexp = 0;
  fs->curr_constructor = 0;
  /* allocate vectors for instruction and register properties */
  a->sizeinsproperties = f->sizecode; /* flags for each instruction */
  a->insproperties = luaM_newvector(H, a->sizeinsproperties, InstructionFlags);
  memset(a->insproperties, 0, f->sizecode * sizeof(InstructionFlags));
  a->sizeregproperties = f->maxstacksize; /* flags for each register */
  a->regproperties = luaM_newvector(H, f->maxstacksize, SlotDesc);
  memset(a->regproperties, 0, a->sizeregproperties * sizeof(SlotDesc));
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



static void addsibling1(BlockNode *bl1, BlockNode *bl2) {
  lua_assert(bl1 != NULL);
  bl1->nextsibling = bl2;
}

static BlockNode *addblnode1(DFuncState *fs, int startpc, int endpc, int type) {
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  BlockNode *curr, *new_node;
  curr = a->bllist.first;
  new_node = newblnode(H, startpc, endpc, type);
  new_node->next = curr;
  a->bllist.first = new_node;
  if (curr == NULL)
    a->bllist.last = new_node;
  D(lprintf("recording new block node of type %b (%i-%i)\n", new_node, startpc,
            endpc));
  return new_node;
}


/*
** use this version at any point after the root `loop1' call, it appends the new
** node to the list so that fhe first node remains as the function block
*/
static BlockNode *addblnode2(DFuncState *fs, int startpc, int endpc, int type) {
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  BlockNode *curr, *new_node;
  curr = a->bllist.last;
  new_node = newblnode(H, startpc, endpc, type);
  new_node->next = NULL;
  a->bllist.last = new_node;
  lua_assert(curr != NULL);  /* at least 1 block should already exist */
  curr->next = new_node;
  D(lprintf("recording new block node of type %b (%i-%i)\n", new_node, startpc,
            endpc));
  return new_node;
}


/*
** delete a BlockNode node and return the new earliest block based on the
** arguments provided (promote all the block's children to siblings)
*/
static BlockNode *remblnode1(DFuncState *fs, BlockNode *node,
                           BlockNode *prevsibling) {
  BlockNode *child = node->firstchild;
  BlockNode *earliestblock; /* return value */
  lua_assert(prevsibling == NULL || prevsibling->nextsibling == node);
  D(lprintf("deleting erroneous block %B\n", node));
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
    child->nextsibling = node->nextsibling;
  }
  else if (prevsibling != NULL) {
    /* connect the previous sibling and the next sibling */
    prevsibling->nextsibling = node->nextsibling;
    earliestblock = prevsibling;
  }
  else {
    earliestblock = node->nextsibling;
  }
  /* remove BBL from the linked list */
  if (fs->a->bllist.first == node) { /* BBL is first in the list */
    fs->a->bllist.first = node->next;
    if (fs->a->bllist.last == node)
      fs->a->bllist.last = fs->a->bllist.first;
  }
  else {
    BlockNode *iterblock = fs->a->bllist.first;
    while (iterblock->next != node)
      iterblock = iterblock->next;
    lua_assert(iterblock->next == node);
    if (fs->a->bllist.last == node) {
      fs->a->bllist.last = iterblock;
      lua_assert(node->next == NULL);
    }
    iterblock->next = node->next;
  }
  luaM_free(fs->H, node);
  return earliestblock;
}


#if 0
static BlockNode *addbbl2(DFuncState *fs, int startpc, int endpc, int type) {
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  BlockNode *curr, *new;
  curr = a->bllist.last;
  new = newblnode(H, startpc, endpc, type);
  /* the first pass should always create at least 1 block */
  lua_assert(a->bllist.first != NULL);
  lua_assert(a->bllist.last != NULL);
  curr->next = new;
  a->bllist.last = new;
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
      luaO_ptr2str(s, pvalue(o));
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

static void debugblnode(DFuncState *fs, BlockNode *node, int indent) {
  BlockNode *child, *nextsibling;
  int i;
  lua_assert(node != NULL);
  child = node->firstchild;
  nextsibling = node->nextsibling;
  for (i = 0; i < indent; i++)
    lprintf("  ");
  if (indent) lprintf("- ");
  lprintf("(%i-%i) %b ", node->startpc, node->endpc, node);
  if (nextsibling != NULL)
    lprintf("(sibling (%i-%i) %b)\n", nextsibling->startpc, nextsibling->endpc,
            nextsibling);
  else
    lprintf("(NO sibling)\n");
  lua_assert(node->visited == 0);
  node->visited = 1;
  while (child != NULL) {
    debugblnode(fs, child, indent+1);
    child = child->nextsibling;
  }
}


static void debugblocksummary(DFuncState *fs)
{
  BlockNode *node = fs->a->bllist.first;
  lprintf("BLOCK SUMMARY\n"
         "-------------------\n");
  lua_assert(node != NULL);
  lua_assert(node->type == BL_FUNCTION);
  lua_assert(node->nextsibling == NULL);
  debugblnode(fs, node, 0);
  lprintf("-------------------\n");
  while (node != NULL) {
    node->visited = 0;  /* unvisit before pass2 */
    node = node->next;
  }
}


static void debugopenexpr(const OpenExpr *e)
{
  static const char *const typenames[] = {
    "VOID",
    "PRECALL",
    "PRECONCAT",
    "PREFORNUM",
    "PREFORLIST",
    "PRESETLIST",
    "HASHTABLEPREP",
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
    "REG_NOTE_CLOSED",
    "REG_NOTE_UPVALUE",
    "REG_NOTE_NONRELOC",
    "REG_NOTE_CHECKTYPE"
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
  debugblocksummary(fs); lprintf("\n");
  debugopenexprsummary(fs); lprintf("\n");
  debugregnotesummary(fs);
}


static void checktreevisited(BlockNode *node)
{
  BlockNode *child;
  lua_assert(node != NULL);
  child = node->firstchild;
  lua_assert(node->visited);
  while (child != NULL) {
    checktreevisited(child);
    child = child->nextsibling;
  }
}


#else /* !LUA_DEBUG */

#define debugpass1summary(fs)  ((void)(fs))
#define checktreevisited(node)  ((void)(node))

#endif /* LUA_DEBUG */


/*
** Check if the given operation O clobbers register A without depending on what
** was previously in register A. if CHECKDEP is false, the check will not take
** into account dependencies related to registers that both read and clobbered.
*/
static int beginseval(OpCode o, int a, int b, int c, int checkdep) {
  switch (o) {
    /* these operations never depend on what's in A */
    case OP_GETGLOBAL: case OP_GETGLOBAL_MEM:
    case OP_LOADBOOL:
    case OP_LOADK:
    case OP_LOADNIL:
    case OP_GETUPVAL:
    case OP_NEWTABLE:
    case OP_NEWSTRUCT:
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
    case OP_GETSLOT:
    case OP_GETSLOTMT:
    case OP_SELFSLOT:
    case OP_SELFSLOTMT:
    case OP_GETFIELD_MM:
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


static const Instruction *getjumpcontrol (DFuncState *fs, int pc) {
  Instruction *pi = &fs->f->code[pc];
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
    return pi-1;
  else
    return NULL;
}


static int isjumpcontroltest (DFuncState *fs, int pc, int *testedreg) {
  const Instruction *jc = getjumpcontrol(fs, pc);
  int ret = 0;
  if (jc) {
    OpCode o = GET_OPCODE(*jc);
    ret = (o == OP_TEST || o == OP_TEST_R1 || o == OP_TESTSET);
    if (testedreg) *testedreg = GETARG_A(*jc);
  }
  return ret;
}


/*
** get the register(s) tested by a jump control code JC, and store them in R1
** and R2; returns how many registers are tested (1 or 2)
*/
static int getregstested (Instruction jc, int *r1, int *r2) {
  OpCode o = GET_OPCODE(jc);
  lua_assert(testTMode(o));
  if (testAMode(o)) {
    *r1 = GETARG_A(jc);
    return 1;
  }
  else {  /* comparison opcode */
    int numregs=0;
    int b = GETARG_B(jc);
    int c = GETARG_C(jc);
    if (ISK(b)) b = c;
    else numregs++;
    numregs+= !ISK(c);
    *r1 = b;
    *r2 = c;
    return numregs;
  }
}


/*
** Check if the given instruction I begins a temporary expression in register
** FISRTREG. PC is the pc of I. JUMPLIMIT is a pc used to check if a preceding
** jump instruction jumps to somewhere within the temporary expression
** evaluation, meaning it is part of the temporary expression. CODE is the
** instruction array which contains I, used for single look-behinds.
*/
static int beginstempexpr(DFuncState *fs, Instruction i, int pc,
                          int firstreg, int jumplimit, int *nextstart)
{
  const Instruction *code = fs->f->code;
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
      (void)sbx;
      return 0;
    }
    case OP_LOADBOOL:
      /* OP_LOADBOOL may begin an expression, unless it is a label */
      if (test_ins_property(fs, pc, INS_BOOLLABEL))
        return 0;
      /* this instruction may not be marked as INS_BOOLLABEL yet */
      else if ((pc-1) >= 0) {
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
            const Instruction *jumpcontrol = getjumpcontrol(fs, pc-1);
            int jumpoffs = GETARG_sBx(code[pc-1]);
            int target = pc + jumpoffs;
            if (jumpcontrol == NULL || target > jumplimit || jumpoffs <= 0)
              return 1; /* the previous jump is not part of this expression */
            else {
              /* jumpcontrol != NULL && target <= jumplimit && jumpoffs >= 0 */
              int testedreg;
              if (isjumpcontroltest(fs, pc-1, &testedreg)) {
                if (testedreg >= firstreg)
                  /* jump control tests FIRSTREG; the jump is part of this
                     expression */
                  return 0;
                else
                  /* jump control tests an earlier resgister; if the jump target
                     is the endpc, it is not part of this expression */
                  return (target == jumplimit);
              }
              else  /* comparison op */
                /* comparison ops that are part of this expression will emit
                   jumps to boolean labels before endpc, so checking target
                   against the endpc is sufficient */
                return (target == jumplimit);
            }
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


struct blockstates1 {
  BlockState *block, *branch;
  BlockNode *blocksibling;
  int stkbase;  /* the stack position when the current loop was entered */
  lu_byte upval;  /* this refers to the current loop; does it have upvalues */
  lu_byte haveblocksibling;
};


/*
** `Code Analyzer' - the main structure used in the first pass. It populates the
** `insproperties' array of its corresponding `DFuncState'.
*/
typedef struct CodeAnalyzer {
  /* a record of all encountered loops is saved implicitly on the callstack;
     these values hold the bounds of the current loop */
  struct stat1 curr; /* the start and end of the current block node */
  int pc;  /* current pc */
  /* most of these values could be local to `whilestat1', but are kept in this
     structure to avoid allocating new copies on every recursive call */
  lu_byte prevTMode;  /* previous pc is a test instruction */
  lu_byte inopenexpr;
  struct {
    int endpc, reg;
  } testset;  /* the current OP_TESTSET expression */
  const Instruction *code;  /* f->code */
} CodeAnalyzer;


#ifdef LUA_DEBUG
#define printinsn1(pc,i) lprintf("pc %i: %O\n",pc,i)
#else /* !LUA_DEBUG */
#define printinsn1(pc,i) ((void)0)
#endif /* LUA_DEBUG */


static void scanforhashitems1(CodeAnalyzer *ca, DFuncState *fs, OpenExpr *e,
                        int minhashsize, int firstreg, BlockNode **chain);


static void addregnote1(DFuncState *fs, int note, int pc, int reg)
{
  if (test_reg_property(fs, reg, REG_HASNOTE) == 0) {
    newregnote(fs, note, pc, reg);
    set_reg_property(fs, reg, REG_HASNOTE);
  }
}


static void applyregnote(DFuncState *fs, int firstreg, int pc, OpCode o, int a,
                         int b, int c)
{
  if (o != OP_LOADNIL && beginseval(o, a, b, c, 0)) {
    /* check if B and C are registers and are less than FIRSTREG */
    if ((getBMode(o) == OpArgR || (getBMode(o) == OpArgRK && !ISK(b))) &&
        b < firstreg)
      addregnote1(fs, REG_NOTE_NONRELOC, pc, b);
    if ((getCMode(o) == OpArgR || (getCMode(o) == OpArgRK && !ISK(c))) &&
        c < firstreg)
      addregnote1(fs, REG_NOTE_NONRELOC, pc, c);
  }
}


/*
** finds and marks the beginning of an open expression of type KIND; recursing
** into nested open expressions is not necessary, as only the start/end pc of
** root open expressions are needed by the second pass analyzer (VOIDPREP calls
** are treated differently than other calls, however, so recursion does happen
** in that case since VOIDPREP expressions are not recorded)
*/
static void openexpr1(CodeAnalyzer *ca, DFuncState *fs, int firstreg, int kind,
                      struct LocVar *locvar)
{
  const Instruction *code = ca->code;
  int endpc = ca->pc;
  int nextpossiblestart = endpc;
  int pc;
  /* VOIDPREP calls already have ca->pc at the correct value; other kinds have
     a particular opcode that ends the expression (e.g. OP_RETURN), and that
     opcode has already been visited in those cases */
  if (kind != VOIDPREP) ca->pc--;
  else {
    /* if this is a local variable expression and the local variable ends 1
       after the endpc of the expression, such as the following case:
          if a then
              local a = 12;
          end
       keep the endpc as it is (in the above case, the LOADK instruction), so
       that the jump for `if a' does not get included as part of the expression;
       this has the side effect of a false positive in cases such as the
       following:
          local a = a and 12; -- generates same code as above
       the above code would be detected as an if-block because the local
       variable `a' ends on the same pc as the jump target, so the jump will
       not be counted as part of the expression. This is usually okay because
       the decompiler will still generate matching debug for it anyway (whether
       a branch or a confitional expression, the variable will have the same
       lifespan in either case); it is a problem if the conditional tests a
       constant, e.g.:
          local a = 1 or 5;
       this would be detected as an if-statement, but it would generate
       different code when re-compiled because of optimization; constant tests
       are optimized in if-statements, but not in conditional expressions; this
       is handled after the code analysis is done */
    if (locvar == NULL || locvar->endpc != endpc+1)
      endpc++;  /* endpc needs to be (ca->pc+1) in all cases */
  }
  ca->inopenexpr++;
  for (pc = ca->pc; (pc = ca->pc) >= 0; ca->pc--) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    switch (o) {
      case OP_LOADBOOL:
        if (c) {
          set_ins_property(fs, pc, INS_BOOLLABEL);
          set_ins_property(fs, pc+1, INS_BOOLLABEL);
        }
        break;
      case OP_CALL:
      case OP_CALL_I:
      case OP_CALL_I_R1:
      case OP_CALL_C:
      case OP_CALL_M:
      case OP_CONCAT:
        if (kind == VOIDPREP) {
          openexpr1(ca, fs, a, o == OP_CONCAT ? CONCATPREP : CALLPREP, NULL);
          goto postrecursion;
        }
        else
        /* a nested end-of-expression code can clobber FIRSTREG, but it does not
           begin an open expression... unless the previous code is OP_LOADNIL
           and OP_LOADNIL clobbers a register less than FIRSTREG; if that is the
           case, do not mark OP_LOADNIL as the start of this expression;
           instead, mark whatever NEXTPOSSIBLESTART was, which is why it is
           still updated here */
          nextpossiblestart = pc;
        continue;
      case OP_SETLIST:
        if (kind == VOIDPREP) {
          openexpr1(ca, fs, a, SETLISTPREP, NULL);
          postrecursion:
          pc = ca->pc;
          i = code[pc];
          o = GET_OPCODE(i);
          a = GETARG_A(i);
          b = GETARG_B(i);
          c = GETARG_C(i);
        }
        break;
      case OP_NEWTABLE:
        if (kind == VOIDPREP) {
          CHECK(fs, b == 0, "no OP_SETLIST found for OP_NEWTABLE even though "
                  "OP_NEWTABLE has non-zero amount of list items");
          if (c) {
            /* walk back up the code vector to find the earliest possible end of
               this constructor */
            scanforhashitems1(ca, fs, newopenexpr(fs, a, pc, -1, HASHTABLEPREP),
                              luaO_fb2int(c-1)+1, a, NULL);
          }
          else
            newopenexpr(fs, a, pc, pc, EMPTYTABLE);
        }
        break;
      default:
        break;
    }
    if (kind == SETLISTPREP && o == OP_NEWTABLE && a == firstreg)
      break;  /* found start of table */
    applyregnote(fs, firstreg, pc, o, a, b, c);
    if (beginstempexpr(fs, i, pc, firstreg, endpc, &nextpossiblestart)) {
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
  if (pc < 0) pc = 0;
  if (kind != VOIDPREP) {
    newopenexpr(fs, firstreg, pc, endpc, kind);
  }
  ca->pc = pc;
  ca->inopenexpr--;
  if (ca->inopenexpr == 0) {
    int i;
    /* clear REG_HASNOTE on all registers */
    for (i = 0; i < fs->a->sizeregproperties; i++)
      unset_reg_property(fs, i, REG_HASNOTE);
  }
}


/*
** traverse a local variable initialized expression
*/
static void locvarexpr1(CodeAnalyzer *ca, DFuncState *fs, int nvars,
                        struct LocVar *var)
{
  int reg;  /* register of first local variable that is initialized */
  lua_assert(nvars > 0);
  lua_assert(fs->nactvar >= nvars);
  reg = fs->nactvar-nvars;
  D(lprintf("entering VOIDPREP for local variable in reg %d\n", reg));
  openexpr1(ca, fs, reg, VOIDPREP, var);
  fs->nactvar -= nvars;
  set_ins_property(fs, ca->pc, INS_LOCVAREXPR);
}


/*
** traverse a stored open expression
*/
static int storeexpr1(CodeAnalyzer *ca, DFuncState *fs, int reg)
{
  DecompState *D = fs->D;
  if (ISK(reg))
    return 0;
  if (D->usedebuginfo) {
    if (reg < fs->nactvar)
      return 0;  /* assigning a local variable */
    /* else go through */
  }
  else {
    return 0;  /* todo: not using debug info */
  }
  lua_assert(isregvalid(fs, reg));
  D(lprintf("entering VOIDPREP for stored register %d\n", reg));
  ca->pc--;  /* advance over the store code */
  openexpr1(ca, fs, reg, VOIDPREP, NULL);
  return 1;
}


/*
** remove any block nodes that start at PC
*/
static void clearblocksatpc1(DFuncState *fs, BlockNode **chain, int pc)
{
  BlockNode *node;
  lua_assert(chain != NULL);
  lua_assert(ispcvalid(fs, pc));
  lua_assert(ispcvalid(fs, pc+1));
  node = *chain;
  while (node && node->startpc == pc) {
    BlockNode *nextsibling = node->nextsibling;
    remblnode1(fs, node, NULL);
    node = nextsibling;
  }
  /* make sure none of these markings are on this pc */
  unset_ins_property(fs, pc, INS_BRANCHFAIL);
  unset_ins_property(fs, pc+1, INS_BRANCHBEGIN);
  unset_ins_property(fs, pc, INS_BRANCHPASS);
  unset_ins_property(fs, pc, INS_BLOCKEND);
  *chain = node;
}


/*
** readjust NEXTBRANCH after removing block nodes in an open or conditional
** expression
*/
static void adjustnextbranch1(DFuncState *fs, BlockNode *nextsibling,
                              BlockNode **chain, int *nextbranchtarget)
{
  BlockNode *nextbranch;
  lua_assert(chain != NULL);
  lua_assert(nextbranchtarget != NULL);
  /* also update NEXTBRANCH */
  if (nextsibling && nextsibling->type == BL_IF) {
    nextbranch = nextsibling;
    lua_assert(nextbranch->startpc-1 >= 0 &&
               nextbranch->startpc-1 < fs->f->sizecode);
    lua_assert(GET_OPCODE(fs->f->code[nextbranch->startpc-1]) == OP_JMP);
    *nextbranchtarget = nextbranch->startpc-1 + 1 +
                       GETARG_sBx(fs->f->code[nextbranch->startpc-1]);
  }
  else {
    nextbranch = NULL;
    *nextbranchtarget = -1;
  }
  *chain = nextbranch;
}


/*
** finds the earliest possible endpc for a SETLIST open expression with only
** hash items (the exact number cannot be known if operand C is graeter than 16
** due to the conversion with `luaO_fb2int')
*/
static void scanforhashitems1(CodeAnalyzer *ca, DFuncState *fs, OpenExpr *e,
                        int minhashsize, int firstreg, BlockNode **chain)
{
  const Instruction *code = ca->code;
  int pc;  /* iterator */
  int numhashitems = 0;  /* number of hash items found so far */
  lua_assert(minhashsize > 0);
  lua_assert(e != NULL);
  lua_assert(e->kind == HASHTABLEPREP);
  lua_assert(ispcvalid(fs, e->startpc));
  lua_assert(GET_OPCODE(code[e->startpc]) == OP_NEWTABLE);
  lua_assert(GETARG_B(code[e->startpc]) == 0);
  for (pc = e->startpc+1; pc < fs->f->sizecode-1; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    if (chain != NULL)
      clearblocksatpc1(fs, chain, pc);
    applyregnote(fs, firstreg, pc, o, a, b, c);
    /* check if this opcode sets a table index */
    if (o == OP_SETFIELD ||
             o == OP_SETFIELD_R1 ||
             o == OP_SETTABLE ||
             o == OP_SETTABLE_BK ||
             o == OP_SETTABLE_N ||
             o == OP_SETTABLE_N_BK ||
             o == OP_SETTABLE_S ||
             o == OP_SETTABLE_S_BK) {
      /* check if a RegNote entry can be added for the source register */
      if (c < firstreg) {
        addregnote1(fs, REG_NOTE_NONRELOC, pc, c);
      }
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


/*
** a structure which holds a snapshot of the previous and next BlockNode for a
** given pc, as well as a root node for basing the search on
*/
struct siblingsnapshot1 {
  BlockNode *startnode;  /* the root node to start searching at */
  BlockNode *prevsibling, *nextsibling;  /* the snapshot nodes */
};


/*
** intializer to call before entering a loop
*/
static void initsiblingsnapshot1(struct siblingsnapshot1 *s, BlockNode *start)
{
  s->prevsibling = s->nextsibling = NULL;
  s->startnode = start;
}


/*
** function to call after each loop iteration
*/
static void advancesiblingsnapshot1(struct siblingsnapshot1 *s)
{
  s->startnode = s->prevsibling;
}


/*
** calculate a snapshot of the previous and next block nodes for a given pc,
** using the current STARTNODE as the base of the search
*/
static void getsiblingsforpc1(struct siblingsnapshot1 *s, int pc)
{
  BlockNode *prevnode = NULL;
  /* start at STARTNODE and find the first block node which is after PC */
  BlockNode *node = s->startnode;
  while (node != NULL && blstartpc(node) < pc) {
    lua_assert(node->nextsibling != node);
    prevnode = node;  /* update PREVSIBLING */
    node = node->nextsibling;
  }
  s->prevsibling = prevnode;
  s->nextsibling = node;
}


/*
** apply a sibling snapshot to a block node's child/sibling relationships
*/
static void fixsiblingchain1(BlockNode *node, struct siblingsnapshot1 *s)
{
  BlockNode *lastchild = s->prevsibling;
  BlockNode *nextsibling = s->nextsibling;
  BlockNode *firstsibling = s->startnode;
  lua_assert(node != NULL);
  if (lastchild) {
    node->firstchild = firstsibling;
    /* fix sibling chain for last child */
    lastchild->nextsibling = NULL;
  }
  node->nextsibling = nextsibling;
}


/*
** correct a NEXTSIBLING variable and a block node's sibling chain
*/
static void rescansiblingchain1(BlockNode *node, BlockNode **chain) {
  struct siblingsnapshot1 s;
  lua_assert(node != NULL);
  initsiblingsnapshot1(&s, chain != NULL ? *chain : node->nextsibling);
  getsiblingsforpc1(&s, node->endpc+1);
  fixsiblingchain1(node, &s);
  if (chain != NULL)
    *chain = s.nextsibling;
}


/* assumes (br->nested > 0) check before calling */
#define prevbranchstate1(brstate) ((brstate) - 1 - (brstate)->u.br.withblock)

/*
** return the BlockState that immediately encloses BRANCH if it exists
*/
static BlockState *getbranchparent1(BlockState *branch)
{
  lua_assert(branch != NULL);
  return branch->nested ? prevbranchstate1(branch) : NULL;
}


/*
** return the root BlockState in the same group as BRANCH
*/
static BlockState *getbranchroot1(BlockState *branch)
{
  short nested;
  lua_assert(branch != NULL);
  nested = branch->nested;
  lua_assert(nested >= 0);
  for (; nested > 0; nested--)
    branch = prevbranchstate1(branch);
  return branch;
}


/*
** return the most recent pending branch context from the block state stack
*/
static BlockState *getlastbranch1(DFuncState *fs, struct blockstates1 *s)
{
  if (s->branch != NULL)
    return s->branch;
  else {
    BlockState *branch;
    BlockState *block = s->block;
    if (block == NULL || block->u.bl.lastbranch == 0)
      return NULL;
    lua_assert(block->u.bl.lastbranch >= 0);
    branch = block - block->u.bl.lastbranch;
    lua_assert(branch >= &fs->a->pendingstk.u.s1[s->stkbase] &&
               branch < &fs->a->pendingstk.u.s1[fs->a->pendingstk.used]);
    lua_assert(branch->branch);
    return branch;
  }
  (void)fs;
  (void)getbranchroot1;
}


/*
** return the first parent do-block state of a branch context or NULL
*/
static BlockState *getparentblock1(DFuncState *fs, BlockState *branch)
{
  lua_assert(branch != NULL);
  lua_assert(branch->branch);
  if (branch->u.br.parentblock)
    return &fs->a->pendingstk.u.s1[branch->u.br.parentblock-1];
  else
    return NULL;
}


/*
** pop a block state from the stack
*/
static BlockState *popblockstate1(DFuncState *fs)
{
  lua_assert(fs->a->pendingstk.used > 0);
  return &fs->a->pendingstk.u.s1[--fs->a->pendingstk.used];
}


/*
** return the previous block state given a BlockState BL and a stack limit from
** S->STKBASE; if the position of BL is above the limit, the BlockState at the
** preceding position is returned, otherwise NULL is returned
*/
static BlockState *prevblockstate1(DFuncState *fs, struct blockstates1 *s,
                                   BlockState *bl)
{
  int base = s->stkbase;
  int index = bl - fs->a->pendingstk.u.s1;
  lua_assert(base >= 0);
  lua_assert(fs->a->pendingstk.used >= base);
  lua_assert(index >= 0 && index < fs->a->pendingstk.used);
  if (index <= base)
    return NULL;
  else
    return bl-1;
}

/*
** return the do-block state that immediately encloses BL if it exists
*/
static BlockState *prevdoblockstate1(BlockState *bl)
{
  return bl->nested ? bl-1 : NULL;
}


/*
** pop the last block state from the stack and update the state pointers
*/
static BlockState *updateblockstates1(DFuncState *fs, struct blockstates1 *s) {
  BlockState *oldstate = popblockstate1(fs);
  BlockState *top = oldstate;
  int base = s->stkbase;
  if (top->branch && top->u.br.withblock)
    /* also pop the block context that was created with the branch */
    top = popblockstate1(fs);
  lua_assert(base >= 0);
  lua_assert(fs->a->pendingstk.used >= base);
  if (fs->a->pendingstk.used == base) {
    s->block = s->branch = NULL;
    return oldstate;
  }
  lua_assert(fs->a->pendingstk.used > base);
  top--;
  if (top->branch) {
    s->block = NULL;
    s->branch = top;
  }
  else {
    s->block = top;
    s->branch = NULL;
  }
  return oldstate;
}


/*
** create a new do-block context and push it to the block state stack
*/
static BlockState *pushblock1(DFuncState *fs, struct blockstates1 *s, int endpc,
                              int reg, lu_byte upval)
{
  /* parent block state snapshots are only needed when a nested branch occludes
     part of the parent block; until a `pushbranch1' call happens, the snapshot
     remains unused */
  struct BlockStateControl parentsnapshot = {-1, NULL, NULL};
  struct BlockState *block;
  struct BlockState *prev = s->block;
  short nested, lastbranch;
  if (prev != NULL) {
    nested = prev->nested+1;
    lastbranch = prev->u.bl.lastbranch+1;
  }
  else {
    nested = 0;
    if (s->branch == NULL)
      lastbranch = 0;
    else
      lastbranch = fs->a->pendingstk.used-(s->branch-fs->a->pendingstk.u.s1);
  }
  block = newblockstate(fs);
  UNUSED(prev);  /* pointer may be invalid past this point */
  block->nextsibling = NULL;
  block->result = NULL;
  block->firstblock = NULL;
  block->nested = nested;
  block->branch = 0;
  block->endpc = endpc;
  block->u.bl.rcnt.startpc = endpc;
  block->u.bl.rcnt.firstchild = NULL;
  block->u.bl.rcnt.prevsibling = NULL;
  block->u.bl.pcnt = block->u.bl.rcnt;
  block->parentsnapshot = parentsnapshot;
  block->u.bl.firstsibling = NULL;
  block->u.bl.reg = reg;
  block->u.bl.lastbranch = lastbranch;
  block->u.bl.upval = upval;
  block->u.bl.opaque = 0;
  block->u.bl.loop = 0;
  s->block = block;
  s->branch = NULL;
  return block;
}


/*
** create a new branch context and push it to the block state stack
*/
static BlockState *pushbranch1(DFuncState *fs, struct blockstates1 *s,
                               int midpc, int endpc, int jumptarget)
{
  /* default parent block state snapshpt to use if there is no parent block */
  struct BlockStateControl parentsnapshot = {-1, NULL, NULL};
  struct BlockState *branch, *parentblock;
  struct BlockState *prev = s->branch;
  short nested;
  int optimalexit, parentbranchwithblock;
  if (prev != NULL) {
    lua_assert(prev->branch);
    nested = prev->nested+1;
    optimalexit = (endpc+1 == prev->u.br.midpc) ? prev->u.br.optimalexit :
                  jumptarget;
    /* the next parent block is that of the parent branch state */
    parentblock = getparentblock1(fs, prev);
    if (prev->u.br.withblock)
      parentbranchwithblock = prev-fs->a->pendingstk.u.s1+1;
    else
      parentbranchwithblock = prev->u.br.parentbranchwithblock;
  }
  else {  /* root */
    nested = 0;
    optimalexit = jumptarget;
    /* maybe there is currently a block pending; that is the parent block */
    parentblock = s->block;
    parentbranchwithblock = 0;
  }
  if (parentblock != NULL) {  /* there is a parent block state */
    BlockState *lastparentblock = parentblock;
    /* save the current state of the first parent to assign to this branch */
    parentsnapshot = parentblock->u.bl.pcnt;
    /* because all pending block states are corrected before `pushbranch1' is
       called, the rest of the parents need to update their snapshots; iterate
       through the rest of the parents and update each snapshot; note that the
       potential state is used for the snapshot because the blocks may or may
       not be occluded by a branch block (when else-blocks are encountered, they
       may actually be if-false-blocks, in which case there is no occlusion
       being done by an if-part, so the potential state being correct means that
       it may be valid, while an incorrect potential state is when its startpc
       is inside a branch block that has been identified, and thus the state is
       definitely not valid) */
    while ((parentblock = prevdoblockstate1(parentblock)) != NULL) {
      lastparentblock->parentsnapshot = parentblock->u.bl.pcnt;
      lastparentblock = parentblock;
    }
  }
  branch = newblockstate(fs);
  UNUSED(parentblock);
  UNUSED(prev);  /* pointer may be invalid past this point */
  branch->nextsibling = NULL;
  branch->result = NULL;
  branch->firstblock = NULL;
  branch->nested = nested;
  branch->branch = 1;
  branch->endpc = endpc;
  branch->parentsnapshot = parentsnapshot;
  branch->u.br.prevsibling2 = NULL;
  branch->u.br.startpc = -1;  /* startpc of the if-part is unknown right now */
  branch->u.br.midpc = midpc;
  branch->u.br.target1 = -1;
  branch->u.br.target2 = jumptarget;
  branch->u.br.optimalexit = optimalexit;
  branch->u.br.parentblock = 0;
  branch->u.br.parentbranchwithblock = parentbranchwithblock;
  branch->u.br.withblock = 0;
  s->branch = branch;
  s->block = NULL;
  return branch;
}


/*
** create a do-block node from a do-block state
*/
static BlockNode *createdoblock1(DFuncState *fs, int startpc, int endpc)
{
  BlockNode *node;
  lua_assert(ispcvalid(fs, startpc));
  lua_assert(ispcvalid(fs, endpc));
  node = addblnode1(fs, startpc, endpc, BL_DO);
  set_ins_property(fs, startpc, INS_DOSTAT);
  set_ins_property(fs, endpc, INS_BLOCKEND);
  return node;
}


static BlockNode *createdoblockfromstate1(DFuncState *fs, BlockState *bl)
{
  int startpc = bl->u.bl.rcnt.startpc;
  BlockNode *node = createdoblock1(fs, startpc, bl->endpc);
  newregnote(fs, REG_NOTE_CLOSED, startpc, bl->u.bl.reg);
  return node;
}


static BlockNode *createdoblock2(DFuncState *fs, int startpc, int endpc)
{
  BlockNode *node;
  lua_assert(ispcvalid(fs, startpc));
  lua_assert(ispcvalid(fs, endpc));
  node = addblnode2(fs, startpc, endpc, BL_DO);
  set_ins_property(fs, startpc, INS_DOSTAT);
  set_ins_property(fs, endpc, INS_BLOCKEND);
  return node;
}


/*
** finalize a do-block context by creating a block node from its current state
*/
static BlockNode *finalizeblock1(DFuncState *fs, BlockState *block)
{
  BlockNode *node;
  lua_assert(block != NULL);
  lua_assert(block->branch == 0);
  if (block->u.bl.loop)  /* this block state was for a loop */
    return NULL;
  node = createdoblockfromstate1(fs, block);
  node->firstchild = block->u.bl.rcnt.firstchild;
  node->nextsibling = block->nextsibling;
  if (block->u.bl.rcnt.prevsibling) {
    /* connect previous sibling to this node */
    block->u.bl.rcnt.prevsibling->nextsibling = node;
  }
  return node;
}


/*
** finalize a branch context (NODE1 is the if-part block node)
*/
static BlockNode *finalizebranch1(DFuncState *fs, BlockState *branch,
                                  BlockNode *node1)
{
  BlockState *parent;
  BlockNode *node2;  /* the else-part */
  BlockNode *firstresult;
  int midpc;
  lua_assert(branch != NULL);
  midpc = branch->u.br.midpc;
  lua_assert(midpc > 0);  /* midpc must always come after a jump */
  node2 = branch->result;
  lua_assert(node2 != NULL);
  lua_assert(node2->type == BL_ELSE);
  parent = getbranchparent1(branch);
  if (node1 != NULL) {
    lua_assert(node1->type == BL_IF);
    lua_assert(branch->u.br.target1 != -1);
    firstresult = node1;
    if (node1->endpc != midpc-1 || node1->nextsibling != node2)
      goto badelsebranch;
  }
  else {  /* no if-part; change else-part to a single if (i.e. `if false') */
    firstresult = node2;
    badelsebranch:
    /* the condition below means the following: this branch is enclosed in an
       immediate parent branch, this branch ends at the end of the if-part of
       the parent, the exit-jump of this branch is optimized to target the
       exit pc of a parent branch, the optimized exit pc is not an earlier pc
       (such as the start of an enclosing while-loop), and the parent branch
       does not have an opaque pending block state */
    if (parent != NULL && branch->endpc+1 == parent->u.br.midpc &&
        branch->u.br.target2 == branch->u.br.optimalexit &&
        branch->u.br.optimalexit > node2->endpc && parent->u.br.withblock == 0)
    {
      /* todo: decide whether to use the optimal exit as the endpc */;
    }
    node2->type = BL_IF;
    /* there is not if-part; unmark block ending */
    unset_ins_property(fs, midpc-1, INS_BLOCKEND);
    /* handle a preceding do-block with upvalues (check against 1 specifically,
       as a value of 2 indicates there is a block but a node should not be
       created) */
    if (branch->u.br.withblock == 1) {
      BlockNode *node;
      BlockState *pblock = branch-1/*prevblockstate1(fs, &s, branch)*/;
      lua_assert(pblock->u.bl.opaque);
      lua_assert(pblock->branch == 0);
      lua_assert(pblock->u.bl.upval);
      node = createdoblockfromstate1(fs, pblock);
      firstresult = node;
      /* if the earliest block before this is not the else-part, make it a child
         of this one */
      if (pblock->u.bl.rcnt.firstchild != node2)
        node->firstchild = pblock->u.bl.rcnt.firstchild;
      node->nextsibling = node2;
      /* connect the preceding block to this do-block if it exists */
      if (pblock->u.bl.rcnt.prevsibling) {
        pblock->u.bl.rcnt.prevsibling->nextsibling = node;
      }
      if (node->firstchild != NULL) {
        BlockNode *child = node->firstchild;
        /* find the last child and disconnect it from the else-part */
        while (child->nextsibling != NULL && child->nextsibling != node2)
          child = child->nextsibling;
        child->nextsibling = NULL;
      }
      branch->u.br.prevsibling2 = NULL;  /* already was handled here */
    }
    /* the if-part endpc may have defaulted to the pc just before the original
       startpc of the else-part, but now the else-part startpc has changed, and
       the if-part endpc needs to be adjusted in case it overlaps with the
       else-part */
    if (node1 != NULL && node1->endpc+1 >= node2->startpc) {
      /* todo: does the node2 endpc actually change? */
    }
    /* correct sibling and child relationships for the branch node */
    rescansiblingchain1(node2, NULL);
    /* todo: handle elseprevsibling */
    if (branch->u.br.prevsibling2 != NULL) {
      BlockNode *prevsibling = branch->u.br.prevsibling2;
      prevsibling->nextsibling = node2;
      if (prevsibling->type == BL_ELSE && prevsibling->endpc == node2->startpc)
      {
        /*prevsibling->endpc = node2->endpc;*/
      }
      /* todo: when is this necessary? */
      rescansiblingchain1(prevsibling, NULL);
    }
    if (branch->u.br.parentbranchwithblock) {
      BlockState *br = &fs->a->pendingstk.u.s1
      [branch->u.br.parentbranchwithblock-1];
      lua_assert(br->branch);
      lua_assert(br->u.br.withblock);
      if (br->u.br.withblock == 1 && br->u.br.midpc-2 == branch->endpc) {
        BlockState *block = br-1;
        lua_assert(block->branch == 0);
        lua_assert(block->u.bl.opaque);
        lua_assert(block->u.bl.upval);
        lua_assert(block->endpc == branch->endpc);
        if (block->u.bl.rcnt.startpc == midpc)
          /* make it still true, but a BlockNode does not need to be created for
             it (this if-false block is sufficient to generate matching code) */
          br->u.br.withblock = 2;
      }
    }
  }
  return firstresult;
}


/*
** finalize the current branch context and pop it from the pending stack
*/
static BlockNode *popbranch1(DFuncState *fs, struct blockstates1 *s,
                       BlockNode *node1, BlockNode **siblingchain)
{
  BlockNode *result;
  BlockState *oldbranch;
  lua_assert(s->branch != NULL);
  result = finalizebranch1(fs, s->branch, node1);
  if (siblingchain != NULL) *siblingchain = result;
  oldbranch = updateblockstates1(fs, s);
  if (s->block != NULL) {
    int branchstartpc = node1 ? oldbranch->u.br.startpc : oldbranch->u.br.midpc;
    int branchendpc = oldbranch->endpc;
    if (s->block->u.bl.pcnt.startpc <= branchendpc &&
        s->block->u.bl.pcnt.startpc >= branchstartpc)
      s->block->u.bl.pcnt = oldbranch->parentsnapshot;
  }
  return result;
}


static int updateblockstatenodes1(BlockState *block, BlockNode *node)
{
  lua_assert(node != NULL);
  lua_assert(block != NULL);
  if (block->u.bl.pcnt.startpc > node->endpc &&
      (block->u.bl.pcnt.prevsibling == NULL ||
       block->u.bl.pcnt.prevsibling->endpc < node->startpc)) {
    block->u.bl.pcnt.prevsibling = node;
    return 1;
  }
  else if (block->u.bl.pcnt.startpc <= blstartpc(node) &&
      (block->u.bl.pcnt.firstchild == NULL ||
       block->u.bl.pcnt.firstchild->startpc > node->endpc)) {
    block->u.bl.pcnt.firstchild = node;
    return 2;
  }
  return 0;
}


static void postfinalizeblock1(BlockState *block, BlockNode *result,
                               BlockNode **siblingchain)
{
  BlockState *parentblock;
  int startpc;
  lua_assert(block != NULL);
  lua_assert(block->branch == 0);
  lua_assert(result != NULL);
  lua_assert(siblingchain != NULL);
  parentblock = prevdoblockstate1(block);
  startpc = block->u.bl.rcnt.startpc;
  if (*siblingchain == NULL || startpc <= blstartpc(*siblingchain))
    *siblingchain = result;
  if (parentblock != NULL)
    updateblockstatenodes1(parentblock, result);
}


static void postfinalizebranch1(DFuncState *fs, BlockState *branch,
                                BlockNode *result, BlockNode *earliestsibling)
{
  BlockState *block = getparentblock1(fs, branch);
  int n = 0;
  lua_assert(result != NULL);
  /* note: RESULT can be a BL_DO block */
  if (result->type == BL_IF && earliestsibling != NULL) {
    /* get the preceding sibling of RESULT */
    BlockNode *prevsibling = earliestsibling;
    while (prevsibling != NULL && prevsibling->nextsibling != result)
      prevsibling = prevsibling->nextsibling;
    /* when 2 branches have the same exit PC, the earlier one can be inferred as
       having an optimized exit, and actually ending before the second one
       begins, where the exit originally would have jumped to the exit-jump of
       the second branch, thus allowing for optimization, but if the second
       branch is an IF-branch, the first branch cannot logically end on the jump
       instruction that exits the second branch, because that jump is part of
       the second branch and cannot be part of both branches; in this case, the
       first branch must enclose the second branch as a child block */
    if (prevsibling != NULL && prevsibling->type == BL_IF &&
        /* the startpc of an if-block is 1 after the JMP instruction; if
           PREVSIBLING ends on that jump, the relationship is illogical */
        prevsibling->endpc+1 == result->startpc) {
      /* make PREVSIBLING the parent of RESULT; it encloses RESULT and must use
         its endpc */
      prevsibling->endpc = result->endpc;
      recalcemptiness(prevsibling);
      /* PREVSIBLING acquires the next sibling of RESULT */
      prevsibling->nextsibling = result->nextsibling;
      if (prevsibling->firstchild != NULL) {
        /* get the last child of PREVSIBLING to append RESULT to the chain */
        BlockNode *child = prevsibling->firstchild;
        while (child != NULL && child->nextsibling != NULL)
          child = child->nextsibling;
        child->nextsibling = result;
      }
      /* PREVSIBLING has no children; simply make RESULT its child */
      else prevsibling->firstchild = result;
      /* update RESULT now that it is c child of PREVSIBLING */
      result->nextsibling = NULL;
      /* now the block to process is PREVSIBLING as it encloses RESULT */
      result = prevsibling;
    }
  }
  for (; block != NULL; block = prevdoblockstate1(block)) {
    n |= updateblockstatenodes1(block, result);
    /* if a FIRSTCHILD and PREVSIBLING field have both been updated, stop */
    if (n == 3) break;
  }
}


/*
** unwind the pending block stack to the local base position for the current
** loop
*/
static void finalizependingblocks1(DFuncState *fs, struct blockstates1 *s,
                                   BlockNode **siblingchain)
{
  BlockNode *nextsibling = *siblingchain;
  lu_byte prevbranch = 0;
  lua_assert(s->stkbase >= 0 && s->stkbase <= fs->a->pendingstk.used);
  while (fs->a->pendingstk.used > s->stkbase) {
    BlockNode *result;
    if (s->branch != NULL) {
      BlockState *branch = s->branch;
      /*int i;*/
      result = finalizebranch1(fs, branch, NULL);
      postfinalizebranch1(fs, branch, result, nextsibling);
      /* the hierarchical structure for branches that have no if-parts is
         inverse to the stack, so the top item is actually the outermost (or
         earliest) branch in its group; therefore, if this was the first branch
         in its group, update nextsibling, otherwise, it is already the earliest
         block so far */
      if (prevbranch == 0) {
        if (nextsibling == NULL || result->startpc <= blstartpc(nextsibling)) {
          nextsibling = result;
        }
      }
      prevbranch = 1;
    }
    else {
      BlockState *block = s->block;
      lua_assert(block != NULL);
      block->u.bl.rcnt = block->u.bl.pcnt;
      result = finalizeblock1(fs, block);
      if (result != NULL)
        postfinalizeblock1(block, result, &nextsibling);
      prevbranch = 0;
    }
    updateblockstates1(fs, s);  /* pop finalized block state */
  }
  *siblingchain = nextsibling;
  lua_assert(s->branch == NULL && s->block == NULL);
}


/*
** update the state of one or more pending blocks for a given pc
*/
static int recordblockstateforpc1(DFuncState *fs, BlockState *bl, LocVar *var,
                int pc, int p, struct siblingsnapshot1 *s, int startpclimit)
{
  BlockNode *nextsibling = s->nextsibling, *prevsibling = s->prevsibling;
  int updatedblock = 0;
  lua_assert(ispcvalid(fs, pc));
  lua_assert(bl != NULL);
  lua_assert(bl->branch == 0);
  if (nextsibling != NULL) {
    lua_assert(nextsibling->startpc >= pc);
  }
  if (test_ins_property(fs, pc, INS_CLOBBER)) {
    int regtocheck;
    int updatedchild = 0;
    OpCode o = GET_OPCODE(fs->f->code[pc]);
    int a = GETARG_A(fs->f->code[pc]);
    int b = GETARG_B(fs->f->code[pc]);
    struct BlockStateControl *blcnt = p ? &bl->u.bl.pcnt : &bl->u.bl.rcnt;
    struct BlockStateControl *lastblcnt;
    if (fs->D->usedebuginfo)
      regtocheck = (var != NULL) ? cast_int(fs->nactvar) : -1;
    else
      regtocheck = a;
    if (isregvalid(fs, regtocheck)) {
      int blreg = bl->u.bl.reg;
      int usesprevnil = (o == OP_LOADNIL && a < blreg && blreg <= b);
      if (blreg == regtocheck || (o == OP_LOADNIL && blreg <= b)) {
        /* this block is now caught up to the current pc */
        blcnt->startpc = pc+usesprevnil;
        if (nextsibling != NULL && nextsibling->endpc <= bl->endpc)
          blcnt->firstchild = nextsibling; /* update first child */
        else
          blcnt->firstchild = NULL;
        /* discharge saved previous sibling */
        blcnt->prevsibling = prevsibling;
        updatedblock = updatedchild = 1;
        if (!p)
          bl->u.bl.pcnt = *blcnt;
      }
    }
    lastblcnt = blcnt;
    /* update the rest of the parents but only update startpc */
    while ((bl = prevdoblockstate1(bl)) != NULL) {
      int blreg = bl->u.bl.reg;
      int usesprevnil = (o == OP_LOADNIL && a < blreg && blreg <= b);
      blcnt = p ? &bl->u.bl.pcnt : &bl->u.bl.rcnt;
      if (blreg == regtocheck || usesprevnil) {
        int newstartpc = pc+usesprevnil;
        blcnt->startpc = newstartpc;
        /* if the first block firstchild/prevsibling were not updated, check if */
        if (updatedchild == 0) {
          if (nextsibling != NULL && nextsibling->endpc <= bl->endpc)
            blcnt->firstchild = nextsibling;
          else
            blcnt->firstchild = NULL;
          blcnt->prevsibling = prevsibling;
          updatedchild = 1;
        }
        if (!p)
          bl->u.bl.pcnt.startpc = blcnt->startpc;
      }
      /* if this block doesn't get updated, but a child block has been updated
         and this block startpc is in an invalid range, move it to the latest
         possible that it can start, i.e. it's child startpc; normally, the
         startpclimit is such that (blcnt->startpc <= startpclimit) always
         evaluates to true; but when rescanning code after encountering a branch
         context, the startpclimit is used to control when parent block contexts
         are updated; if the parent startpc was set to a pc that is inside the
         newly encountered branch context, it will be moved to a valid pc
         outside of the branch context, otherwise it will be updated normally */
      else if (updatedchild && blcnt->startpc <= startpclimit) {
        blcnt->startpc = lastblcnt->startpc;
      }
      lastblcnt = blcnt;
    }
  }
  return updatedblock;
}


/*
** returns the first local variable that starts at PC; also sets the number of
** active local variables at PC for P_NACTVAR, and sets NVARS to the number of
** variables that start at PC
*/
static struct LocVar *getactvar1(DFuncState *fs, int pc, int *nvars,
                                 lu_byte *nactvar)
{
  DecompState *D = fs->D;
  struct LocVar *retvar = NULL;  /* first local that starts at PC */
  int i,n=0;  /* N = number of variables that start on PC */
  lu_byte nact = 0;  /* number of active locals at PC */
  lua_assert(ispcvalid(fs, pc));
  if (D->usedebuginfo == 0)
    return NULL;
  for (i = fs->sizelocvars-1; i >= 0; i--) {
    struct LocVar *var = &fs->locvars[i];
    if (var->startpc <= pc && pc <= var->endpc)
      nact++;
    if (var->startpc == pc) {
      retvar = var;
      n++;
    }
  }
  if (nactvar) *nactvar = nact;
  if (nvars) *nvars = n;
  return retvar;
}


/*
** rescan an instruction range and stop on the first pc where the state of a
** given block is updated
*/
static int updateblockstateinrange1(DFuncState *fs, BlockState *block,
                    int startpc, int endpc, BlockNode *nextnode, int occluded)
{
  int updatedblock = 0;
  int nextnodestart = nextnode ? nextnode->startpc : -1;
  struct siblingsnapshot1 s;
  int pc;
  initsiblingsnapshot1(&s, nextnode);
  for (pc = startpc; pc < endpc; pc++) {
    LocVar *var;
    /* skip over existing block nodes that start inside this do-block */
    if (pc == nextnodestart) {
      pc = nextnode->endpc;
      nextnode = nextnode->nextsibling;
      nextnodestart = nextnode ? nextnode->startpc : -1;
      continue;
    }
    getsiblingsforpc1(&s, pc);
    /* get the first local variable that started at PC */
    var = getactvar1(fs, pc, NULL, NULL);
    if (recordblockstateforpc1(fs, block, var, pc, occluded, &s, startpc-1)) {
      updatedblock = 1;
      break;
    }
    advancesiblingsnapshot1(&s);
  }
  return updatedblock;
}


/*
** close any pending blocks that are inside an encountered branch and update the
** state the remaining pending blocks
*/
static void rescanblocksfornewbranch1(DFuncState *fs, struct blockstates1 *s,
                                      int endpc, BlockNode **siblingchain)
{
  int occluded;
  BlockNode *nextsibling = *siblingchain;
  BlockState *laststate, *block = s->block;
  BlockState *branch;
  if (block != NULL && block->endpc <= endpc) {
    struct BlockState *lastbl = NULL;
    while (block != NULL) {
      BlockNode *node;
      lastbl = block;
      if (block->endpc >= endpc)
        break;
      block->u.bl.rcnt = block->u.bl.pcnt;
      node = finalizeblock1(fs, block);
      lua_assert(node != NULL);
      postfinalizeblock1(block, node, &nextsibling);
      updateblockstates1(fs, s);  /* pop finalized do-block state */
      block = s->block;
    }
    lua_assert(lastbl != NULL);
    lua_assert(lastbl->branch == 0);
    /* check if a block was created for this branch because it has upvalues */
    if (lastbl->endpc == endpc) {
      /* check if the current loop was thought to have upvalues; this would
         happen if there the OP_CLOSE code was before the last OP_JMP in a loop
        */
      if (lastbl->u.bl.loop) s->upval = 0;
      if (s->haveblocksibling == 0) {
        s->blocksibling = lastbl->nextsibling;
        s->haveblocksibling = 1;
      }
      /* this branch has upvalues; the block is redundant */
      updateblockstates1(fs, s);
      block = s->block;
    }
    else if (lastbl->endpc > endpc)
      block = lastbl;
    else
      lua_assert(block == NULL);
  }
  branch = s->branch;
  if (branch != NULL) {
    /* get the last do-block context */
    block = getparentblock1(fs, branch);
    laststate = branch;
  }
  else
    laststate = block;
  /* for the rest of the block contexts that end after the new branch context,
     correct their start pcs if needed to ensure they do not start in the middle
     of the new branch block */
  occluded = (branch != NULL);
  while (block != NULL && block->endpc > endpc) {
    /*BlockNode *nextnode = nextsibling;
    int nextnodestart = nextnode ? nextnode->startpc : -1;*/
    int limitpc = occluded ? branch->u.br.midpc-1 : block->endpc;
    if (block->u.bl.pcnt.startpc <= endpc) {
      if (updateblockstateinrange1(fs, block, endpc+1, limitpc, nextsibling,
                                   occluded) == 0) {
      /* the block context has only been updated to pcs inside the new branch
         context since its last valid state, so reset it to its last valid
         state */
        if (occluded)
          /* if this block has a child, the child will have a snapshot of this
             block's last valid state */
          block->u.bl.pcnt = laststate->parentsnapshot;
        else {
          /* no valid state exists; reset the state completely */
          block->u.bl.rcnt.startpc = block->endpc;
          block->u.bl.rcnt.prevsibling = NULL;
          block->u.bl.rcnt.firstchild = NULL;
        }
      }
      /* stop once a block has actually been updated (not reset) because its
         parents startpcs will have been corrected as well */
      else break;
    }
    laststate = block;
    block = prevdoblockstate1(block);
  }
  /* update NEXTSIBLING for the caller in case there were nested block nodes
     created */
  *siblingchain = nextsibling;
}


/*
** close all pending blocks that exist within a newly encountered branch before
** pushing a new state for the branch
*/
static int updateblocksfornewbranch1(DFuncState *fs, struct blockstates1 *s,
                                      int maxendpc, BlockNode **siblingchain)
{
  BlockState *oldbranch = s->branch;
  lu_byte prevbranch = 0;
  rescanblocksfornewbranch1(fs, s, maxendpc, siblingchain);
  while (s->branch != NULL && maxendpc > s->branch->endpc &&
         maxendpc > s->branch->u.br.optimalexit+1) {
    BlockState *branch = s->branch;
    BlockNode *result;
    /* finalize any branch contexts that are contained within this new branch
       context */
    short nested = branch->nested;
    /* the previous branch context ends earlier than this one, and no if-part
       has been found yet for it; finalize the branch and craete an if-false
       node for it */
    /* only update NEXTSIBLING for root if-false branches because their order is
       inverse to the stack, i.e. the top branch in its group is the earliest
       one */
    result = popbranch1(fs, s, NULL, prevbranch ? NULL : siblingchain);
    postfinalizebranch1(fs, branch, result, *siblingchain);
    prevbranch = (nested != 0);
    /* check again for nested block contexts that need to be finalized first */
    rescanblocksfornewbranch1(fs, s, maxendpc, siblingchain);
  }
  return oldbranch != s->branch;
}


/*
** updates the number of active local variables at PC; returns the first
** variable that starts at PC, sets NVARS to the number of variables that start
** at PC
*/
static struct LocVar *updateactvar1(DFuncState *fs, int pc, int *nvars)
{
  return getactvar1(fs, pc, nvars, &fs->nactvar);
}


/*
** return whether the line at PC was fixed to the start-line of a loop starting
** at TARGET
*/
static int isjumplinefixed(DFuncState *fs, int pc, int target, int defaultvalue)
{
  lua_assert(ispcvalid(fs, pc));
  lua_assert(ispcvalid(fs, target));
  lua_assert(pc >= target);
  if (fs->D->usedebuginfo == 0)
    return defaultvalue;
  else {
    int pcline = getline(fs->f, pc);
    int targetline = getline(fs->f, target);
    if (pcline > targetline)
      return 0;  /* line was not fixed */
    else if (pcline < targetline)
      return 1;  /* line was fixed */
    else {
      int i;
      for (i = target+1; i < pc; i++) {
        if (getline(fs->f, i) > targetline)
          return 1;  /* line was fixed */
      }
      return 0;  /* line may have been fixed but it does not manifest */
    }
  }
}


static void updateprevsibling1(DFuncState *fs, struct blockstates1 *s,
                               BlockNode *node)
{
  BlockState *bl;
  int p;
  if (s->branch != NULL) {
    if (s->branch->u.br.prevsibling2 == NULL)
      s->branch->u.br.prevsibling2 = node;
    else if (blstartpc(s->branch->u.br.prevsibling2) <= node->endpc)
      s->branch->u.br.prevsibling2 = node;
    if (s->branch->u.br.withblock) {
      bl = prevblockstate1(fs, s, s->branch);
      lua_assert(bl->branch == 0);
      lua_assert(bl->u.bl.opaque);
      if (bl->u.bl.rcnt.prevsibling == NULL)
        bl->u.bl.rcnt.prevsibling = node;
    }
    bl = getparentblock1(fs, s->branch);
    p = bl != NULL && bl->u.bl.opaque == 0;
  }
  else {
    bl = s->block;
    p = 0;
  }
  if (bl != NULL) {
    struct BlockStateControl *blstate;
    lua_assert(bl->branch == 0);
    blstate = p ? &bl->u.bl.pcnt : &bl->u.bl.rcnt;
    if (blstate->prevsibling == NULL)
      blstate->prevsibling = node;
  }
}


static void loop1(CodeAnalyzer *ca, DFuncState *fs, int startpc, int type,
                    BlockNode *futuresibling);


static void innerloop1(CodeAnalyzer *ca, DFuncState *fs, int startpc, int type,
                       BlockNode **nextsibling, struct blockstates1 *s)
{
  struct BlockNode *new_node;
  /* get the index for block/branch pointers because the stack may grow */
  int branchidx = s->branch ? s->branch - fs->a->pendingstk.u.s1 + 1 : 0;
  int blockidx = s->block ? s->block - fs->a->pendingstk.u.s1 + 1 : 0;
  lua_assert(branchidx >= 0 && branchidx <= fs->a->pendingstk.used);
  lua_assert(blockidx >= 0 && blockidx <= fs->a->pendingstk.used);
  loop1(ca, fs, startpc, type, *nextsibling);
  new_node = fs->a->bllist.first;
  lua_assert(new_node != *nextsibling);
  if (new_node->nextsibling == NULL) addsibling1(new_node, *nextsibling);
  /* update the branch/block pointers */
  lua_assert(branchidx <= fs->a->pendingstk.used);
  lua_assert(blockidx <= fs->a->pendingstk.used);
  s->branch = branchidx ? &fs->a->pendingstk.u.s1[branchidx-1] : NULL;
  s->block = blockidx ? &fs->a->pendingstk.u.s1[blockidx-1] : NULL;
  /* update block/branch states */
  updateprevsibling1(fs, s, new_node);
  *nextsibling = new_node;
}


static void loop1(CodeAnalyzer *ca, DFuncState *fs, int startpc, int type,
                    BlockNode *futuresibling)
{
  DecompState *D = fs->D;
  /* the outer loop context, saved on the stack */
  struct stat1 outer = ca->curr;
  struct blockstates1 s;
  /* NEXTSIBLING is the next chronological block node that has been created */
  BlockNode *nextsibling = NULL;
  /* NEXTBRANCH is the next if-branch block node that has been created */
  BlockNode *nextbranch = NULL;
  int nextbranchtarget = -1;
  int endpc;  /* endpc of the current block */
  int nextstat = -1;
  int closedloopreg = -1;
  lu_byte finalnactvar = fs->nactvar;  /* number of active locals at the end of
                                          this loop */
  const Instruction *code = ca->code;
  endpc = ca->pc--;
  s.branch = s.block = NULL;
  s.blocksibling = NULL;
  /* each loop context has a local stack base, any block states below this base
     position are outside of the current loop and will be handled after
     returning */
  s.stkbase = fs->a->pendingstk.used;
  s.upval = 0;
  s.haveblocksibling = 0;
  ca->curr.start = startpc; ca->curr.end = endpc; ca->curr.type = type;
  for (; ca->pc >= 0; ca->pc--) {
    struct LocVar *locvarhere;/* the first local vartable that starts at PC+1 */
    int nvars;  /* the number of variables that start at PC+1 */
    int pc = ca->pc;
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    int bx = GETARG_Bx(i);
    int sbx = GETARG_sBx(i);
    printinsn1(pc,i);
    /* update NACTVAR for PC+1 to catch variables that start on the final
       return; variables that start at PC=0 are not interesting in the first
       pass */
    if ((locvarhere = updateactvar1(fs, pc+1, &nvars))) {
      lua_assert(nvars > 0);
      if (o == OP_FORPREP) {
        updateactvar1(fs, pc, &nvars);
        goto traversefornumprep;
      }
      else if (test_ins_property(fs, pc+1, INS_FORLIST)) {
        updateactvar1(fs, pc, &nvars);
        goto traverseforlistprep;
      }
      D(lprintf("encountered start of new local variable at next pc (%i)\n", pc+1));
      locvarexpr1(ca, fs, nvars, locvarhere);
      goto poststat;
    }
    printf("nactvar: %d\n", fs->nactvar);
    switch (o) {
      case OP_JMP: {
        int target = pc + 1 + sbx; /* the jump target pc */
        int branchstartpc, branchendpc; /* these are used when creating
                                           branch block nodes (BL_IF/BL_ELSE),
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
        if (test_ins_property(fs, target, INS_BOOLLABEL) ||
            test_ins_property(fs, pc+1, INS_BOOLLABEL))
          break;  /* this jump is part of a boolean expression */
        if (test_ins_property(fs, pc+1, INS_FORLIST)) {
          traverseforlistprep:
          openexpr1(ca, fs, GETARG_A(code[pc+1+sbx]), FORLISTPREP, NULL);
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
                clearblocksatpc1(fs, &nextsibling, testsetpc);
              }
              D(lprintf("--\n"));
              adjustnextbranch1(fs, nextsibling, &nextbranch,&nextbranchtarget);
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
              innerloop1(ca, fs, target, BL_FORLIST, &nextsibling, &s);
              nextstat = ca->pc;
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
              if (type == BL_WHILE) { /* inside a while-loop */
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
                if (type == BL_REPEAT && target == startpc) {
                  lua_assert(test_ins_property(fs, target, INS_REPEATSTAT));
                  if (nextstat == -1 && finalnactvar == fs->nactvar)
                    /* no statement has been seen yet and no local variables
                       have died; the loops can be combined */
                    break;
                }
                markrepeatstat:
                encountered1("repeat", target);
                set_ins_property(fs, target, INS_REPEATSTAT);
                init_ins_property(fs, pc, INS_LOOPEND);
                init_ins_property(fs, pc, INS_LOOPPASS);
                innerloop1(ca, fs, target, BL_REPEAT, &nextsibling, &s);
                nextstat = ca->pc;
              }
            }
          }
          else { /* unconditional backward jump */
            if (type == BL_WHILE && target == startpc) {
              /* if not using debug info, err on the side of assuming this is a
                 while-loop; if you treat it as a branch and it is actually a
                 while-loop, then you run into trouble when encountering break
                 instructions because they will target a pc that is still inside
                 the current loop; it is easier to detect a wrong guess with a
                 while-loop because any jumps that break the loop will target a
                 pc that is past the end of the loop, which cannot happen unless
                 the next instruction after the end of the loop has a jump, but
                 that can be accounted for using instruction properties */
              if (isjumplinefixed(fs, pc, target, 1))
                goto markwhilestat;
              /* otherwise it is a branch with an optimized exit-jump */
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
            else if (type != BL_FUNCTION && outer.type == BL_WHILE &&
                     target == outer.start) {
              init_ins_property(fs, pc, INS_BREAKSTAT);
            }
            else { /* a while-loop */
              markwhilestat:
              /* the final jump in a while-loop will have its line fixed to the
                 start-line of the loop; if the line for this jump is greater
                 than the start-line, this cannot be a while-loop */
              if (fs->D->usedebuginfo &&
                  getline(fs->f, pc) > getline(fs->f, target))
                goto markrepeatstat;
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
              innerloop1(ca, fs, target, BL_WHILE, &nextsibling, &s);
              nextstat = ca->pc;
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
            lua_assert(type != BL_FUNCTION); /* pc is already checked */
            /* the current loop must have a sibling for it to be an optimized
               exit */
            if (futuresibling != NULL) {
              /* get the pc that would be the jump - if the sibling is an ELSE
                 block, than use the jump out of the preceding IF block */
              int siblingstartpc = futuresibling->startpc -
                                   (futuresibling->type == BL_ELSE);
              siblingjump = code[siblingstartpc];
              if (GET_OPCODE(siblingjump) == OP_JMP) {
                siblingtarget = siblingstartpc+1+GETARG_sBx(siblingjump);
                if (target == siblingtarget) { /* optimized jump? */
                  if (ca->prevTMode || (pc == startpc && type == BL_WHILE))
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
            if (type == BL_REPEAT && target-1 == endpc) {
              markrepeatfail:
              init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else if (type == BL_REPEAT &&
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
            else if (type == BL_WHILE &&
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
            else if (type == BL_WHILE && outer.type == BL_WHILE &&
                     target > outer.end) {
              BlockNode *lastsibling;
              BlockNode *newblock;
              D(lprintf("fixing erroneous while-true-loop detection\n"));
              /* this loop is not actually a loop, so if it was though to have,
                 upvalues, there is actually a do-block that needs to be made
                 visible */
              if (s.upval) {
                /* get the first block state that was pushed, as that is the one
                   which corresponds to the loop block in the case of the loop
                   having upvalues */
                BlockState *bl = &fs->a->pendingstk.u.s1[s.stkbase];
                lua_assert(bl->u.bl.loop);
                bl->u.bl.loop = 0;  /* make it a separate block */
                s.upval = 0;
              }
              finalizependingblocks1(fs, &s, &nextsibling);
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
                if (lastsibling->type == BL_IF &&
                    lastsibling->endpc == endpc-1) {
                  lastsibling->endpc++; /* if-block includes the exit-jump */
                  newblocktype = BL_ELSE;
                }
                else
                  newblocktype = BL_IF;
                /* insert the new block in the middle of the chain, so that
                   NEXTSIBLING gets set correctly after returning */
                newblock = newblnode(fs->H, endpc+1, endpc, newblocktype);
                {
                  BlockNode *nextinchain = lastsibling->next;
                  lastsibling->next = newblock;
                  lastsibling->nextsibling = newblock;
                  newblock->next = nextinchain;
                }
              }
              else { /* no preceding sibling blocks */
                D(lprintf("(NULL)\n")); /* no last sibling */
                newblock = addblnode1(fs, endpc+1, endpc, BL_IF);
                nextsibling = newblock;
              }
              D(lprintf("created new block %B\n", newblock));
              D(lprintf("first block in chain is %B\n",
                        fs->a->bllist.first));
              ca->testset.endpc = ca->testset.reg = -1;
              ca->curr = outer;
              return;
            }
            else { /* jumping inside a branch-condition */
              if (test_ins_property(fs, target-1, INS_BRANCHFAIL) &&
                  !test_ins_property(fs, target, INS_EMPTYBLOCK))
                init_ins_property(fs, pc, INS_BRANCHPASS);
              else {
                init_ins_property(fs, pc, INS_BRANCHFAIL);
                if (!test_ins_property(fs, target-1, INS_BRANCHPASS)) {
                  int issingle; /* true if this branch has no else-block; this
                                   is significant because only branches with an
                                   else-block cause a recursive call, whereas
                                   else-less branches are always child blocks in
                                   the current branch context */
                  BlockNode *new_node;
                  BlockState *branch;
                  ifbranch:
                  if (tailbranchjump)
                    branchendpc = endpc-1;
                  else
                    branchendpc = target-1;
                  updateblocksfornewbranch1(fs, &s, branchendpc, &nextsibling);
                  branch = getlastbranch1(fs, &s);
                  branchstartpc = pc+1;
                  if (branch != NULL)
                    branch->u.br.startpc = branchstartpc;
                  /* determine if this is a single branch statement ot not and
                     calculate the endpc of the if-part */
                  if (branch != NULL &&
                      (target == branch->u.br.midpc || tailbranchjump)) {
                    /* the end of the if-part */
                    branchendpc = branch->u.br.midpc-1;
                    if (tailbranchjump &&
                        (GET_OPCODE(code[endpc-1]) != OP_JMP ||
                         endpc+GETARG_sBx(code[endpc-1]) != startpc)) {
                      issingle = 1;
                    }
                    else
                      issingle = 0;
                    /*issingle = tailbranchjump;*/ /* there is an else-block */
                  }
                  else {
                    if (tailbranchjump && branch == NULL)
                      /* a single tail if-statement in a while-loop */
                      branchendpc = endpc-1;
                    else if (branch == NULL || target < branch->u.br.midpc)
                      /* a single (possibly child) if-statement */
                      branchendpc = target-1;
                    else
                      branchendpc = branch->u.br.midpc-1;
                    /* NEXTBRANCH is used to check if this branch and the next
                       branch have the same target and endpc, in which case they
                       can be merged as one if-statement with multiple
                       conditions connected with `and' or `or' */
                    if (nextbranch != NULL) {
                      BlockNode *next = nextbranch;
                      lua_assert(nextbranchtarget != -1);
                      if (next->endpc == branchendpc &&
                          nextbranchtarget == target) {
                        new_node = next;
                        D(lprintf("merging this branch with the next block "
                                  "%B\n", next));
                        issingle = 0;
                        nextsibling = new_node->nextsibling;
                        goto ifbranchnodecreated;
                      }
                    }
                    issingle = 1;
                    if (nextsibling != NULL && nextsibling->type == BL_IF) {
                      lua_assert(ispcvalid(fs, nextstat));
                      if (nextsibling->endpc == branchendpc &&
                          nextstat+1 >= nextsibling->startpc) {
                        new_node = nextsibling;
                        nextsibling = new_node->nextsibling;
                        goto ifbranchnodecreated;
                      }
                    }
                  }
                  nextbranch = NULL;
                  nextbranchtarget = -1;
                  if (branch) branch->u.br.target1 = target;
                  /*rescanblocksfornewbranch1(fs, &s, branchendpc, &nextsibling);*/
                  new_node = addblnode1(fs, branchstartpc, branchendpc, BL_IF);
                  ifbranchnodecreated:
                  /* BRANCHENDPC can be less than BRANCHSTARTPC if the block is
                     empty */
                  if (branchstartpc <= branchendpc) {
                    init_ins_property(fs, branchstartpc, INS_BRANCHBEGIN);
                    set_ins_property(fs, branchendpc, INS_BLOCKEND);
                  }
                  else { /* empty block */
                    init_ins_property(fs, branchstartpc, INS_EMPTYBLOCK);
                  }
                  lua_assert(new_node != NULL);
                  D(lprintf("BRANCH BLOCK - %B\n", new_node));
                  if (issingle || s.branch == NULL ||
                      nextsibling != s.branch->result) {
                    rescansiblingchain1(new_node, &nextsibling);
                    /* re-attach the next sibling for the block context that was
                       finalized because it is actually part of this branch */
                    if (s.haveblocksibling)
                      new_node->nextsibling = s.blocksibling;
                  }
                  /* discharge saved values from finalized erroneous blocks */
                  s.haveblocksibling = 0;
                  s.blocksibling = NULL;
                  /* connect the if and else parts */
                  if (s.branch != NULL)
                    new_node->nextsibling = s.branch->result;
                  nextsibling = new_node;
                  if (s.branch && !issingle) {
                    if (nextbranch == NULL) {
                      popbranch1(fs, &s, new_node, NULL);
                    }
                    else {
                      nextbranch = new_node;
                      nextbranchtarget = target;
                    }
                  }
                  else
                    updateprevsibling1(fs, &s, new_node);
                  ca->testset.endpc = ca->testset.reg = -1;
                  nextstat = pc;
                }
              }
            }
          }
          else { /* unconditional forward jump */
            if (prevop == OP_CLOSE && type == BL_REPEAT && target-1 == endpc) {
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
            if (type == BL_REPEAT && target <= endpc &&
                     test_ins_property(fs, pc, INS_LOOPPASS)) {
              break; /* already marked from above; do nothing */
            }
            else if (test_ins_property(fs, target-1, INS_LOOPEND) &&
                target-1 == endpc) { /* break statement */
              /* a while-loop can begin with a jump that is not a break if it
                 has a literal `false' condition */
              if (pc != startpc || type != BL_WHILE)
                init_ins_property(fs, pc, INS_BREAKSTAT);
              else /* (pc == startpc && type == BL_WHILE) */
                init_ins_property(fs, pc, INS_LOOPFAIL);
            }
            else { /* exiting a branch */
              BlockNode *result;
              int parentblock;
              /* use the maximum possible endpc to begin with */
              branchendpc = target-1;
              elsebranch:
              lua_assert(ispcvalid(fs, branchendpc));
              /* first finalize any block contexts that are contained within
                 this new branch context */
              if (updateblocksfornewbranch1(fs, &s, branchendpc, &nextsibling))
              {
                if (s.branch != NULL && s.branch->u.br.withblock) {
                  BlockState *block = s.branch-1;
                  int limitpc = s.branch->u.br.midpc-2;
                  short nested = block->nested;
                  /* pretend it doesn't have any parents so that they don't get
                     updated during the rescanning (they already have valid
                     startpcs) */
                  block->nested = 0;
                  updateblockstateinrange1(fs, s.branch-1, pc+1, limitpc,
                                           nextsibling, 0);
                  block->nested = nested;
                }
              }
              /* now that the stack has been cleaned up, calculate a more
                 appropriate endpc for this branch */
              if (s.branch != NULL &&
                  (target == s.branch->u.br.optimalexit ||
                   target == s.branch->endpc+1))
                branchendpc = s.branch->u.br.midpc-2;
              /* check if this is the fail-jump in an if-false-else */
              if (s.branch != NULL && GET_OPCODE(code[branchendpc]) == OP_JMP &&
                  target == s.branch->u.br.midpc) {
                goto ifbranch;
              }
              /* something is wrong if this new else-branch ends after an
                 enclosing if-branch ends  */
              if (s.branch != NULL && branchendpc >= s.branch->u.br.midpc) {
                /* the problem: a new branch has been encountered and a context
                   for it must be created, but simultaneously, the enclosing
                   branch context is false and needs to be returned from */
                /* the idea: switch out the branch context for the enclosing
                   one, return from the enclosing context, fix the erroneous
                   block, and recurse into the new branch context */
                /* Check if this new branch ends in the middle of the parent
                   branch. Even if the parent branch is erroneous, it is still a
                   valid block and if this branch jumps to the middle of that
                   block, neither this context nor the parent context should be
                   committed as block nodes. This addresses the following case:
                      local a = true or 5;
                      local b = false or 5;
                      local c = 1 or 5;
                   */
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
              /* compute the stack index of the next parent block context */
              if (s.branch != NULL)
                /* use the previous branch context value */
                parentblock = s.branch->u.br.parentblock;
              else if (s.block != NULL)
                /* compute the stack index of s.block */
                parentblock = s.block - fs->a->pendingstk.u.s1 + 1;
              else
                /* no parent block exists currently */
                parentblock = 0;
              if (s.block != NULL) {
                /* update the block's possible state to its actual state, as the
                   possible state may be what gets updated within the branch
                   context, so it needs to be initialized */
                s.block->u.bl.pcnt = s.block->u.bl.rcnt;
                ;
              }
              /* create the node for this `else' part, which may actually be a
                 single if-false block */
              result = addblnode1(fs, branchstartpc, branchendpc, BL_ELSE);
              updateprevsibling1(fs, &s, result);
              /* if the supposed if-part closes variables, keep track of the
                 block context in case there is no if-part and it is actually
                 its own do-block */
              if (prevop == OP_CLOSE) {
                int prevbranch = s.branch != NULL;
                int prevblock = s.block != NULL;
                pushblock1(fs, &s, -1, -1, 1)->u.bl.opaque = 1;
                if (prevbranch) {
                  /* restore parent branch pointer */
                  s.branch = s.block-1;
                  s.block = NULL;
                }
                else if (prevblock)
                  /* restore parent block pointer */
                  s.block = s.block-1;
                else
                  /* block is opaque, there is not parent */
                  s.block = NULL;
              }
              pushbranch1(fs, &s, branchstartpc, branchendpc, target);
              s.branch->result = result;
              s.branch->u.br.parentblock = parentblock;
              s.branch->u.br.withblock = (prevop == OP_CLOSE);
              lua_assert(s.branch->result != NULL);
              /* scan for block nodes that are contained in this else-block;
                 they were thought to be siblings when they were created, but
                 now they need to be demoted to children of this else-block */
              rescansiblingchain1(s.branch->result, &nextsibling);
              /* re-attach the next sibling from the finalized block that was
                 finalized because it is actually part of this new block */
              if (s.haveblocksibling)
                result->nextsibling = s.blocksibling;
              /* discharge saved values from the finalized erroneous block */
              s.haveblocksibling = 0;
              s.blocksibling = NULL;
              nextsibling = s.branch->result;
              nextstat = ca->pc;  /* else-stat or if-stat */
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
        innerloop1(ca, fs, target, BL_FORNUM, &nextsibling, &s);
        nextstat = ca->pc;
        break;
      }
      case OP_CLOSE: /* check for a block-ending if the next op is not a jump */
        if (pc+1 == endpc && type != BL_FUNCTION) {
          s.upval = 1;
          closedloopreg = a;
          lua_assert(s.branch == NULL);
          lua_assert(s.block == NULL);
          pushblock1(fs, &s, pc, a, 1)->u.bl.loop = 1;
          break;
        }
        if (s.branch != NULL && s.branch->u.br.withblock &&
            pc+2 == s.branch->u.br.midpc) {
          /* initialize the potential block context for the branch context */
          struct BlockState *pblock = s.branch-1/*prevblockstate1(fs, &s, branch)*/;
          lua_assert(pblock != NULL);
          lua_assert(pblock->u.bl.opaque);
          pblock->nextsibling = nextsibling;
          pblock->u.bl.rcnt.prevsibling = NULL;
          pblock->u.bl.rcnt.firstchild = NULL;
          pblock->u.bl.rcnt.startpc = pc;
          pblock->u.bl.pcnt = pblock->u.bl.rcnt;
          pblock->endpc = pc;
          pblock->u.bl.reg = a;
          pblock->u.bl.upval = 1;
        }
        else if (s.upval && test_ins_property(fs, pc+1, INS_BREAKSTAT)) {
          if (a == closedloopreg)
            break;
          /* if an OP_CLOSE before a break statement closes up to a lower
             register than that of the block state created for the current loop,
             that means the state created for the loop was actually for a nested
             block, and the loop itself does not have upvalues */
          else if (a < closedloopreg) {
            BlockState *bl;
            lua_assert(s.stkbase < fs->a->pendingstk.used);
            bl = &fs->a->pendingstk.u.s1[s.stkbase];
            lua_assert(bl->branch == 0);
            lua_assert(bl->u.bl.loop);
            bl->u.bl.loop = 0;
            /*upval = 0;*/
            closedloopreg = -1;
          }
        }
        else if (test_ins_property(fs, pc+1, INS_LOOPPASS) ||
                 test_ins_property(fs, pc+1, INS_LOOPFAIL) ||
                 (s.upval &&
                  (test_ins_property(fs, pc+1, INS_BREAKSTAT) ||
                   test_ins_property(fs, pc+1, INS_AUGBREAK) ||
                   test_ins_property(fs, pc+1, INS_AUGCONT)))) {
          break;
        }
        else {
          /* when multiple, possibly nested OP_CLOSE are encountered, it is
             important to compare the argA of each. A nested block would not
             close up to register <= to that of its enclosing block. So, if
             there is a previous OP_CLOSE context, check its register. If it is
             greater than or equal to this A, it cannot logically be nested. */
          if (s.block != NULL && s.block->u.bl.reg >= a) {
            /* this block is adjacent to the previous, switch out the context
               instead of pushing a new one */
            BlockNode *node = createdoblockfromstate1(fs, s.block);
            node->firstchild = s.block->u.bl.rcnt.firstchild;
            node->nextsibling = s.block->nextsibling;
            /* link the last sibling before the adjacent block to it */
            if (s.block->u.bl.rcnt.prevsibling)
              s.block->u.bl.rcnt.prevsibling->nextsibling = node;
            else
              nextsibling = node;
            /* re-initialize the context and continue without recursing */
            s.block->nextsibling = nextsibling;
            s.block->u.bl.rcnt.prevsibling = NULL;
            s.block->u.bl.rcnt.firstchild = NULL;
            s.block->u.bl.rcnt.startpc = pc;
            s.block->endpc = pc;
            s.block->u.bl.reg = a;
          }
          else {
            /* push a new do-block context */
            BlockState *new_block = pushblock1(fs, &s, pc, a, 1);
            /* save the current nextsibling for later when creating the node */
            new_block->nextsibling = nextsibling;
            new_block->u.bl.rcnt.startpc = pc;
            nextsibling = NULL;  /* set to NULL as if recursing */
            nextstat = pc;  /* end of do-stat */
          }
        }
        break;
      case OP_SETGLOBAL:
      case OP_SETUPVAL:
      case OP_SETUPVAL_R1:
        /* instructions that use register A as a source in a store operation */
        nextstat = pc;  /* encountered an assignment statement */
        if (storeexpr1(ca, fs, a))
          goto poststat;
        break;
      case OP_SETFIELD:
      case OP_SETFIELD_R1:
      case OP_SETTABLE:
      case OP_SETTABLE_BK:
      case OP_SETTABLE_N:
      case OP_SETTABLE_N_BK:
      case OP_SETTABLE_S:
      case OP_SETTABLE_S_BK:
      case OP_SETSLOTI:
      case OP_SETSLOT:
      case OP_SETSLOTS:
      case OP_SETSLOTMT:
        /* instructions that use C as a source in a store operation */
        nextstat = pc;  /* encountered an assignment statement */
        if (storeexpr1(ca, fs, c))
          goto poststat;
        break;
      case OP_SETSLOTN:
        /* assigning nil to a structure slot */
        nextstat = pc;  /* encountered an assignment statement */
        break;
      case OP_MOVE:
        if (D->usedebuginfo) {
          if (a < fs->nactvar) {  /* storing something in a local variable */
            storeexpr1(ca, fs, b);
            goto poststat;
          }
        }
        goto postexpr;
      case OP_CALL:
      case OP_CALL_I:
      case OP_CALL_I_R1:
      case OP_CALL_C:
      case OP_CALL_M: {
        int nret = c-1;  /* number of return values used */
        openexpr1(ca, fs, a, CALLPREP, NULL);
        if (nret == 0)  /* call is a statement */
          goto poststat;
        goto postexpr;
      }
      case OP_TAILCALL:
      case OP_TAILCALL_I:
      case OP_TAILCALL_I_R1:
      case OP_TAILCALL_C:
      case OP_TAILCALL_M:  /* a function call expression */
        openexpr1(ca, fs, a, CALLPREP, NULL);
        goto poststat;
      case OP_CONCAT:  /* a concat expression */
        openexpr1(ca, fs, b, CONCATPREP, NULL);
        if (D->usedebuginfo) {
          if (a < fs->nactvar) goto poststat;
        }
        goto postexpr;
      case OP_RETURN: {  /* a return expression */
        int nret = b-1;
        if (nret == 1) {  /* returns a single register */
          /* if debug info is available, a single-return can be differentiated
             as either an open expression or a returned local variable */
          if (D->usedebuginfo) {
            if (a >= fs->nactvar) {  /* returns a non-local register */
              openexpr1(ca, fs, a, RETPREP, NULL);
            }
          }
          else if (pc > 0)
            /* the second pass is interested in knowing when it is about to
               encounter a single-return */
            set_ins_property(fs, pc-1, INS_PRERETURN1);
        }
        else if (nret != 0) {  /* returns an open expression */
          openexpr1(ca, fs, a, RETPREP, NULL);
        }
        goto poststat;
      }
      case OP_FORPREP: {  /* mark the start of preparation code */
        int target;
        traversefornumprep:
        target = pc + 1 + sbx;  /* end of for-loop */
        CHECK(fs, ispcvalid(fs, target), "invalid target pc in OP_FORPREP");
        CHECK(fs, GET_OPCODE(code[target]) == OP_FORLOOP, "unexpected target "
              "code for OP_FORPREP (expected to jump to OP_FORLOOP)");
        lua_assert(test_ins_property(fs, target, INS_LOOPEND));
        openexpr1(ca, fs, a, FORNUMPREP, NULL);
        goto poststat;
      }
      case OP_SETLIST:
        openexpr1(ca, fs, a, SETLISTPREP, NULL);
        goto postexpr;
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
      case OP_LOADBOOL:
        if (c) {
          set_ins_property(fs, pc, INS_BOOLLABEL);
          set_ins_property(fs, pc+1, INS_BOOLLABEL);
        }
        goto postexpr;
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
        goto postexpr;
      }
      case OP_CHECKTYPE:
      case OP_CHECKTYPES:
        newregnote(fs, REG_NOTE_CHECKTYPE, pc, a);
        break;
      case OP_DATA:
        break;
      case OP_NEWTABLE:
        /* table constructors with only hash-items will not emit OP_SETLIST, nor
           empty table constructors, so they are detected here */
        CHECK(fs, b == 0, "no OP_SETLIST found for OP_NEWTABLE even though "
                "OP_NEWTABLE has non-zero amount of list items");
        if (c) {
          /* walk back up the code vector to find the earliest possible end of
             this constructor */
          scanforhashitems1(ca, fs, newopenexpr(fs, a, pc, -1, HASHTABLEPREP),
                            luaO_fb2int(c-1)+1, a, &nextsibling);
          adjustnextbranch1(fs, nextsibling, &nextbranch, &nextbranchtarget);
        }
        else
          newopenexpr(fs, a, pc, pc, EMPTYTABLE);
        goto postexpr;
      poststat:
        nextstat = ca->pc;
      postexpr:  /* update variables after calls which change the pc */
        pc = ca->pc;
        i = code[pc];
        o = GET_OPCODE(i);
        a = GETARG_A(i);
        b = GETARG_B(i);
        c = GETARG_C(i);
        /* fallthrough */
      default:
        if (beginseval(o, a, b, c, 0)) {
          set_ins_property(fs, pc, INS_CLOBBER);
          updatefirstclob1(fs, pc, a);
          /* check for an unused OP_LOADNIL label */
          if (o == OP_LOADNIL) {
            if (pc > 0 && GET_OPCODE(code[pc-1]) == OP_LOADNIL) {
              OpCode prevop = GET_OPCODE(code[pc-1]);
              if (prevop == OP_LOADNIL) {
                int prevA = GETARG_A(code[pc-1]);
                int prevB = GETARG_B(code[pc-1]);
                /* if these 2 nil codes could have been optimized, this pc must
                   have been a possible jump label when the code was generated,
                   such as an if-true block:
                      local a = nil;
                      if true then
                          local b = nil;
                      end */
                if (a >= prevA && a <= prevB+1)
                  set_ins_property(fs, pc, INS_NILLABEL);
              }
            }
          }
          if (D->usedebuginfo) {
            if (a < fs->nactvar)
              nextstat = pc;  /* something was assigned to a local variable */
          }
          /* update a pending block if needed (I am using WHILE so that I can
             break if there is no block to update; this avoids having to
             compare BL to NULL which is unnecessary) */
          while (s.block != NULL || s.branch != NULL) {
            int occluded;
            struct BlockState *bl;
            if (s.block != NULL) {
              bl = s.block;
              occluded = 0;
            }
            else {  /* see if there is an occluded or opaque block to update */
              BlockState *branch = s.branch;
              if (branch->u.br.withblock) {
                bl = branch-1;
                occluded = 0;  /* never treat the opaque block as occluded */
              }
              else if (branch->u.br.parentblock) {
                bl = getparentblock1(fs, branch);
                occluded = 1;
              }
              else break;  /* no block to update */
            }
            lua_assert(bl != NULL);
            {
              struct siblingsnapshot1 s;
              /* the current sibling snapshot: nextsibling is nextsibling,
                 prevsibling is unknown currently and therefore NULL */
              s.nextsibling = nextsibling; s.prevsibling = NULL;
              recordblockstateforpc1(fs, bl, locvarhere, pc, occluded, &s,
                                     fs->f->sizecode-1);
            }
            break;
          }
        }

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
      nextbranch->nextsibling->type == BL_ELSE &&
      nextbranch->nextsibling->isempty == 0) {
    Instruction jmp;
    int jmpval;
    int jmptarget;
    BlockNode *nextif = nextbranch;
    BlockNode *nextelse = nextbranch->nextsibling;
    lua_assert(nextif->type == BL_IF);
    lua_assert(nextelse->type == BL_ELSE);
    jmp = code[nextif->startpc-1];
    lua_assert(GET_OPCODE(jmp) == OP_JMP);
    jmpval = GETARG_sBx(jmp);
    jmptarget = nextif->startpc + jmpval;
    if (jmptarget != nextelse->startpc) {
      BlockNode *node = nextif->firstchild;
      nextelse->type = BL_IF;
      nextif->endpc = nextelse->endpc;
      if (node != NULL) {
        while (node->nextsibling != NULL)
          node = node->nextsibling;
        node->nextsibling = nextelse;
      }
      else
        nextif->firstchild = nextelse;
      nextif->nextsibling = nextelse->nextsibling;
    }
  }
  ca->testset.endpc = ca->testset.reg = -1;
  ca->curr = outer;
  finalizependingblocks1(fs, &s, &nextsibling);
  /* create a new block node */
  /* `nextsibling' will end up being the first child of this new block */
  addblnode1(fs, startpc, endpc, type)->firstchild = nextsibling;
}


/*
** create extra blocks to account for LOADNIL labels which do not already exist
** on a basic block boundary
** remove if-blocks that will not be able to generate matching code due to
** compiler optimizations and turn them into expressions
*/
static void addnillabels1(DFuncState *fs, BlockNode *node)
{
  BlockNode *prevchild = NULL;
  BlockNode *nextchild = node->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  int pc;
  for (pc = node->startpc; pc <= node->endpc; pc++) {
    if (pc == nextchildstartpc) {
      addnillabels1(fs, nextchild);
      /* note that if NEXTCHILD is empty, pc will end up being decremented by 1,
         then incremented by 1 when continuing, so that this pc is processed
         again under the current node, because it wouldn't be processed by the
         empty child block */
      pc = nextchild->endpc;
      prevchild = nextchild;
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      continue;
    }
    /* check for nil-label and augment a basic block if needed */
    if (test_ins_property(fs, pc, INS_NILLABEL)) {
      int isblockend = (test_ins_property(fs, pc-1, INS_BLOCKEND) ||
                        test_ins_property(fs, pc-1, INS_LOOPEND));
      int isblockstart = (test_ins_property(fs, pc, INS_REPEATSTAT) ||
                          test_ins_property(fs, pc, INS_WHILESTAT));
      if (!isblockend && !isblockstart) {
        /* a block node needs to be created so that a basic block boundary can
           exist between the 2 LOADNIL codes */
        BlockNode *new_node = addblnode2(fs, pc, pc, BL_IF);  /* if-true */
        if (prevchild != NULL)
          prevchild->nextsibling = new_node;
        else
          node->firstchild = new_node;
        prevchild = new_node;
        new_node->nextsibling = nextchild;
      }
    }
  }
}


/*
** get the next local variable that starts at PC, with a persistent iteration
** state saved in fs->nlocvars
*/
static struct LocVar *getnextlocvaratpc1(DFuncState *fs, int pc)
{
  lua_assert(ispcvalid(fs, pc));
  lua_assert(fs->D->usedebuginfo);
  if (fs->nlocvars < fs->sizelocvars) {
    struct LocVar *var = &fs->locvars[fs->nlocvars];
    if (var->startpc == pc) {
      fs->nlocvars++;
      return var;
    }
  }
  return NULL;
}


/*
** returns the first pc that would be in the indented part of a block in the
** source code
*/
static int getfirstindentedpc(DFuncState *fs, const BlockNode *node)
{
  int firstchildstart;
  int pc = node->startpc;
  int lastpossiblestart;
  if (node->type != BL_WHILE)
    return pc;
  /* for while-loops, find the first pc after the conditions */
  lastpossiblestart = pc;
  firstchildstart = node->firstchild ? node->firstchild->startpc : -1;
  for (; pc < node->endpc; pc++) {
    if (pc == firstchildstart)
      break;
    if (test_ins_property(fs, pc, INS_LOOPFAIL))
      lastpossiblestart = pc+1;
  }
  return lastpossiblestart;
}


/*
** returns the natural endpc for local variables declared inside NODE
*/
static int getnaturalvarendpc(const BlockNode *node)
{
  switch (node->type) {
    case BL_REPEAT: return node->endpc+1;
    case BL_FORLIST: return node->endpc-1;
    case BL_IF: return node->endpc+!haselsepart(node);
    /* variables in do-blocks that have upvalues end on the OP_CLOSE code, while
       variables in do-blocks that don't have upvalues end 1 after the last pc
       in the block */
    case BL_DO: return node->endpc+node->augmentedbyp1;
    default: /* FUNCTION, WHILE, FORNUM, ELSE */ return node->endpc;
  }
}


/*
** get the `follow block' pc, which is the pc where a `return' or `break' can
** exist without needing to augment a do-block around it
*/
static int getblockfollowpc(const BlockNode *node)
{
  switch (node->type) {
    /* for REPEAT, it needs to be calculated with stack analysis */
    case BL_REPEAT: return -1;
    /* for-list ends with TFORLOOP and JMP; subtract 2 from endpc */
    case BL_FORLIST: return node->endpc-2;
    /* for do-blocks which have OP_CLOSE, subtract 1 from endpc */
    case BL_DO: return node->endpc-!node->augmentedbyp1;
    /* if-blocks with else parts end with JMP; subtract 1 if needed */
    case BL_IF: return node->endpc-haselsepart(node);
    case BL_ELSE: return node->endpc;
    /* the rest have single termination code; subtract 1 */
    default: /* FUNCTION, WHILE, FORNUM */ return node->endpc-1;
  }
}


/*
** restructures the lexical scope hierarchy to preserve local variable info
*/
static void fixblockendings1(DFuncState *fs, BlockNode *node)
{
  /* variables for the most recently processed child block and the next child
     block to process for the current NODE */
  BlockNode *prevchild = NULL;
  BlockNode *nextchild = node->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  /* the last pc marked LOCVAREXPR (start of local variable initialization) */
  int lastlocvarexpr = -1;
  /* save this value so it can be restored when this call needs to be redone */
  short nlocvars = fs->nlocvars;
  /* the pc where locals in this block would naturally end */
  int varendpc = getnaturalvarendpc(node);
  /* function blocks end on the final return; only process instructions up to
     the final return (so that I can safely use (pc+1) when getting the next
     local variable) */
  int endpc = (node->type == BL_FUNCTION) ? node->endpc-1 : node->endpc;
  int pc;
  lua_assert(fs->D->usedebuginfo);
  for (pc = node->startpc; pc <= endpc; pc++) {
    struct LocVar *var;
    if (pc == nextchildstartpc) {
      fixblockendings1(fs, nextchild);
      pc = nextchild->endpc;
      prevchild = nextchild;
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      continue;
    }
    if (test_ins_property(fs, pc, INS_LOCVAREXPR))
      lastlocvarexpr = pc;
    if (pc+1 == nextchildstartpc && isforloop(nextchild)) {
      /* for-loop control variables start on the startpc of the for-loop, and
         no other varibale can start there; because of the 1-pc lookahead, those
         control variables will be seen here before recursing with NEXTCHILD */
      continue;
    }
    while ((var = getnextlocvaratpc1(fs, pc+1)) != NULL) {
      if (varisaugmented(var))
        continue;
      /* check if the variable does not end on the natural endpc */
      if (var->endpc != varendpc) {
        if (var->endpc > varendpc) {
          /* the variable can end after the block ends because the code analyzer
             may have detected an optimization that was not there; the lexical
             blocks were already ordered `optimally' in the source code */
          BlockNode *nextsibling = node->nextsibling;
          if (node->type == BL_IF) {
            if (nextsibling != NULL && nextsibling->type == BL_IF) {
              /* now NEXTSIBLING will become a child of the current block */
              BlockNode *newchild = nextsibling;
              BlockNode *child;  /* for iterating through the child chain */
              node->endpc = newchild->endpc;
              recalcemptiness(node);
              /* find the last child; get a headstart with NEXTCHILD */
              child = nextchild;
              while (child != NULL && child->nextsibling != NULL)
                child = child->nextsibling;
              /* add NEWCHILD to the chain */
              if (child != NULL)
                child->nextsibling = newchild;
              else {
                node->firstchild = newchild;
                nextchild = newchild;
                nextchildstartpc = nextchild->startpc;
              }
              /* transfer the next sibling from NEXTSIBLING to NODE */
              node->nextsibling = newchild->nextsibling;
              newchild->nextsibling = NULL;
            }
            /* another case is when a LOADNIL label was generated by an
               augmented if-true (by the decompiler); the if-true block is
               created as a single-instruction-block, but the local variable
               within it can have a greater lifespan; that is corrected here */
            else if (test_ins_property(fs, node->startpc, INS_NILLABEL)) {
              node->endpc = var->endpc-1;
              lua_assert(node->isempty == 0);
              rescansiblingchain1(node, NULL);
            }
            else continue;
          }
          /* I'm not aware of any other case where the variable ending after the
             block makes sense; but the decompiler is not responsible for
             matching malformed code */
          else continue;
        }
        else {  /* var->endpc < varendpc */
          /* a variable ends earlier than the block ends; insert a new do-block
             into the current block */
          BlockNode *new_node;
          int new_node_startpc;
          if (lastlocvarexpr != -1)
            new_node_startpc = lastlocvarexpr;
          else
            new_node_startpc = getfirstindentedpc(fs, node);
          new_node = createdoblock2(fs, new_node_startpc, var->endpc-1);
          /* this is my lazy way of saying this do-block doesn't have
             upvalues */
          new_node->augmentedbyp1 = 1;
          {  /* make NEW_NODE a child of NODE */
            /* find the surrounding child nodes for NEW_NODE */
            BlockNode *prevchild = NULL;
            BlockNode *child = node->firstchild;
            while (child != NULL && child->endpc < blstartpc(new_node)) {
              prevchild = child;
              child = child->nextsibling;
            }
            /* insert NEW_NODE into the chain */
            if (prevchild != NULL)
              prevchild->nextsibling = new_node;
            else
              node->firstchild = new_node;
            new_node->nextsibling = child;
            /* fix sibling chain for NEW_NODE */
            rescansiblingchain1(new_node, NULL);
          }
        }
        /* now redo this call; reset fs->nlocvars and do a tailcall with the
           same arguments */
        fs->nlocvars = nlocvars;
        fixblockendings1(fs, node);
        return;
      }
    }
  }
}


/*
** returns the `follow block' pc of a repeat-loop, or -1 if the repaet-loop does
** not have any return statements that could possibly be on a follow block pc
*/
static int findrepeatfollowblock1(DFuncState *fs, const BlockNode *node)
{
  BlockNode *nextchild = node->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  int pc;
  int lastreturn;
  lu_byte state = 0;  /* state-machine variable */
  lua_assert(node->type == BL_REPEAT);
  for (pc = node->startpc; pc < node->endpc; pc++) {
    if (pc == nextchildstartpc) {
      /* skip over child blocks */
      pc = nextchild->endpc;
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      state = 0;
      continue;
    }
    if (GET_OPCODE(fs->f->code[pc]) == OP_RETURN) {
      /* found a return code; check if it exists on a follow-block pc */
      state = 1;
      lastreturn = pc;
    }
    if (state) {
      int i;
      lu_byte nactvar = 0;
      /* update number of active locals */
      for (i = 0; i < fs->sizelocvars; i++) {
        struct LocVar *var = &fs->locvars[i];
        if (var->startpc <= pc && pc <= var->endpc)
          nactvar++;
      }
      /* see if the `until' part has been reached */
      if (test_ins_property(fs, pc, INS_LOOPFAIL) ||
          test_ins_property(fs, pc, INS_LOOPPASS)) {
        const Instruction *jc;
        lua_assert(GET_OPCODE(fs->f->code[pc]) == OP_JMP);
        jc = getjumpcontrol(fs,pc);
        if (jc == NULL)  /* unconditional jump */
          return pc-1;  /* the last pc is the follow-block pc */
        else {  /* conditional jump */
          int r, r2;
          int numregs = getregstested(*jc, &r, &r2);
          if (numregs == 1)
            r2 = r;
          if (numregs == 0 || (r < nactvar && r2 < nactvar))
            return pc-2;  /* pc before TEST and JMP */
          /* find the lowest temporary register of the 2 that are tested */
          if (r < nactvar)
            r = r2;
          else if (r2 >= nactvar)
            r = (r < r2) ? r : r2;
          /* now walk back down the code to the start of the evluation of R */
          for (pc -= 2; pc > lastreturn; pc--) {
            OpCode o = GET_OPCODE(fs->f->code[pc]);
            int a = GETARG_A(fs->f->code[pc]);
            int b = GETARG_B(fs->f->code[pc]);
            int c = GETARG_C(fs->f->code[pc]);
            if (!beginseval(o, a, b, c, 0) || a < nactvar)
              break;  /* found a code that is not part of the condition */
          }
          return pc;
        }
      }
    }
  }
  return -1;
}


/*
** mark the `follow block' pc for a node and each of its children
*/
static void markfollowblock1(DFuncState *fs, BlockNode *node)
{
  BlockNode *child = node->firstchild;
  int followblockpc = getblockfollowpc(node);
  if (!ispcvalid(fs, followblockpc))
    followblockpc = findrepeatfollowblock1(fs, node);
  if (ispcvalid(fs, followblockpc))
    set_ins_property(fs, followblockpc, INS_BLOCKFOLLOW);
  while (child != NULL) {
    markfollowblock1(fs, child);
    child = child->nextsibling;
  }
}

enum GENVARNOTE {
  GENVAR_TRIVIAL,
  GENVAR_FILL,
  GENVAR_UNCERTAIN,
  GENVAR_CERTAIN,
  GENVAR_NECESSARY,
  GENVAR_PARAM,
  GENVAR_FORNUM,  /* augmented numeric for-loop variables */
  GENVAR_FORLIST  /* augmented list for-loop variables */
};

typedef struct {
  const BlockNode *currnode;
  DFuncState *fs;
  const Instruction *code;
  const OpenExpr *nextexpr;
  const RegNote *nextnote;
  int nregnote, nopenexpr;
  int pc;
  OpCode o;
  int a, b, c;
  int currvarlimit;
  int varendpc;
  short nlocvars;
  lu_byte nactvar;
  unsigned short actvar[LUAI_MAXVARS];  /* declared-variable stack */
} DebugGenerator;

static int getvarnote1(struct LocVar *var)
{
  return cast_int(cast(size_t, var->varname));
}

static void setvarnote1(DebugGenerator *s, int r, int note)
{
  unsigned short i = s->actvar[r];
  s->fs->a->locvars[i].varname = cast(TString *, cast(size_t, note));
}

static void addvar1(DebugGenerator *s, int r, int startpc, int endpc, int note)
{
  Analyzer *a = s->fs->a;
  struct LocVar *var;
  luaM_growvector(s->fs->H, a->locvars, s->nlocvars, a->sizelocvars,
                  LocVar, SHRT_MAX, "too many local variables");
  var = &a->locvars[s->nlocvars];
  var->startpc = startpc;
  var->endpc = endpc;
  var->varname = cast(TString *, cast(size_t, note));
  lua_assert(r == s->nactvar);
  lua_assert(r < s->fs->f->maxstacksize);
  s->actvar[r] = s->nlocvars++;
  s->nactvar++;
}

static void updatenextopenexpr1(DebugGenerator *s)
{
  if (s->nopenexpr < 0) {
    s->nextexpr = NULL;
    s->currvarlimit = s->fs->f->maxstacksize;
  }
  else {
    const OpenExpr *e = &s->fs->a->opencalls[s->nopenexpr--];
    s->nextexpr = e;
    if (e->startpc < s->currnode->endpc)
      s->currvarlimit = e->firstreg;
    else
      s->currvarlimit = s->fs->f->maxstacksize;
  }
}

static void updatenextregnote1(DebugGenerator *s)
{
  lua_assert(s->nregnote >= 0);
  if (s->nregnote >= s->fs->nregnotes) s->nextnote = NULL;
  else s->nextnote = &s->fs->a->regnotes[s->nregnote++];
}


static void advance1(DebugGenerator *s)
{
  Instruction i = s->code[++s->pc];
  OpCode o = GET_OPCODE(i);
  int b, c = 0;
  s->o = o;
  s->a = GETARG_A(i);
  if (getOpMode(o) == iABC) { b = GETARG_B(i); c = GETARG_C(i); }
  else if (getOpMode(o) == iABx) b = GETARG_Bx(i);
  else b = GETARG_sBx(i);
  s->b = b;
  s->c = c;
}


static int maybeaddvar1(DebugGenerator *s, int r, int pc, int pclimit)
{
  /* see if register A can possibly be a local variable in this block */
  if (r < s->currvarlimit) {
    /* check reg notes and see if this is a good time to create a local
       variable at A */
    int startpc = pc+1;
    int note;
    int i;
    for (i = s->nactvar; i < r; i++)
      addvar1(s, i, startpc, s->varendpc, GENVAR_FILL);
    if (s->nextnote == NULL)
      note = GENVAR_TRIVIAL;
    else if (s->nextnote->pc < pclimit && s->nextnote->reg <= r)
      note = GENVAR_CERTAIN;
    else
      note = GENVAR_UNCERTAIN;
    addvar1(s, r, startpc, s->varendpc, note);
    return 1;
  }
  return 0;
}


static void genblockdebug1(DebugGenerator *s, const BlockNode *node)
{
  const BlockNode *prevnode = s->currnode;
  BlockNode *nextchild = node->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  int nextpclimit = nextchildstartpc != -1 ? nextchildstartpc : node->endpc+1;
  const int varendpc = s->varendpc;
  const lu_byte nactvar = s->nactvar;
  int r1 = -1, rstreak = 0;
  s->currnode = node;
  s->varendpc = getnaturalvarendpc(node);
  if (isforloop(node)) {
    int isnum = node->type == BL_FORNUM;
    int note = isnum ? GENVAR_FORNUM : GENVAR_FORLIST;
    int i,nvars;
    /* reserve space for augmented vars */
    addvar1(s, s->nactvar, node->startpc-1, node->endpc+1, note);
    addvar1(s, s->nactvar, node->startpc-1, node->endpc+1, note);
    addvar1(s, s->nactvar, node->startpc-1, node->endpc+1, note);
    nvars = isnum ? 1 : GETARG_C(s->code[node->endpc-1]);
    /* todo: how do you handle the for-loop parser bug */
    for (i = 0; i < nvars; i++)
      addvar1(s, s->nactvar, node->startpc, s->varendpc, GENVAR_NECESSARY);
  }
  for (s->pc = node->startpc; s->pc <= node->endpc; advance1(s)) {
    int pc = s->pc;
    OpCode o = s->o;
    if (pc == nextchildstartpc) {
      genblockdebug1(s, nextchild);
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      nextpclimit = nextchildstartpc != -1 ? nextchildstartpc : node->endpc+1;
      continue;
    }
    if (s->nextexpr && pc == s->nextexpr->startpc) {
      s->pc = s->nextexpr->endpc;
      updatenextopenexpr1(s);
      continue;
    }
    if (beginseval(o, s->a, s->b, s->c, 0)) {
      int r = s->a;
      if (r != r1) rstreak = 0;
      else rstreak++;
      r1 = r;
      if (r >= s->nactvar) {  /* clobbering a free register */
        /* see if register A can possibly be a local variable in this block */
        /*if (o == OP_MOVE && s->b > s->a) {
          
        }*/
        int newvar = maybeaddvar1(s, r, pc, nextpclimit);
        if (newvar && (o == OP_LOADNIL || o == OP_VARARG)) {
          int i, lastreg = o == OP_LOADNIL ? s->b : r + s->b - 2;
          for (i = r+1; i <= lastreg; i++) {
            if (!maybeaddvar1(s, i, pc, nextpclimit))
              break;
          }
        }
      }
    }
    else if (o != OP_DATA) {
      r1 = -1;
      rstreak = 0;
    }
  }
  s->currnode = prevnode;
  s->nactvar = nactvar;
  s->varendpc = varendpc;
  (void)getvarnote1; (void)setvarnote1;
}


static TString *genvarname1(DFuncState *fs, int i, int ispar)
{
  char buff[sizeof("f_local") + (2 *INT_CHAR_MAX_DEC)];
  const char *fmt = ispar ? "f%d_par%d" : "f%d_local%d";
  sprintf(buff, fmt, fs->idx, i);
  return luaS_new(fs->H, buff);
}


static int createvarnames1(DFuncState *fs, int nvars, struct LocVar *vars)
{
  static const char *const forloop_names[] = {
    "(for index)", "(for limit)", "(for step)",
    "(for generator)", "(for state)", "(for control)"
  };
  int i;
  for (i = 0; i < nvars; i++) {
    struct LocVar *var = &vars[i];
    int note = getvarnote1(var);
    if (note == GENVAR_FORNUM || note == GENVAR_FORLIST) {
      int j = (note == GENVAR_FORNUM) ? 0 : 3;
      int k = j+ 3;
      for (; j < k; i++, j++) {
        vars[i].varname = luaS_new(fs->H, forloop_names[j]);
        lua_assert(ispcvalid(fs, vars[i].startpc));
        lua_assert(ispcvalid(fs, vars[i].endpc));
      }
    }
    else {
      vars[i].varname = genvarname1(fs, i, note == GENVAR_PARAM);
    }
  }
  return nvars;
}


static void createparams1(DebugGenerator *s)
{
  int i;
  for (i = 0; i < s->fs->f->numparams; i++)
    addvar1(s, i, 0, s->fs->f->sizecode-1, GENVAR_PARAM);
}


static void debugdebug1(DFuncState *fs)
{
  int i;
  printf("Local variables\n");
  printf("--------------------------\n");
  for (i = 0; i < fs->sizelocvars; i++) {
    LocVar *var = &fs->locvars[i];
    lprintf("(%i)  %s  (%i-%i)\n", i, getstr(var->varname), var->startpc, var->endpc);
  }
  printf("--------------------------\n");
}


static void gendebug1(DFuncState *fs, const BlockNode *startnode)
{
  int newsizelocvars;
  DebugGenerator s;
  Analyzer *a = fs->a;
  s.fs = fs;
  s.currnode = startnode;
  s.code = fs->f->code;
  s.nregnote = 0;
  s.nopenexpr = fs->nopencalls-1;
  s.nactvar = 0;
  s.nlocvars = 0;
  s.pc = -1;
  /* FS must be using the analyzer data to generate variables */
  lua_assert(fs->locvars == a->locvars);
  advance1(&s);
  updatenextopenexpr1(&s);
  updatenextregnote1(&s);
  createparams1(&s);
  genblockdebug1(&s, startnode);
  newsizelocvars = createvarnames1(fs, s.nlocvars, a->locvars);
  luaM_reallocvector(fs->H, a->locvars, a->sizelocvars, newsizelocvars, LocVar);
  a->sizelocvars = newsizelocvars;
  fs->sizelocvars = a->sizelocvars;
  fs->locvars = a->locvars;
  debugdebug1(fs);
  memset(a->actvar, 0, a->sizeactvar * sizeof(unsigned short));
}


/*
** initialize memory for first pass
*/
static void initpass1(DFuncState *fs)
{
  Analyzer *a = fs->a;
  lua_assert(a->decomppass == 1);
  a->pendingstk.total = 4;
  a->pendingstk.used = 0;
  a->pendingstk.u.s1 = luaM_newvector(fs->H, a->pendingstk.total, BlockState);
}


/*
** free memory for first pass
*/
static void cleanuppass1(DFuncState *fs)
{
  Analyzer *a = fs->a;
  lua_assert(a->decomppass == 1);
  /* free the pending block stack, it is no longer needed */
  luaM_freearray(fs->H, a->pendingstk.u.s1, a->pendingstk.total, BlockState);
  a->pendingstk.total = 0;
  a->pendingstk.used = 0;
  a->pendingstk.u.s1 = NULL;
  /* resize the open expression and regnote arrays */
  luaM_reallocvector(fs->H, a->opencalls, a->sizeopencalls, fs->nopencalls,
                     OpenExpr);
  a->sizeopencalls = fs->nopencalls;
  luaM_reallocvector(fs->H, a->regnotes, a->sizeregnotes, fs->nregnotes,
                     RegNote);
  a->sizeregnotes = fs->nregnotes;
  /* reset fields for the second pass */
  fs->nlocvars = 0;
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
  BlockNode *func;
  CodeAnalyzer ca;
  ca.curr.start = ca.curr.end = ca.curr.type = -1;
  ca.pc = f->sizecode - 1; /* will be decremented again to skip final return */
  ca.prevTMode = 0;
  ca.inopenexpr = 0;
  ca.testset.endpc = ca.testset.reg = -1;
  ca.code = f->code;
  initpass1(fs);
  loop1(&ca, fs, 0, BL_FUNCTION, NULL);
  func = fs->a->bllist.first;
  lua_assert(func != NULL);
  lua_assert(func->nextsibling == NULL);
  lua_assert(func->type == BL_FUNCTION);
  lua_assert(ca.pc <= 0);
  lua_assert(ca.testset.endpc == -1 && ca.testset.reg == -1);
  if (fs->D->usedebuginfo == 0)
    gendebug1(fs, func);
  /* add post-processing functions here */
  addnillabels1(fs, func);
  if (fs->D->usedebuginfo)
    fixblockendings1(fs, func);
  markfollowblock1(fs, func);
  /* end of pass1 post-processing */
  cleanuppass1(fs);
}


typedef struct StackAnalyzer {
  const Instruction *code;
  BlockNode *currbl;
  struct HoldItem *currheader;  /* current block header hold item */
  int laststore;  /* exp index of last store node */
  struct {
    int e, target, pc;
  } lastcond;
  int numforloopvars;
  int nextforloopbase;
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
** returns true if RK indexes a non-local slot
*/
static int istempreg(DFuncState *fs, int rk)
{
  return !ISK(rk) && !test_reg_property(fs, rk, REG_LOCAL);
}


/*
** returns the LocVar currently corresponding to REG
*/
static LocVar *getlocvar2(DFuncState *fs, int reg)
{
  return &fs->locvars[fs->a->actvar[reg]];
}


/*
** map the N newest local variables from their corresponding registers to their
** positions in the LocVar vector
*/
static void addlocalvars2(DFuncState *fs, int n)
{
  int i;
  for (i = 0; i < n; i++)
    fs->a->actvar[fs->nactvar+i] = fs->nlocvars-n+i;
}


/*
** `varstartsatpc2' returns how many variables start at the given PC (if debug
** info is not being used, the return value is always -1)
*/
static int varstartsatpc2(DFuncState *fs, int pc)
{
  struct LocVar *var;
  int i = fs->nlocvars;
  int n = 0;
  lua_assert(ispcvalid(fs, pc));
  while (i < fs->sizelocvars && (var = &fs->locvars[i])->startpc == pc) {
    i = ++fs->nlocvars;
    n++;
    D(lprintf("variable '%s' begins at (%i)\n", getstr(var->varname), pc));
  }
  return n;
}


#if 0
static LocVar *addlocvar2(DFuncState *fs, int startpc, int endpc, int param)
{
  struct LocVar *var;
  int i = fs->nactvar++;
  lua_assert(i >= 0 && i < fs->sizelocvars);
  var = &fs->locvars[i];
  if (1 || fs->D->usedebuginfo) { /* variable information already exists */
    lua_assert(ispcvalid(fs, var->startpc));
    lua_assert(ispcvalid(fs, var->endpc));
    lua_assert(var->varname != NULL);
    if (param) {
      lua_assert(var->startpc == startpc);
      lua_assert(var->endpc == endpc);
    }
    lua_assert(fs->nactvar <= fs->nlocvars);
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
#endif


/*
** find the initial first free register for a function and initialize register
** flags for the non-free registers
*/
#if 0
static void initfirstfree2(DFuncState *fs, const Proto *f)
{
  int i;
/*  int firstfree;
  int firstclobnonparam = fs->firstclobnonparam;
  lua_assert(firstclobnonparam == -1 || ispcvalid(fs, firstclobnonparam));*/
  /* in regular Lua, initial OP_LOADNILs are removed, so the real first free
     register is equal to however many variable start at pc 0 */
  for (i = f->numparams; i < fs->sizelocvars; i++) {
    if (fs->locvars[i].startpc != 0)
      break;
  }
  fs->firstfree = i;
/*  if (firstclobnonparam != -1)
    firstfree = GETARG_A(f->code[firstclobnonparam]);
  else
    firstfree = f->numparams;*/
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
  addlocalvars2(fs, firstfree);
  for (i = 0; i < f->numparams; i++)
    addlocvar2reg2(fs, 0, f->sizecode-1, 1, i); /* add param */
  fs->nactvar = 
  for (; i < firstfree; i++)
    addlocvar2reg2(fs, 0, f->sizecode-1, 0, i); /* add local variable */
  fs->firstfree = firstfree;
}
#endif


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
static void debugenterblock2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  D(lprintf("pass2: entered %sblock %b (%i-%i) at pc (%i)\n", 
       node->isempty ? (sa->intailemptyblock ? "tail-empty " : "empty ") : "",
       node, node->startpc, node->endpc, sa->pc));
#ifdef HKSC_DECOMP_DEBUG_PASS1
  if (node->type == BL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  DumpIndentation(fs->D);
  DumpString(bltypename(node->type),fs->D);
  DumpLiteral("\n",fs->D);
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
  UNUSED(fs);
}

static void debugleaveblock2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  D(lprintf("pass2: leaving %sblock %b (%i-%i) at pc (%i)\n", node->isempty ?
            (sa->intailemptyblock ? "tail-empty " : "empty") : "", node,
            node->startpc, node->endpc, sa->pc));
#ifdef HKSC_DECOMP_DEBUG_PASS1
  if (node->type == BL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  if (node->type != BL_IF || node->nextsibling == NULL ||
      node->nextsibling->type != BL_ELSE) { /* block ends with `END' */
    DumpIndentation(fs->D);
    DumpLiteral("END\n",fs->D);
  }
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
  UNUSED(fs);
}
#else /* !LUA_DEBUG */
#define debugenterblock2(sa,fs,node) ((void)0)
#define debugleaveblock2(sa,fs,node) ((void)0)
#endif /* LUA_DEBUG */


#ifdef LUA_DEBUG
static void assertblvalid(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  int startpc = node->startpc;
  int endpc = node->endpc;
  int type = node->type;
  BlockNode *nextsibling = node->nextsibling;
  lua_assert((startpc > endpc) == node->isempty);
  lua_assert(type >= 0 && type < MAX_BLTYPE);
  if (sa->intailemptyblock) {
    lua_assert(sa->pc+1 == startpc);
    lua_assert(node->nextsibling == NULL);
  }
  else
    lua_assert(sa->pc == startpc);
  if (type == BL_FUNCTION) {
    lua_assert(sa->pc == 0);
    lua_assert(endpc == sa->sizecode-1);
  }
  if (type < BL_DO && type != BL_FUNCTION) {
    check_ins_property(fs, endpc, INS_LOOPEND);
    if (type == BL_WHILE)
      check_ins_property(fs, startpc, INS_WHILESTAT);
    else if (type == BL_REPEAT)
      check_ins_property(fs, startpc, INS_REPEATSTAT);
    else if (type == BL_FORNUM)
      check_ins_property(fs, startpc, INS_FORNUM);
    else if (type == BL_FORLIST)
      check_ins_property(fs, startpc, INS_FORLIST);
  }
  else if (type == BL_DO) {
    check_ins_property(fs, startpc, INS_DOSTAT);
    check_ins_property(fs, endpc, INS_BLOCKEND);
  }
  else if (type == BL_IF && nextsibling != NULL &&
           nextsibling->type == BL_ELSE) {
    lua_assert(node->endpc+1 == nextsibling->startpc);
  }
}

static void printinsn2(DFuncState *fs, BlockNode *node, int pc, Instruction i)
{
  int type = node->type;
  lprintf("pc = (%i), %O    %s\n", pc, i, bltypename(type));
  UNUSED(fs);
}
#else /* !LUA_DEBUG */
#define assertblvalid(sa,fs,node) ((void)(sa),(void)(fs),(void)(node))
#define printinsn2(fs,node,pc,i) ((void)(fs),(void)(node),(void)(pc),(void)(i))
#endif /* LUA_DEBUG */


static void visitinsn2(DFuncState *fs, BlockNode *node, int pc, Instruction i)
{
#ifdef LUA_DEBUG
  printinsn2(fs, node, pc, i);
  printinsflags(fs, pc, "  flags for ");
  /* make sure this instruction hasn't already been visited */
  lua_assert(!test_ins_property(fs, pc, INS_VISITED));
  /* set this directly to avoid printing debug message every time */
  fs->a->insproperties[pc] |= (1 << INS_VISITED);
#endif /* LUA_DEBUG */
  UNUSED(fs); UNUSED(node); UNUSED(pc); UNUSED(i);
}


/*
** update pc variables based on CHILD; if it is not NULL, update
** NEXTCHILDSTARTPC to its startpc and NEXTPCLIMIT to 1 before its startpc,
** otherwise, set NEXTCHILDSTARTPC to (-1) and NEXTPCLIMIT to 1 before the endpc
** of PARENT
*/
static void initnextchild2(StackAnalyzer *sa, BlockNode *parent,
                           BlockNode *child, int *childstartpc)
{
  lua_assert(parent != NULL);
  lua_assert(childstartpc != NULL);
  if (child != NULL) {
    *childstartpc = child->startpc;
    sa->nextpclimit = child->startpc-1;
    /* update the next for-loop base register */
    if (isforloop(child))
      sa->nextforloopbase = getforloopbase(sa->code, child);
    else
      sa->nextforloopbase = sa->maxstacksize;
  }
  else {
    *childstartpc = -1;
    sa->nextpclimit = parent->endpc-1;
    sa->nextforloopbase = sa->maxstacksize;
  }
}


static BlockNode *updatenextchild2(StackAnalyzer *sa, BlockNode *parent,
                                    BlockNode *child, int *childstartpc)
{
  BlockNode *nextchild;
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
  lua_assert(prevcall == NULL || (prevcall >= fs->a->pendingstk.u.s2 &&
             prevcall < &fs->a->pendingstk.u.s2[fs->a->pendingstk.used]));
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


static void setfirstfree(DFuncState *fs, int newfirstfree)
{
  int numfree = fs->firstfree - newfirstfree;
  lua_assert(numfree >= 0);
  /* firstfree does not need to be an accessible slot as it can be equal to
     maxstacksize */
  if (isregvalid(fs, newfirstfree))
    memset(getslotdesc(fs, newfirstfree), 0, numfree * sizeof(SlotDesc));
  if (newfirstfree > 0) {
    ExpNode *lastreg = getexpinreg2(fs, newfirstfree-1);
    if (lastreg) {
      lastreg->nextregindex = 0;
      /* if the last used reg holds the current table constructor, then reset
         its firstarrayitem as it now points to a free register */
      if (lastreg->kind == ECONSTRUCTOR)
        lastreg->u.con.firstarrayitem = 0;
    }
  }
  fs->firstfree = newfirstfree;
}


static void flushpendingexp2(DFuncState *fs)
{
#ifdef LUA_DEBUG
  int i;
  for (i = 0; i < fs->a->pendingstk.used; i++) {
    ExpNode *exp = &fs->a->pendingstk.u.s2[i];
    lua_assert(exp->pending == 0);
  }
#endif /* LUA_DEBUG */
  fs->a->pendingstk.used = 0;
  D(lprintf("resetting fs->firstfree from %d to %d\n", fs->firstfree,
            fs->nactvar));
  setfirstfree(fs, fs->nactvar);
  fs->lastcallexp = 0;
  fs->curr_constructor = 0;
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


static void dumphashitem2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  int isfield;
  int rkkey;
  lua_assert(exp->kind == ESTORE);
  isfield = exp->u.store.rootop == OP_SETFIELD;
  rkkey = exp->u.store.aux2;
  if (isfield && ISK(rkkey) && ttisstring(&fs->f->k[INDEXK(rkkey)])) {
    CheckSpaceNeeded(D);
    DumpTString(rawtsvalue(&fs->f->k[INDEXK(rkkey)]), D);
  }
  else {
    struct HoldItem bracket;
    addliteralholditem2(D, &bracket, "[", 0);
    if (istempreg(fs, rkkey))
      dumpexpoperand2(D, fs, index2exp(fs, exp->prevregindex), exp, 0);
    else
      dumpRK2(D, fs, rkkey, 0);
    DumpLiteral("]",D);
  }
  DumpLiteral(" =",D);
  D->needspace = 1;
  if (exp->aux)
    dumpexpoperand2(D, fs, index2exp(fs, exp->aux), exp, 0);
  else
    dumpRK2(D, fs, exp->u.store.srcreg, exp);
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
/*  if (index2exp(fs, exp->prevregindex))
    index2exp(fs, exp->prevregindex)->nextregindex = exp->previndex;
  if (index2exp(fs, exp->nextregindex))
    index2exp(fs, exp->nextregindex)->prevregindex = exp->previndex;*/
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
      /* avoid dumping 2 minus signs in a row by using an inner paren for the
         operand */
      if (op == OPR_MINUS && o != NULL && o->kind == EUNOP &&
          o->u.unop.op == OPR_MINUS)
        o->u.unop.needinnerparen = 1;
      /* see explanation `case EBINOP' */
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
    case ECOMP:
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
    case ECONDITIONAL: {
      ExpNode *o1 = index2exp(fs, exp->u.cond.e1);
      ExpNode *o2 = index2exp(fs, exp->u.cond.e2);
      BinOpr op = exp->u.cond.goiftrue ? OPR_AND : OPR_OR;
      dumpexpoperand2(D, fs, o1, exp, priority[op].left);
      DumpSpace(D);
      DumpBinOpr(op,D);
      D->needspace = 1;
      dumpexpoperand2(D, fs, o2, exp, priority[op].right);
      D->needspace = 1;
      break;
    }
    case EINDEXED:
    case ESELF: {
      ExpNode *tab, *key;
      int b = exp->u.indexed.b;
      int c = exp->u.indexed.c;
      int isself = exp->kind == ESELF;
      int isfield = exp->u.indexed.isfield;
      struct HoldItem holditem; /* dot or open-brace */
      lua_assert((!isself && !isfield) || (isself != isfield));
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
      else if (isself == 0)
        addliteralholditem2(D, &holditem, "[", 0);
      else
        addliteralholditem2(D, &holditem, ":", 0);
      D->needspace = 0;
      if (c == -1) {
        lua_assert(isfield == 0);
        if (isself == 0)
          dumpexpoperand2(D, fs, key, exp, 0);
        else {
          TString *field;
          lua_assert(key != NULL);
          key->pending = 0;
          predumpexp2(D, fs, key);
          CHECK(fs, key->kind == ELITERAL, "invalid key in OP_SELF (register "
                "does not contain a string constant)");
          field = rawtsvalue(key->u.k);
          DumpTString(field,D);
          postdumpexp2(D, fs, key);
        }
      }
      else {
        if (isfield || isself) {
          TString *field = rawtsvalue(&fs->f->k[INDEXK(c)]);
          predumpexp2(D, fs, exp);
          DumpTString(field,D);
          postdumpexp2(D, fs, exp);
        }
        else {
          dumpRK2(D, fs, c, exp);
        }
      }
      if (isfield == 0 && isself == 0)
        DumpLiteral("]",D);
      if (needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
      break;
    }
    case ECONSTRUCTOR: {
      struct ExpListIterator iter;
      int needparen = (limit == SUBEXPR_PRIORITY || needparenforlineinfo);
      int totalitems = exp->u.con.narray + exp->u.con.nhash;
      if (needparen)
        addliteralholditem2(D, &holdparen, "({", 0);
      else
        addliteralholditem2(D, &holdparen, "{", 0);
      if (totalitems) {
        int i;
        int reg;
        ExpNode *nextarrayitem = index2exp(fs, exp->u.con.firstarrayitem);
        ExpNode *nexthashitem = index2exp(fs, exp->u.con.firsthashitem);
        lua_assert(nextarrayitem != NULL || nexthashitem != NULL);
        if (nextarrayitem != NULL)
          initexplistiter2(&iter, fs, exp->info+1, nextarrayitem);
        for (i = reg =0; i < totalitems; i++) {
          int dumparray;
          if (i != 0)
            DumpLiteral(",",D);
          if (nexthashitem == NULL)
            dumparray = 1;
          else if (nextarrayitem == NULL)
            dumparray = 0;
          else if (nexthashitem > nextarrayitem)
            dumparray = 1;
          else
            dumparray = 0;
          if (dumparray == 0) {
            lua_assert(nexthashitem != NULL);
            lua_assert(nexthashitem->kind == ESTORE);
            dumphashitem2(D, fs, nexthashitem);
            nexthashitem = index2exp(fs, nexthashitem->nextregindex);
          }
          else {
            dumpexp2(D, fs, nextarrayitem, 0);
            reg++;
            if (isregvalid(fs, iter.firstreg+reg))
              nextarrayitem = getnextexpinlist2(&iter, reg);
            else
              nextarrayitem = NULL;
          }
        }
      }
      DumpLiteral("}",D);
      if (needparen)
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
      if (needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      dumpexp2(D, fs, firstexp, SUBEXPR_PRIORITY);  /* called expression */
      DumpLiteral("(",D);  /* start of arguments */
      D->needspace = 0;
      narg -= (firstexp->kind == ESELF);
      if (narg != 0) {
        /* initialize the expression list iterator for dumping arguments */
        initexplistiter2(&iter, fs, exp->info+1+(firstexp->kind == ESELF),
                         firstexp);
        if (firstexp->kind == ESELF) {
          D(lprintf("firstexp->nextregindex = %d\n", firstexp->nextregindex));
        }
        for (i = 0; i < narg; i++) {
          if (i != 0)
            DumpLiteral(",",D);
          dumpexp2(D, fs, getnextexpinlist2(&iter, i), 0);
        }
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
                firstindex, exp->u.concat.lastindex));
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


static void commitlocalvars2(DFuncState *fs, int base, int n)
{
  int i;
  lua_assert(fs->nlocvars >= base+n);
  lua_assert(n > 0);
  for (i = 0; i < n; i++) {
    LocVar *var = getlocvar2(fs, base+i);
    setreglocal2(fs, base+i, var);
  }
  fs->nactvar += n;
  lua_assert(fs->firstfree <= fs->nactvar);
  fs->firstfree = fs->nactvar;
}


/*
** register N local variables in the active frame at REG, return the first
** varibale created
*/
static void pushlocalvars2(DFuncState *fs, int base, int n)
{
  lua_assert(n > 0);
  addlocalvars2(fs, n);
  commitlocalvars2(fs, base, n);
}


/*
** register N for-loop control variables
*/
static void commitcontrolvars2(DFuncState *fs, int base, int n)
{
  int i;
  for (i = 0; i < n; i++) {
    LocVar *var = getlocvar2(fs, base+i);
    setreglocal2(fs, base+i, var);
    set_reg_property(fs, base+i, REG_CONTROL);
  }
  fs->nactvar += n;
  if (fs->firstfree < fs->nactvar)
    fs->firstfree = fs->nactvar;
}


static ExpNode *addnilexp2(StackAnalyzer *sa, DFuncState *fs, int reg, int pc);


static void
dumpfornumheader2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  DecompState *D = fs->D;
  struct ExpListIterator iter;
  struct HoldItem initvar, initeq;
  struct HoldItem *loopheader = sa->currheader;
  struct LocVar *var;
  /* get the base control register */
  int base = GETARG_A(fs->f->code[node->endpc]);
  /* get the pending initial expression before creating the control variables */
  ExpNode *exp = getexpinreg2(fs, base);
  if (exp == NULL) {
    exp = addnilexp2(sa, fs, base, node->startpc - (node->startpc > 0));
    exp->aux = base+2;
  }
  /* create the control variables */
  commitcontrolvars2(fs, base, 3);
  /* create the loop variable */
  pushlocalvars2(fs, base+3, 1);
  var = getlocvar2(fs, base+3);
  /* add hold items for for-loop header */
  addliteralholditem2(D, loopheader, "for", 1);
  addholditem2(D, &initvar, getstr(var->varname), var->varname->tsv.len, 1);
  addliteralholditem2(D, &initeq, "=", 1);
  /* dump initializer expression */
  initexplistiter2(&iter, fs, base, exp);
  dumpexp2(D, fs, exp, 0);
  DumpComma(D);
  /* dump limit expression */
  exp = getnextexpinlist2(&iter, 1);
  dumpexp2(D, fs, exp, 0);
  DumpComma(D);
  /* dump step expression */
  exp = getnextexpinlist2(&iter, 2);
  /*exp = index2exp(fs, exp->nextregindex);*/
  dumpexp2(D, fs, exp, 0);
  DumpLiteral(" do", D);
  D->needspace = 1;
  flushpendingexp2(fs);
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
  int needblock = (test_ins_property(fs, pc, INS_BLOCKFOLLOW) == 0);
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
  needblock = (test_ins_property(fs, pc, INS_BLOCKFOLLOW) == 0);
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
      luaO_ptr2str(s, pvalue(o));
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
      predumpexp2(D,fs,exp);
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
    size_t len;
    struct LocVar *var = getlocvar2(fs, fs->nactvar+(i-firstreg));
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
    setreglocal2(fs, i, getlocvar2(fs, fs->nactvar++));
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
    /*needc initlocvar2(fs, i, &fs->locvars[fs->nactvar++]);*/
  }
  if (seenfirstexp == 0) {
    /* no expressions have been dumped, but the items in the hold need to be
       discharged and the line needs to be updated */
    firstexp->pending = 0;
    D->needspace = 0;
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
  if (exp->kind == ECONSTRUCTOR) {
    /*int e = exp2index(fs, exp);*/
    if (exp2index(fs, exp) == fs->curr_constructor)
      return exp;
   /* exp->aux = fs->curr_constructor;
    fs->curr_constructor = e;*/
  }
  if (!test_reg_property(fs, reg, REG_LOCAL)) {
    int link, lastreg;
    if (exp->kind == ENIL || exp->kind == ESELF)
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
    if (exp->kind == ECONDITIONAL)
      link =0 ;
    else if (exp->kind == ECALL || exp->kind == ETAILCALL)
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
    if (exp->kind == ESELF)
      pushexp2(fs, reg+1, exp, 0);
    pushexp2(fs, reg, exp, link);
    if (splitnil != NULL) *splitnil = 0;
    return exp;
  }
  else {
    lua_assert(fs->nactvar > 0);
    lua_assert(exp->info < fs->nactvar);
    exp->previndex = sa->laststore;
    sa->laststore = exp2index(fs, exp);
    if (exp->kind == ENIL && exp->aux >= fs->nactvar) {
      ExpNode *new;
      ExpNode node = *exp;  /* the node starts as local; the stack is about to
                               be discharged; add it to the stack after */
      node.info = fs->nactvar;  /* start this node at the first pending */
      exp->aux = fs->nactvar-1;  /* end the original node at the last local */
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
          fs->nactvar == sa->nextopenreg) {
        lua_assert(ispcvalid(fs, sa->pc+1));
        new->line = getline(fs->f, sa->pc+1);
        new->closeparenline = new->line;
      }
      if (splitnil != NULL) *splitnil = 1;
      lua_assert(!test_reg_property(fs, fs->nactvar, REG_LOCAL));
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


static void initexp2(DFuncState *fs, ExpNode *exp, int reg, int pc)
{
  exp->info = reg;
  exp->aux = 0;
  exp->previndex = exp2index(fs, NULL);
  if (reg == 0)
    exp->prevregindex = exp2index(fs, NULL);
  else
    exp->prevregindex = exp2index(fs, getexpinreg2(fs, reg-1));
  exp->nextregindex = exp2index(fs, NULL);
  exp->line = getline(fs->f,pc);
  exp->closeparenline = exp->line;
  exp->leftside = 0;
  exp->pending = 1;
  exp->goiftrue = 0;
}


static void linkexp2(StackAnalyzer *sa, DFuncState *fs, ExpNode *exp)
{
  ExpNode *prevreg = index2exp(fs, exp->prevregindex);
  sa->lastexpindex = exp2index(fs, exp);
  if (prevreg) {
    prevreg->nextregindex = sa->lastexpindex;
    if (prevreg->kind == ECONSTRUCTOR && prevreg->u.con.firstarrayitem == 0)
      prevreg->u.con.firstarrayitem = sa->lastexpindex;
  }
}


static ExpNode *addboolexp2(StackAnalyzer *sa, DFuncState *fs, int reg, int pc,
                            int b)
{
  ExpNode *exp = newexp(fs);
  initexp2(fs, exp, reg, pc);
  exp->kind = b ? ETRUE : EFALSE;
  linkexp2(sa, fs, exp);
  return exp;
}


static ExpNode *addnilexp2(StackAnalyzer *sa, DFuncState *fs, int reg, int pc)
{
  ExpNode *exp = newexp(fs);
  initexp2(fs, exp, reg, pc);
  exp->kind = ENIL;
  linkexp2(sa, fs, exp);
  return exp;
}


static ExpNode *addexp2(StackAnalyzer *sa, DFuncState *fs, int pc, OpCode o,
                        int a, int b, int c, int bx)
{
  ExpNode node;
  ExpNode *exp = &node;
  const Proto *f = fs->f;
  lua_assert(ispcvalid(fs, pc));
  initexp2(fs, exp, a, pc);
  switch (o) {
    case OP_GETGLOBAL: case OP_GETGLOBAL_MEM:
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
      if (test_ins_property(fs, pc, INS_BOOLLABEL)) {
        return NULL;
        return index2exp(fs, sa->lastcond.e);
      }
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
      exp->kind = ECONSTRUCTOR;
      exp->u.con.arrsize = luaO_fb2int(b);
      exp->u.con.hashsize = luaO_fb2int(c);
      exp->u.con.narray = exp->u.con.nhash = 0;
      /*exp->u.con.est = (exp->u.con.arrsize == 0 && exp->u.con.hashsize > 16)*/;
      exp->u.con.firstarrayitem = 0;
      exp->u.con.firsthashitem = 0;
      exp->u.con.lasthashitem = 0;
      break;
    case OP_SETLIST: {
      ExpNode *tab = getexpinreg2(fs, a);
      /* should always be non-NULL, but a lone OP_SETLIST can crash the program
         if this isn't checked */
      if (tab != NULL || tab->kind != ECONSTRUCTOR) {
        if (b == 0) {  /* b == 0 means set up to stack top */
          /* get number of array expressions pushed */
          b = fs->firstfree-(tab->info+1);
        }
        tab->u.con.narray += b;
        fs->curr_constructor = exp2index(fs, tab);
      }
      /* return the existing table constructor node */
      return tab;
    }
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
    case OP_SELF:
      exp->kind = ESELF;
      exp->u.indexed.b = b;
      exp->u.indexed.c = c;
      exp->u.indexed.isfield = 0;
      exp->aux = a+1;
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
    case OP_EQ: case OP_EQ_BK:
    case OP_LT: case OP_LT_BK:
    case OP_LE: case OP_LE_BK: {
      OpCode comp = o - (o % 2);  /* remove BK bit */
      BinOpr op;
      exp->kind = ECOMP;
      if (comp == OP_EQ)
        op = a ? OPR_EQ : OPR_NE;
      else {
        if (istempreg(fs, b) && istempreg(fs, c) && b > c) {
          int temp = b; b = c; c = temp;
          a = !a;
        }
        if (comp == OP_LT)
          op = a ? OPR_LT : OPR_GT;
        else /* OP_LE */
          op = a ? OPR_LE : OPR_GE;
      }
      exp->u.binop.b = b;
      exp->u.binop.c = c;
      exp->u.binop.op = op;
      exp->info = -1;
      break;
    }
    /* the remaining operations do not clobber A */
    default:
      return NULL;
  }
  if (exp->kind == EBINOP || exp->kind == ECOMP) {
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
  else if (exp->kind == EINDEXED || exp->kind == ESELF) {
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
  if (exp->kind != ESTORE && pc >= sa->lastcond.target)
    checkdischargestores2(sa,fs);
  exp = newexp(fs);
  *exp = node;
  linkexp2(sa, fs, exp);
  if (exp->kind == ESTORE) {
    exp->pending = 0;
    updatelaststore2(sa, fs, exp);
  }
  else if (exp->kind == ECALL || exp->kind == ETAILCALL)
    fs->lastcallexp = exp2index(fs, exp);  /* update last call node */
  return exp;
}


static void addconditionalnode2(StackAnalyzer *sa, DFuncState *fs, int pc,
                               int jumptarget)
{
  int targetbool = test_ins_property(fs, jumptarget, INS_BOOLLABEL);
  int e;
  int reg = -1;
  lu_byte goiftrue;
  ExpNode *exp = gettopexp(fs);
  if (exp != NULL && exp->kind == ECOMP) {
    if (targetbool) {
      /* see which bool label is targeted; if false, then this is a go-if-true
         node, e.g. `a == b and 1', because the boolean result of the first node
         will be used only if it is false */
      goiftrue = !GETARG_B(fs->f->code[jumptarget]);
      exp->info = GETARG_A(fs->f->code[jumptarget]);
    }
    else
      goiftrue = (!test_ins_property(fs, pc, INS_BRANCHFAIL) &&
                  !test_ins_property(fs, pc, INS_LOOPFAIL));
  }
  else {
    const Instruction *jc = getjumpcontrol(fs, pc);
    if (jc != NULL) {
      /* simply use the C arg in the test opcode; C=1 means jump if equivalent
         to false, i.e. go if true */
      goiftrue = !GETARG_C(*jc);
      if (GET_OPCODE(*jc) == OP_TESTSET)
        /*reg = GETARG_A(*jc);*/;
    }
    else if (exp == NULL) {
      if (targetbool) {
        int b = GETARG_B(fs->f->code[jumptarget]);
        exp = addboolexp2(sa, fs, GETARG_A(fs->f->code[jumptarget]), pc, b);
        goiftrue = !b;
      }
      else {
        exp = addnilexp2(sa, fs, 0, pc);
        goiftrue =1;
      }
    }
    else
      goiftrue = exp->goiftrue;
  }
  exp->goiftrue = goiftrue;
  /*exp->previndex = sa->lastcond.e;*/
  e = exp2index(fs, exp);
  if (sa->laststore == e)
    sa->laststore = exp->previndex;
  if (reg == -1) reg = exp->info;
  if (sa->lastcond.e) {
    ExpNode *prev = index2exp(fs, sa->lastcond.e);
    if (prev->kind == ECONDITIONAL)
      goiftrue = index2exp(fs, prev->u.cond.e2)->goiftrue;
    else
      goiftrue = prev->goiftrue;
    exp = newexp(fs);
    initexp2(fs, exp, reg, pc);
    exp->kind = ECONDITIONAL;
    exp->u.cond.e1 = sa->lastcond.e;
    exp->u.cond.e2 = e;
    exp->u.cond.goiftrue = exp->goiftrue = goiftrue;
    exp->u.cond.pc = pc;
    linkexp2(sa, fs, exp);
    e = exp2index(fs, exp);
  }
  addexptoreg2(sa, fs, reg, exp, NULL);
  sa->lastcond.e = e;
  sa->lastcond.target = jumptarget;
  sa->lastcond.pc = pc;
}


static int addstore2(StackAnalyzer *sa, DFuncState *fs, int pc, OpCode o, int a,
                     int b, int c, int bx)
{
  ExpNode *exp;
  OpCode rootop;
  int srcreg;
  int info;
  int aux1, aux2;
  switch (o) {
    case OP_SETFIELD: case OP_SETFIELD_R1:
      /* OP_SETFIELD is used as the root if it should be written as a field */
      if (rawtsvalue(&fs->f->k[b])->tsv.reserved > 0)
        rootop = OP_SETTABLE;
      else
        rootop = OP_SETFIELD;
      b = RKASK(b);
      goto addstoretable;
    case OP_SETTABLE:
    case OP_SETTABLE_BK:
    case OP_SETTABLE_S:
    case OP_SETTABLE_S_BK:
    case OP_SETTABLE_N:
    case OP_SETTABLE_N_BK:
      rootop = OP_SETTABLE;
      addstoretable:
      srcreg = c;
      info = a;
      aux1 = a;  /* table register */
      aux2 = b;  /* index register */
      break;
    case OP_SETGLOBAL:
      CHECK(fs, isregvalid(fs, a),
            "invalid register referenced in OP_SETGLOBAL");
      srcreg = a;
      info = -1;
      rootop = OP_SETGLOBAL;
      aux1 = bx;
      aux2 = 0;
      break;
    case OP_SETUPVAL:
    case OP_SETUPVAL_R1:
      CHECK(fs, isregvalid(fs, a),
            "invalid register referenced in OP_SETUPVAL");
      srcreg = a;
      info = -1;
      rootop = OP_SETUPVAL;
      aux1 = b;
      aux2 = 0;
      break;
    default:
      return 0;
  }
  if (!ISK(srcreg) && test_reg_property(fs, srcreg, REG_LOCAL)) {
    ExpNode *top = gettopexp(fs);
    if (top != NULL && top->kind != ESTORE)
      checkdischargestores2(sa, fs);
  }
  exp = newexp(fs);
  exp->kind = ESTORE;
  exp->info = info;
  exp->u.store.aux1 = aux1;
  exp->u.store.aux2 = aux2;
  exp->u.store.rootop = rootop;
  if (!istempreg(fs, srcreg))
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


static ExpNode *addhashitem2(DFuncState *fs, int pc, OpCode o, int a, int b,
                             int c)
{
  ExpNode *exp;
  OpCode rootop;
  int info, aux1, aux2, srcreg;
  switch (o) {
    case OP_SETFIELD: case OP_SETFIELD_R1:
      /* OP_SETFIELD is used as the root if it should be written as a field */
      if (rawtsvalue(&fs->f->k[b])->tsv.reserved > 0)
        rootop = OP_SETTABLE;
      else
        rootop = OP_SETFIELD;
      b = RKASK(b);
      goto addstoretable;
    case OP_SETTABLE:
    case OP_SETTABLE_BK:
    case OP_SETTABLE_S:
    case OP_SETTABLE_S_BK:
    case OP_SETTABLE_N:
    case OP_SETTABLE_N_BK:
      rootop = OP_SETTABLE;
      addstoretable:
      srcreg = c;
      info = a;
      aux1 = a;  /* table register */
      aux2 = b;  /* index register */
      break;
    default:
      return NULL;
  }
  exp = newexp(fs);
  exp->kind = ESTORE;
  exp->info = info;
  exp->u.store.aux1 = aux1;
  exp->u.store.aux2 = aux2;
  exp->u.store.rootop = rootop;
  if (!istempreg(fs, srcreg))
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
  return exp;
}


static int openexpr2(StackAnalyzer *sa, DFuncState *fs)
{
  const OpenExpr *e = sa->nextopenexpr;
  int limit;  /* pc limit */
  lua_assert(e != NULL);
  lua_assert(sa->nextopenreg != -1);
  lua_assert(e->endpc >= sa->pc);
  lua_assert(ispcvalid(fs, e->endpc));
  sa->openexprkind = e->kind;
  lua_assert(sa->openexprnildebt >= 0);
  if (sa->openexprnildebt)
    dischargenildebt2(sa, fs, sa->nextopenreg);
  updatenextopenexpr2(sa, fs);  /* discharge this one now */
  /* HASHTABLEPREP expressions don't have a termination code */
  limit = e->endpc + (e->kind == HASHTABLEPREP);
  for (; sa->pc < limit; sa->pc++) {
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
    visitinsn2(fs, sa->currbl, pc, i);
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
      else if ((exp = addhashitem2(fs, pc, o, a, b, c))) {
        /* a is the table in the current constructor */
        ExpNode *tab = getexpinreg2(fs, a);
        /* this should always be true, but if its not checked, malformed code
           could cause a crash */
        if (tab && tab->kind == ECONSTRUCTOR) {
          ExpNode *last = index2exp(fs, tab->u.con.lasthashitem);
          int e = exp2index(fs, exp);
          if (last) {
            lua_assert(last->kind == ESTORE);
            /* `nextregindex' is not used in ESTORE nodes, so I use it to
               chain hash items in a constructor */
            last->nextregindex = e;
          }
          else {
            tab->u.con.firsthashitem = e;
          }
          tab->u.con.lasthashitem = e;
          /* similarly, I use `prevregindex' to save the exp index of the
             current node in registre B if it is a temporary register */
          if (istempreg(fs, exp->u.store.aux2)) {
            exp->prevregindex = exp2index(fs, getexpinreg2(fs, b));
            setfirstfree(fs, exp->u.store.aux2);
          }
          tab->u.con.nhash++;
        }
      }
    }
    UNUSED(sbx);
  }
  sa->openexprkind = -1;
  return limit - e->startpc;
}


static int clamptoreg(int base, int n, int limit) {
  int res = base+n;
  if (res > limit)
    return limit-base;
  return n;
}


static int getnvarstartatpc2(DFuncState *fs, int pc, int reglimit) {
  if (ispcvalid(fs, pc)) {
    int nvars = varstartsatpc2(fs, pc);
    if (nvars == 0) return 0;
    return clamptoreg(fs->nactvar, nvars, reglimit);
  }
  return -1;
}


static void enterblock2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node,
                        struct HoldItem *blockheader)
{
  DecompState *D = fs->D;
  /*if (node->type == BL_FUNCTION && fs->prev == NULL)
    return;*/
  if (node->type != BL_FUNCTION || fs->prev != NULL)
    lua_assert(D->indentlevel >= 0);
  sa->currheader = blockheader;
  switch (node->type) {
    case BL_FUNCTION: {
      int numvars, actualnumvars;
      lua_assert(fs->nlocvars == fs->f->numparams);
      actualnumvars = varstartsatpc2(fs, 0);
      numvars = clamptoreg(fs->nactvar, actualnumvars, sa->nextforloopbase);
      if (numvars) {
        /* this is a little hack to make dumping work before the function body
           has technically been entered */
        D->indentlevel++;
        addlocalvars2(fs, actualnumvars);
        /* Lua does not emit LOADNIL if it is the first instruction and it is
           not a label, so update FIRSTFREE manually */
        fs->firstfree += numvars;
        /* see if there is room for a line feed, so that this declaration has a
           chance of being on its own line (otherwise it will be on the same
           line as the statement after it) */
        if (getline(fs->f, 0) > D->linenumber)
          beginline2(fs, 1, D);
        initlocvars2(fs, fs->nactvar, numvars);
        D->indentlevel--;
      }
      break;
    }
    case BL_REPEAT: addliteralholditem2(D, blockheader, "repeat", 1); break;
    case BL_FORNUM: {
      dumpfornumheader2(sa, fs, node);
      break;
    }
    case BL_DO: addliteralholditem2(D, blockheader, "do", 1); break;
  }
  sa->numforloopvars = 0;  /* discharge */
}


static void leaveblock2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  UNUSED(sa); UNUSED(fs); UNUSED(node);
}


#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*
** Second pass `block node' handler - the function's registers and stack are
** analyzed and local variables are detected. With information about the local
** variables, any undetected do-end blocks can be detected in this pass.
*/
static void blnode2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
#ifdef HKSC_DECOMP_HAVE_PASS2
  struct HoldItem blockheader;
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  DecompState *D = fs->D;
  const Instruction *code = sa->code;
  const lu_byte nactvar = fs->nactvar;
  BlockNode *nextchild = node->firstchild;
  int nextchildstartpc;
  /*int startpc = node->startpc;*/
  int endpc = node->endpc;
  /*int type = node->type;*/
  assertblvalid(sa,fs,node);
  /* initialize NEXTCHILDSTARTPC and NEXTPCLIMIT */
  initnextchild2(sa, node, nextchild, &nextchildstartpc);
  debugenterblock2(sa, fs, node);
#ifdef HKSC_DECOMP_HAVE_PASS2
  enterblock2(sa, fs, node, &blockheader);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  /* mark this block as visited */
  lua_assert(node->visited == 0);
  D(node->visited = 1);
  if (node->isempty) /* block has no instructions */
    goto block2finished;
  D->indentlevel++;
  /* main instruction loop */
  for (; sa->pc < sa->sizecode; sa->pc++) {
    int pc, a, b, c, bx, sbx;
    Instruction i;
    OpCode o;
    int numvars; /* number of variables which start at PC+1 */
    int actualnumvars;
    if (sa->pc == nextchildstartpc) {
      /* save NEXTCHILD->ISEMPTY to use after updating NEXTCHILD */
      int waschildempty;
      processnextchild:
      lua_assert(nextchild != NULL);
      waschildempty = nextchild->isempty;
      blnode2(sa, fs, nextchild);
      nextchild = updatenextchild2(sa, node, nextchild, &nextchildstartpc);
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
    visitinsn2(fs, node, pc, i); /* visit this instruction */
    /* make sure to run the first pass on all nested closures */
    if (o == OP_CLOSURE) { /* nested closure? */
      const Proto *f = fs->f->p[bx];
      DecompileFunction(D,f);
    }
    UNUSED(numvars); UNUSED(actualnumvars);
#else /* !HKSC_DECOMP_DEBUG_PASS1 */
    actualnumvars = ispcvalid(fs, pc+1) ? varstartsatpc2(fs, pc+1) : -1;
    numvars = clamptoreg(fs->nactvar, actualnumvars, sa->nextforloopbase);
    /* check if this instruction begins preparation code */
    while (sa->nextopenexpr != NULL && sa->nextopenexpr->startpc == pc) {
      int kind = sa->nextopenexpr->kind;
      int isfor = (kind == FORNUMPREP || kind == FORLISTPREP);
      int exprsize = (sa->currbl = node, openexpr2(sa, fs));
      if (exprsize) {
        /* pc has chanegd */
        pc = sa->pc;
        i = code[pc];
        o = GET_OPCODE(i);
        a = GETARG_A(i);
        b = GETARG_B(i);
        c = GETARG_C(i);
        bx = GETARG_Bx(i);
        sbx = GETARG_sBx(i);
      }
      /* all open expressions beside hashtables have a final opcode that ends
         the expression and provides an instruction-worth of breathing room for
         the main loop to catch new variables and emit them, but hashtables
         don't have a terminal opcode, so variables can start at the pc right
         after the hashtable evluation */
      {
        int numvars = varstartsatpc2(fs, pc);
        if (numvars > 0) {
          addlocalvars2(fs, numvars);
          if (isfor) numvars -= 3;
          initlocvars2(fs, fs->nactvar, numvars);
        }
      }
      if (isfor) {
        if (exprsize)
          actualnumvars = getnvarstartatpc2(fs, pc+1, sa->maxstacksize);
        sa->numforloopvars = actualnumvars;
        numvars = actualnumvars = 0;
      }
      else if (exprsize) {
        actualnumvars = ispcvalid(fs, pc+1) ? varstartsatpc2(fs, pc+1) : -1;
        numvars = clamptoreg(fs->nactvar, actualnumvars, sa->nextforloopbase);
      }
    }
    addlocalvars2(fs, actualnumvars);
    visitinsn2(fs, node, pc, i); /* visit this instruction */
    if (testAMode(o) || testTMode(o)) { /* A is a register */
      ExpNode *exp;
      lua_assert(!test_ins_property(fs, pc, INS_BREAKSTAT));
      exp = addexp2(sa, fs, pc, o, a, b, c, bx);
      if (exp != NULL) {
        int splitnil;
        D(lprintf("created new expression node\n"));
        D(lprintf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(lprintf("---------------------------\n"));
        if (exp->kind != ESTORE && exp->info != -1)
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
        if (pc+1 == sa->lastcond.target) {
           addconditionalnode2(sa, fs, pc, sa->lastcond.target);
           sa->lastcond.e = 0;
           sa->lastcond.target = -1;
           sa->lastcond.pc = -1;
        }
        if (numvars > 0) {
          checkdischargestores2(sa, fs);
          D(lprintf("NEW LOCAL VARIABLE\n"));
          (void)getfirstexp;
          initlocvars2(fs, fs->nactvar, numvars);
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
      if (o == OP_JMP) {
        int target = pc+1+sbx;
        if (test_ins_property(fs, pc, INS_BREAKSTAT)) {
          checkdischargestores2(sa, fs);
          lua_assert(node->type >= BL_WHILE && node->type <= BL_FORLIST);
          emitbreakstat2(fs, pc);
        }
        else {
          addconditionalnode2(sa, fs, pc, target);
        }
      }
      else
        checkdischargestores2(sa, fs);
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
#ifdef HKSC_DECOMP_HAVE_PASS2
  leaveblock2(sa, fs, node);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  debugleaveblock2(sa, fs, node);
  fs->nactvar = nactvar;
}


static void addparams(DFuncState *fs, const Proto *f)
{
  fs->nlocvars = f->numparams;
  if (f->numparams)
    pushlocalvars2(fs, 0, f->numparams);
}


/*
** initialize memory for second pass
*/
static void initpass2(DFuncState *fs)
{
  Analyzer *a = fs->a;
  a->decomppass = 2;
  a->pendingstk.total = 4;
  a->pendingstk.used = 0;
  a->pendingstk.u.s2 = luaM_newvector(fs->H, a->pendingstk.total, ExpNode);
}


/*
** free memory for second pass
*/
static void cleanuppass2(DFuncState *fs)
{
  Analyzer *a = fs->a;
  lua_assert(a->decomppass == 2);
  luaM_freearray(fs->H, a->pendingstk.u.s2, a->pendingstk.total, ExpNode);
  a->pendingstk.u.s2 = NULL;
  a->pendingstk.total = a->pendingstk.used = 0;
}


static void pass2(const Proto *f, DFuncState *fs)
{
  BlockNode *functionblock = fs->a->bllist.first;
  StackAnalyzer sa;
  sa.pc = 0;
  sa.code = f->code;
  sa.sizecode = f->sizecode;
  sa.maxstacksize = f->maxstacksize;
  sa.intailemptyblock = 0;
  sa.inheadercondition = 0;
  sa.lastexpindex = 0;
  sa.laststore = 0;
  sa.lastcond.e = 0;
  sa.lastcond.target = -1;
  sa.lastcond.pc = -1;
  sa.openexprkind = -1;
  sa.openexprnildebt = 0;
  sa.numforloopvars = 0;
  fs->nactvar = 0;
  lua_assert(fs->firstfree == 0);
  lua_assert(functionblock != NULL);
  lua_assert(functionblock->type == BL_FUNCTION);
  initpass2(fs);
#ifdef HKSC_DECOMP_HAVE_PASS2
  addparams(fs, f);
  updatenextopenexpr2(&sa, fs);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  blnode2(&sa, fs, functionblock);
#ifdef LUA_DEBUG
  { /* debug: make sure all instructions were visited */
    int pc;
    for (pc = 0; pc < f->sizecode; pc++)
      check_ins_property(fs, pc, INS_VISITED);
    checktreevisited(functionblock);
  }
#endif /* LUA_DEBUG */
  cleanuppass2(fs);
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
