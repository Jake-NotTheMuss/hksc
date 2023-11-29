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
#include <ctype.h>

#define ldecomp_c
#define LUA_CORE

#include "hksclua.h"

#include "lanalyzer.h"
#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "llex.h"
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

#define MARKED_GLOBAL cast_byte(NUM_RESERVED+1)
#define CONFLICTING_GLOBAL cast_byte(NUM_RESERVED+2)


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

struct LoopState;
struct BlockState;
struct pendingstorechain1;


#define VEC_STRUCT(T,name) \
  struct { T *s; int used, alloc; } name

#define VEC_INIT(name) ((name).s = NULL, (name).used = 0, (name).alloc = 0)

#define VEC_FREE(H,name,T) luaM_freearray(H, (name).s, (name).alloc, T)

#define growvector(H,st,t) \
  luaM_growvector(H,(st).s,(st).used,(st).alloc,t,MAX_INT,"")

#define SIZE_STATIC_KMAP 2

/*
** DecompState - state of decompilation across all functions
*/
typedef struct {
  hksc_State *H;
  struct DFuncState *fs;  /* current function state */
  lua_Writer writer;
  void *data;
  const char *name;  /* input name */
  int status;
  int rescan;  /* true if rescanning code in the first pass */
  int usedebuginfo;  /* true if using debug info */
  int matchlineinfo;  /* true if matching statements to line info */
  int funcidx;  /* n for the nth function that is being decompiled */
  int indentlevel;  /* indentation level counter */
  int linenumber;  /* output line counter */
  int nextlinenumber;  /* used when not using line info */
  int needspace;  /* for adding space between tokens */
  int maxtreedepth;  /* maximum number of function states that will be active at
                        once during the decompilation */
  /* Pass 1 dynamic vectors - instances of these arrays to do not need to exist
     per-function, because the first pass is not recursive, i.e. each closure in
     the program is analyzed consecutively */
  VEC_STRUCT(struct LoopState, loopstk);
  VEC_STRUCT(struct BlockState, blockstk);
  VEC_STRUCT(BlockNode *, freeblocknodes);
  VEC_STRUCT(lu_byte, varnotes);
  /* constants bitmap - each bit represents whether the corresponding constant
     has been referenced in the current function - used in the first pass when
     generating variable info */
  lu_int32 *kmap;
  /* if the function has <= 32*SIZE_STATIC_KMAP constants, no heap allocation is
     needed, this static array can be used */
  lu_int32 kmap_1[SIZE_STATIC_KMAP];
  int sizekmap;
  /* this structure is for per-function data that only needs to be instanced
     once at any time, because it is only needed in the initial passes where
     functions are processsed consecutively, not recursively */
  struct {
    const OpenExpr *openexpr;
    struct BlockState *bl;
    struct pendingstorechain1 *store;
    BlockNode *prevnode, *nextnode;
    int currvarlimit;
    int nclose;
    int skippedstorerefpc;
    int lastup;
    int laststat;
    int testsetendlabel;  /* current TESTSET end label or -1 */
    /* generic `prepstart' variable for loops/branches, it is calculated before
       the loop/branch block is entered, so it must be saved temporarily in the
       DecompState before being saved to the loop or branch block state */
    int genprepstart;
#define applyprepstart(D)  \
  (void)((D)->a.bl->prepstart = (D)->a.genprepstart, (D)->a.genprepstart = -1)
    struct {
      OpCode o;
      int a, b, c, bx, sbx;
    } insn;
  } a;
  /* Pass 2 data that does not need to exist per-function */
  struct HoldItem *holdfirst;  /* first hold item in the chain */
  struct HoldItem *holdlast;  /* last hold item in the chain */
  Mbuffer buff;  /* buffer for building strings */
  struct {
    TString *name;  /* function name or NULL */
    /* pc and line-mapping of the OP_CLOSURE code */
    /* HAVESELF is true if the artificial variable SELF is present */
    int pc, line, haveself;
  } lastcl;  /* data for the last encountered closure */
} DecompState;


/*
** ensure D->kmap has enough space for NK constants
*/
static void allockmap (DecompState *D, int nk)
{
  int prevnumblocks = D->sizekmap;
  int numblocks = (nk + 31) >> 5;
  if (prevnumblocks > SIZE_STATIC_KMAP) {
    /* current vector is dynamically allocated, grow if needed */
    if (numblocks > prevnumblocks) {
      luaM_reallocvector(D->H, D->kmap, prevnumblocks, numblocks, lu_int32);
      D->sizekmap = numblocks;
    }
  }
  else {
    /* current vector is static, allocate on the heap if needed */
    if (numblocks > SIZE_STATIC_KMAP)
      D->kmap = luaM_newvector(D->H, numblocks, lu_int32);
    else
      D->kmap = D->kmap_1;
    D->sizekmap = numblocks;
  }
  memset(D->kmap, 0, numblocks * sizeof(lu_int32));
}


/*
** free D->kmap
*/
static void freekmap (DecompState *D)
{
  if (D->sizekmap > SIZE_STATIC_KMAP) {
    lua_assert(D->kmap != D->kmap_1);
    luaM_freearray(D->H, D->kmap, D->sizekmap, lu_int32);
  }
  D->kmap = NULL;
  D->sizekmap = 0;
}


/*
** set bit K in the constants bitmap, marking it as having been referenced
*/
static void setkreferenced (DecompState *D, unsigned int k)
{
  D->kmap[k >> 5] |= (1u << (k & 31));
}


/*
** test bit K in the constants bitmap
*/
static int iskreferenced (DecompState *D, unsigned int k)
{
  return ((D->kmap[k >> 5] & (1u << (k & 31))) != 0);
}


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
  BlockNode *root;  /* the root lexical block of the function */
  short nlocvars;  /* number of local variables created so far */
  lu_byte nactvar;
  lu_byte seenstatinblock;  /* true if a simple statement has been encountered
                               in the current block node */
  int pc;
  int inopenexpr;
  int sizelocvars;
  int sizeupvalues;
  int firstclob;  /* first pc that clobbers register A */
  int firstclobnonparam;  /* first pc that clobbers non-parameter register A */
  int firstfree;
  int lastcallexp;  /* exp index of last function call node */
  int curr_constructor;  /* exp index of current table constructor */
  int nopencalls;  /* number of OpenExpr entries created */
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
          printf("(%s) (%d-%d)", bltypename(block->kind), block->startpc+1,
                 block->endpc+1);
        else
          printf("(NULL)");
        break;
      }
      case 'b': { /* BlockNode * (type only) */
        BlockNode *block = va_arg(argp, BlockNode *);
        if (block != NULL)
          printf("%s", bltypename(block->kind));
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
    if (fs->a->regproperties[reg].flags & (1 << i))
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
** the startpc of a node or -1 if node is NULL
*/
#define NODE_STARTPC(node) ((node) ? (node)->startpc : -1)

/* empty blocks start 1 pc after they end; the startpc to use for comparisons is
   the emptiness subtracted from the startpc (consider a tail-empty child block
   which has an actual startpc that is greater than its parent block's endpc, or
   similarly, a previous empty sibling block that ends immediately before its
   next sibling block, which would have a startpc that is equal to its sibling
   block's startpc) */
#define blstartpc(bl)  check_exp(bl, cast_int((bl)->startpc - (bl)->isempty))

#define isforloop(bl)  ((bl)->kind == BL_FORNUM || (bl)->kind == BL_FORLIST)

/*
** initialize a BlockNode struct
*/
static void initblnode(BlockNode *node, int startpc, int endpc, int kind) {
  node->nextsibling = NULL;
  node->firstchild = NULL;
  node->startpc = startpc;
  node->endpc = endpc;
  node->kind = kind;
  node->isempty = (endpc < startpc);
  node->upval = 0;
  node->iselseif = 0;
  node->hardstatbeforechild = 0;
  node->repuntiltrue = 0;
  node->parentnilvars = 0;
  D(node->visited = 0);
}


/*
** true if NODE represents a loop scope
*/
static int isloopnode(const BlockNode *node)
{
  int k = node->kind;
  return (k == BL_WHILE || k == BL_REPEAT || k == BL_FORNUM || k == BL_FORLIST);
}


/*
** re-calculate the emptiness of a block
*/
static void recalcemptiness(BlockNode *node) {
  node->isempty = (node->endpc < node->startpc);
}


/*
** true if the if-block represented by NODE has en else-part
*/
static int haselsepart(const BlockNode *node) {
  lua_assert(node != NULL);
  lua_assert(node->kind == BL_IF);
  return (node->nextsibling != NULL && node->nextsibling->kind == BL_ELSE);
}


/*
** returns the pc where a natural OP_CLOSE would occur within NODE, natural
** meaning that the node alone generates the OP_CLOSE code, not an inner
** do-block
*/
static int getnaturalclosepc(const BlockNode *node)
{
  switch (node->kind) {
    case BL_FORLIST: return node->endpc-2;
    case BL_IF: case BL_ELSE: return node->endpc;
    default: return node->endpc-1;
  }
}


/*
** returns the natural endpc for local variables declared inside NODE
*/
static int getnaturalvarendpc(const BlockNode *node)
{
  int pc;
  switch (node->kind) {
    /* repeat-loops without upvalues have variables end 1 after the endpc,
       whereas repeat-loops with upvalues have variables end on the OP_CLOSE,
       which is 1 before the endpc */
    case BL_REPEAT: return node->endpc+1 - 2*node->upval;
    case BL_FORLIST: pc = node->endpc-1; break;
    case BL_IF: pc = node->endpc+!haselsepart(node); break;
    /* variables in do-blocks that have upvalues end on the OP_CLOSE code, while
       variables in do-blocks that don't have upvalues end 1 after the last pc
       in the block */
    case BL_DO: pc = node->endpc+1; break;
    default: /* FUNCTION, WHILE, FORNUM, ELSE */ pc = node->endpc; break;
  }
  return pc - node->upval;
}


#ifdef HKSC_DECOMP_HAVE_PASS2
/*
** true if CHILD is a tail-block of PARENT, i.e. CHILD is the last statement in
** PARENT
*/
static int istailblock(const BlockNode *parent, const BlockNode *child) {
  int hasendcode;
  if (child->nextsibling != NULL)
    return 0;
  switch (parent->kind) {
    case BL_FUNCTION: case BL_REPEAT: return 0;
    case BL_DO: case BL_ELSE: hasendcode = 0; break;
    case BL_IF: hasendcode = haselsepart(parent); break;
    default: hasendcode = 1; break;
  }
  return (child->endpc + hasendcode == parent->endpc);
}

static int issinglelinefunc(DFuncState *fs, const BlockNode *func)
{
  const Proto *f = fs->f;
  lua_assert(func->kind == BL_FUNCTION);
  if (fs->D->usedebuginfo == 0)
    return 0;
  return (getline(f, func->startpc) == getline(f, func->endpc));
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


static int getforloopbase(const Instruction *code, const BlockNode *node) {
  lua_assert(isforloop(node));
  if (node->kind == BL_FORNUM)
    return GETARG_A(code[node->endpc]);
  else
    return GETARG_A(code[node->endpc-1]);
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


static int hasmultret(ExpNode *exp)
{
  return (exp->kind == ECALL || exp->kind == EVARARG);
}


static int getexpline(ExpNode *exp)
{
  int line = exp->line;
  if (exp->kind == ECONSTRUCTOR && exp->aux > 0)
    line = exp->aux;  /* use line-mapping of OP_SETLIST or final hash item */
  return line;
}


static int gethighestexpreg(ExpNode *exp)
{
  int numextraregs = 0;
  if (hasmultret(exp))
    numextraregs = exp->kind == ECALL ? exp->u.call.nret-1 : exp->aux-2;
  return exp->info + numextraregs;
}


/*
** check if an expression returns multiple values REG is the last one clobbered
*/
static int hasmultretuptoreg(ExpNode *exp, int reg)
{
  if (hasmultret(exp))
    return gethighestexpreg(exp) == reg;
  return 0;
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
    case ECONSTRUCTOR:
      lprintf("'{}' %d, %d", exp->u.cons.arrsize, exp->u.cons.hashsize);
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
  DecompState *D = fs->D;
  const Instruction *code;
  int i, pc;
  lua_assert(parent != NULL && parent->f != NULL);
  code = parent->f->code;
  lua_assert(ispcvalid(parent, D->lastcl.pc));
  lua_assert(GET_OPCODE(code[D->lastcl.pc]) == OP_CLOSURE);
  pc = D->lastcl.pc+1;
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

#define IS_RESERVED(ts) \
  ((ts)->tsv.reserved > 0 && (ts)->tsv.reserved < MARKED_GLOBAL)


/*
** returns true if NAME can be written as a field
*/
static int isfieldname(TString *name)
{
  const char *str;
  lua_assert(name != NULL);
  if (IS_RESERVED(name))
    return 0;
  str = getstr(name);
  if (isalpha(*str) || *str == '_') {
    size_t i;
    for (i = 1; i < name->tsv.len; i++) {
      if (!isalnum(str[i]) && str[i] != '_')
        return 0;
    }
    return 1;
  }
  return 0;
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


static int varisaugmented(struct LocVar *var) {
  TString *name = var->varname;
  lua_assert(name != NULL);
  return (*getstr(name) == '(');
}


/*
** call this version before debug info is generated in pass1
*/
static int getline1(DFuncState *fs, int pc) {
  return getline(fs->f, pc);
}


#ifdef HKSC_DECOMP_HAVE_PASS2
/*
** call this version after debug info is generated
*/
static int getline2(DFuncState *fs, int pc) {
  lua_assert(ispcvalid(fs, pc));
  if (fs->D->matchlineinfo)
    return fs->f->lineinfo[pc];
  else
    return fs->D->nextlinenumber;
}
#endif /* HKSC_DECOMP_HAVE_PASS2 */


static void open_func (DFuncState *fs, DecompState *D, const Proto *f) {
  hksc_State *H = D->H;
  Analyzer *a = luaA_newanalyzer(H);
  D->loopstk.used = D->blockstk.used = 0;
  fs->a = a;
  fs->prev = D->fs;  /* linked list of funcstates */
  fs->D = D;
  fs->H = H;
  D->fs = fs;
  fs->f = f;
  fs->root = NULL;
  fs->idx = D->funcidx++;
  fs->nlocvars = 0;
  fs->nactvar = 0;
  fs->pc = 0;
  fs->inopenexpr = 0;
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


static BlockNode *addblnode(DFuncState *fs, int startpc, int endpc, int kind) {
  DecompState *D = fs->D;
  BlockNode *new_node;
  if (D->freeblocknodes.used > 0)
    new_node = D->freeblocknodes.s[--D->freeblocknodes.used];
  else {
    Analyzer *a = fs->a;
    BlockNode *curr = a->bllist.first;
    new_node = luaM_new(fs->H, BlockNode);
    new_node->next = curr;
    a->bllist.first = new_node;
    if (curr == NULL)
      a->bllist.last = new_node;
  }
  initblnode(new_node, startpc, endpc, kind);
  D(lprintf("recording new block node of type %b (%i-%i)\n", new_node, startpc,
            endpc));
  return new_node;
}


static void freeblnode(DFuncState *fs, BlockNode *node)
{
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  growvector(H, D->freeblocknodes, BlockNode *);
  D->freeblocknodes.s[D->freeblocknodes.used++] = node;
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


static const char *TValueToString(const TValue *o, DecompState *D)
{
  TString *res;
  switch (ttype(o))
  {
    case LUA_TNIL:
      return "nil";
    case LUA_TBOOLEAN:
      return bvalue(o) ? "true" : "false";
    case LUA_TLIGHTUSERDATA: {
      char s[LUAI_MAXUI642STR+sizeof("0xhi")-1];
      luaO_ptr2str(s, pvalue(o));
      res = luaS_new(D->H, s);
      break;
    }
    case LUA_TNUMBER: {
      char s[LUAI_MAXNUMBER2STR];
      lua_number2str(s, nvalue(o));
      res = luaS_new(D->H, s);
      break;
    }
    case LUA_TSTRING:
      res = luaO_kstring2print(D->H, rawtsvalue(o));
      break;
    case LUA_TUI64: {
      char s[LUAI_MAXUI642STR+sizeof("oxhl")-1];
      lua_ui642str(s+2, ui64value(o));
      s[0] = '0'; s[1] = 'x';
      strcat(s, "hl");
      res = luaS_new(D->H, s);
      break;
    }
    default:
      return "";
  }
  return getstr(res);
}


/*
** prints a Lua object as it would appear in source code to output
*/
static void DumpTValue(const TValue *o, DecompState *D)
{
  const char *str = TValueToString(o, D);
  DumpString(str, D);
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
  BlockNode *node = fs->root;
  lprintf("BLOCK SUMMARY\n"
         "-------------------\n");
  lua_assert(node != NULL);
  lua_assert(node->kind == BL_FUNCTION);
  lua_assert(node->nextsibling == NULL);
  debugblnode(fs, node, 0);
  lprintf("-------------------\n");
  node = fs->a->bllist.first;
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
  for (i = fs->a->sizeopencalls-1; i >= 0; i--)
    debugopenexpr(&fs->a->opencalls[i]);
  lprintf("-------------------\n");
}


static void debugpass1summary(DFuncState *fs)
{
  debugblocksummary(fs); lprintf("\n");
  debugopenexprsummary(fs); lprintf("\n");
  {
    int pc;
    for (pc = 0; pc < fs->f->sizecode; pc++)
      printinsflags(fs, pc, "");
  }
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
** returns true if open expressions of type KIND leave a result in a free
** register that is yet to be used
*/
static int exprpushesresult(int kind)
{
  switch (kind) {
    case CALLPREP: case SETLISTPREP: case HASHTABLEPREP: case EMPTYTABLE:
    return 1;
    /* CONCAT can store its result to a local register */
    default: return 0;
  }
}


static int isstorecode(OpCode o)
{
  switch (o) {
    CASE_OP_SETTABLE:
    case OP_SETGLOBAL:
    case OP_SETUPVAL:
    case OP_SETUPVAL_R1:
    case OP_SETSLOTN:
    case OP_SETSLOTI:
    case OP_SETSLOT:
    case OP_SETSLOTS:
    case OP_SETSLOTMT:
      return 1;
    default:
      return 0;
  }
}


static int iscallstat(DFuncState *fs, int pc)
{
  OpCode o = GET_OPCODE(fs->f->code[pc]);
  return (IS_OP_CALL(o) && GETARG_C(fs->f->code[pc]) == 1);
}


/*
** true if an operation clobbers a register while using it as an operand
*/
#define continueseval(o,a,b,c) (beginseval(o,a,b,c,0) && !beginseval(o,a,b,c,1))

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


static int clobberslocal(DFuncState *fs, OpCode o, int a, int b, int c)
{
  return (beginseval(o, a, b, c, 0) && a < fs->nactvar);
}


#define KREF_B (1 << 0)
#define KREF_C (1 << 1)
#define KREF_BX (1 << 2)

static int referencesk(OpCode o, int b, int c, int bx)
{
  int res = 0;
  int barg;
  int bmask;
  switch (getOpMode(o)) {
    case iABC:
      bmask = KREF_B;
      barg = b;
      /* see if C is a K index */
      switch (getCMode(o)) {
        case OpArgRK:
          if (!ISK(c)) break;
          /* fallthrough */
        case OpArgK:
          res |= KREF_C;
          break;
        default: break;
      }
      checkargb:
      /* see if B is a K index */
      switch (getBMode(o)) {
        case OpArgRK:
          if (!ISK(barg)) break;
          /* fallthrough */
        case OpArgK:
          res |= bmask;
          break;
        default: break;
      }
      break;
    case iABx:
      bmask = KREF_BX;
      barg = bx;
      goto checkargb;
    default: break;
  }
  return res;
}

static int referencesslot(OpCode o, int a, int b, int c, int slots[3])
{
  int n = 0;
  /* handle store codes */
  if (isstorecode(o))
    slots[n++] = a;
  switch (o) {
    case OP_VARARG:
    case OP_LOADNIL:
      return 0;
    case OP_SETLIST:
      slots[0] = a;
      return 1;
    default: break;
  }
  switch (getBMode(o)) {
    case OpArgRK:
      if (ISK(b)) break;
      /* fallthrough */
    case OpArgR:
      slots[n++] = b;
      break;
    default: break;
  }
  switch (getCMode(o)) {
    case OpArgRK:
      if (ISK(c)) break;
      /* fallthrough */
    case OpArgR:
      slots[n++] = c;
      break;
    default: break;
  }
  return n;
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


static int getjump (DFuncState *fs, int pc)
{
  return pc+1+GETARG_sBx(fs->f->code[pc]);
}


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
** create a do-block node from a do-block state
*/
static BlockNode *createdoblock(DFuncState *fs, int startpc, int endpc)
{
  BlockNode *node;
  lua_assert(ispcvalid(fs, startpc));
  lua_assert(ispcvalid(fs, endpc));
  node = addblnode(fs, startpc, endpc, BL_DO);
  set_ins_property(fs, startpc, INS_DOSTAT);
  set_ins_property(fs, endpc, INS_BLOCKEND);
  return node;
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
      if (test_ins_property(fs, pc, INS_SKIPBOOLLABEL))
        return 0;
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


static void hashtableexpr1(DFuncState *fs, int argC, int firstreg, int pc);


/* 
** common LOADBOOL handler for initial pass
*/
static void onloadbool1(DFuncState *fs, int pc, int c)
{
  if (c) {
    set_ins_property(fs, pc, INS_BOOLLABEL);
    set_ins_property(fs, pc+1, INS_BOOLLABEL);
  }
}

/*
** common jump handler for initial pass
*/
static void onjump1(DFuncState *fs, int pc, int offs)
{
  const Instruction *jc = getjumpcontrol(fs, pc);
  /* check if this jump skips over a pair of bool labels */
  if (jc == NULL && offs >= 2 && test_ins_property(fs, pc+1, INS_BOOLLABEL))
    set_ins_property(fs, pc, INS_SKIPBOOLLABEL);
  if (jc)
    set_ins_property(fs, pc, INS_LEADER);  /* jump instruction is leader */
  /* JMP cannot be the last instruction */
  set_ins_property(fs, pc+1, INS_LEADER); /* next instriction is leader */
  set_ins_property(fs, pc+1+offs, INS_LEADER);  /* jump target is leader */
}


/*
** finds and marks the beginning of an open expression of type KIND; recursing
** into nested open expressions is not necessary, as only the start/end pc of
** root open expressions are needed by the second pass analyzer (VOIDPREP calls
** are treated differently than other calls, however, so recursion does happen
** in that case since VOIDPREP expressions are not recorded)
*/
static void openexpr1(DFuncState *fs, int firstreg, int kind, LocVar *locvar)
{
  const Instruction *code = fs->f->code;
  int endpc = fs->pc;
  int nextpossiblestart = endpc;
  int pc;
  int sharednil = 0;
  /* VOIDPREP calls already have ca->pc at the correct value; other kinds have
     a particular opcode that ends the expression (e.g. OP_RETURN), and that
     opcode has already been visited in those cases */
  if (kind != VOIDPREP) fs->pc--;
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
      endpc++;  /* endpc needs to be (fs->pc+1) in all cases */
  }
  fs->inopenexpr++;
  for (pc = fs->pc; (pc = fs->pc) >= 0; fs->pc--) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    switch (o) {
      case OP_LOADBOOL:
        onloadbool1(fs, pc, c);
        break;
      case OP_JMP:
        onjump1(fs, pc, GETARG_sBx(i));
        break;
      case OP_CALL:
      case OP_CALL_I:
      case OP_CALL_I_R1:
      case OP_CALL_C:
      case OP_CALL_M:
      case OP_CONCAT:
        if (kind == VOIDPREP) {
          openexpr1(fs, a, o == OP_CONCAT ? CONCATPREP : CALLPREP, NULL);
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
          openexpr1(fs, a, SETLISTPREP, NULL);
          postrecursion:
          pc = fs->pc;
          i = code[pc];
          o = GET_OPCODE(i);
          a = GETARG_A(i);
          b = GETARG_B(i);
          c = GETARG_C(i);
        }
        break;
      case OP_NEWTABLE:
        if (kind == VOIDPREP) {
          if (c) {
            /* walk back up the code vector to find the earliest possible end of
               this constructor */
            hashtableexpr1(fs, c, a, pc);
          }
          else
            newopenexpr(fs, a, pc, pc, EMPTYTABLE);
        }
        break;
      CASE_OP_SETTABLE:
        if (kind == VOIDPREP && locvar != NULL) {
          openexpr1(fs, a, HASHTABLEPREP, NULL);
          goto postrecursion;
        }
        break;
      default:
        break;
    }
    if ((kind == SETLISTPREP || kind == HASHTABLEPREP) &&
        o == OP_NEWTABLE && a == firstreg)
      break;  /* found start of table */
    if (beginstempexpr(fs, i, pc, firstreg, endpc, &nextpossiblestart)) {
      break;  /* found the beginning */
    }
    else if (o == OP_LOADNIL && a < firstreg) {
      /* found an OP_LOADNIL that clobbers an earlier register; mark
         NEXTPOSSIBLESTART as the start */
      pc = nextpossiblestart;
      sharednil = 1;
      break;
    }
    if (pc == 0)
      break;
  }
  if (pc < 0) pc = 0;
  if (kind != VOIDPREP) {
    newopenexpr(fs, firstreg, pc, endpc, kind)->sharednil = sharednil;
  }
  fs->pc = pc;
  fs->inopenexpr--;
}


/*
** traverse a local variable initialized expression
*/
static void locvarexpr1(DFuncState *fs, int nvars, struct LocVar *var)
{
  int endpc = fs->pc;
  int reg;  /* register of first local variable that is initialized */
  lua_assert(nvars > 0);
  lua_assert(fs->nactvar >= nvars);
  reg = fs->nactvar-nvars;
  openexpr1(fs, reg, VOIDPREP, var);
  fs->nactvar -= nvars;
  set_ins_property(fs, fs->pc, INS_LOCVAREXPR);
  set_ins_property(fs, fs->pc, INS_ASSIGNSTART);
  set_ins_property(fs, endpc, INS_ASSIGNEND);
}


/*
** traverse a stored open expression
*/
static void storeexpr1(DFuncState *fs, OpCode o, int a, int c)
{
  DecompState *D = fs->D;
  int endpc;
  int reg = a;
  if (D->usedebuginfo == 0)
    return;  /* not using debug info - don't know which registers are local */
  /* get the lowest temporary register used in the operation */
  if (reg < fs->nactvar) {
    if (o == OP_SETSLOTN || !IS_OP_SETTABLE(o) || ISK(c) || c < fs->nactvar)
      return;  /* doesn't use a temporary register */
    else
      reg = c;  /* C is the lowest temporary register */
  }
  lua_assert(isregvalid(fs, reg));
  endpc = --fs->pc;  /* advance over the store code */
  openexpr1(fs, reg, VOIDPREP, NULL);
  set_ins_property(fs, fs->pc, INS_ASSIGNSTART);
  set_ins_property(fs, endpc, INS_ASSIGNEND);
}


/*
** finds the earliest possible endpc for a SETLIST open expression with only
** hash items (the exact number cannot be known if operand C is graeter than 16
** due to the conversion with `luaO_fb2int')
*/
static void hashtableexpr1(DFuncState *fs, int argC, int firstreg, int pc)
{
  const Instruction *code = fs->f->code;
  OpenExpr *e = newopenexpr(fs, firstreg, pc, -1, HASHTABLEPREP);
  int endpc = e->startpc;
  int numhashitems = 0;  /* number of hash items found so far */
  int minhashsize = luaO_fb2int(argC-1)+1;
  int maxhashsize = luaO_fb2int(argC);
  (void)minhashsize;
  for (pc = e->startpc+1; pc < fs->f->sizecode-1; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    if (o == OP_MOVE && b == firstreg) {
      /* found the end, as the constructor is being moved to a local variable */
      break;
    }
    /* check if this opcode sets a table index */
    if (IS_OP_SETTABLE(o)) {
      if (fs->D->usedebuginfo && GET_OPCODE(code[pc-1]) == OP_CLOSURE) {
        const Proto *p = fs->f->p[GETARG_Bx(code[pc-1])];
        if (p->name != NULL)
          break;  /* cannot have a named function inside a constructor */
      }
      if (a == firstreg) {
        numhashitems++;
        endpc = pc;
        if (numhashitems == maxhashsize)
          break;
      }
      else if (a < firstreg)
        break;
    }
    /* if there is any other store code, the constructor has ended */
    else if (isstorecode(o))
      break;
    else if (beginseval(o, a, b, c, 0) && a <= firstreg)
      break;
  }
  if (numhashitems == 0)
    e->kind = EMPTYTABLE;
  e->endpc = endpc;
}



/*
** returns the first local variable that starts at PC; also sets the number of
** active local variables at PC for P_NACTVAR, and sets NVARS to the number of
** variables that start at PC
*/
static struct LocVar *getactvar(DFuncState *fs, int pc, int *nv, lu_byte *na)
{
  int i,n=0;
  lu_byte nact=0;
  LocVar *firstvar = NULL;
  lua_assert(ispcvalid(fs, pc));
  for (i = fs->sizelocvars-1; i >= 0; i--) {
    struct LocVar *var = &fs->locvars[i];
    if (var->startpc <= pc && pc <= var->endpc)
      nact++;
    if (var->startpc == pc) {
      firstvar = var;
      n++;
    }
  }
  if (nv) *nv = n;
  if (na) *na = nact;
  return firstvar;
}


/*
** use this version in `loop1', before local variable info is generated when not
** using loaded debug info; after `gendebug1' runs, you should use `getactvar'
*/
static struct LocVar *getactvar1(DFuncState *fs, int pc, int *nv, lu_byte *na)
{
  if (fs->D->usedebuginfo == 0)
    return NULL;
  return getactvar(fs, pc, nv, na);
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
static int isjumplinefixed(DFuncState *fs, int pc, int target)
{
  lua_assert(ispcvalid(fs, pc));
  lua_assert(ispcvalid(fs, target));
  lua_assert(pc >= target);
  lua_assert(fs->D->usedebuginfo);
  {
    int pcline = getline1(fs, pc);
    int targetline = getline1(fs, target);
    if (pcline > targetline)
      return 0;  /* line was not fixed */
    else if (pcline < targetline)
      return 1;  /* line was fixed */
    else {
      int i;
      for (i = target+1; i < pc; i++) {
        if (getline1(fs, i) > targetline)
          return 1;  /* line was fixed */
      }
      return 0;  /* line may have been fixed but it does not manifest */
    }
  }
}


typedef struct LoopState {
  /* the start and end label of the loop */
  int startlabel, endlabel;
  /* BREAKLABEL and EXITLABEL may vary between each other and from ENDLABEL */
  /* the label that break statements will target */
  int breaklabel;
  /* the exit label for the loop condition */
  int exitlabel;
  unsigned kind : 4;
  unsigned unsure : 1;
  unsigned hasbreak : 2;
} LoopState;


static const LoopState dummyloop = {
  -1, -1, -1, -1, BL_FUNCTION, 0, 0
};


static void setlooplabels(DFuncState *fs, LoopState *loop);


static void setloophasbreak(DFuncState *fs, const LoopState *loop, int pc)
{
  LoopState *loop1;
  if (loop == &dummyloop)
    return;
  loop1 = ((LoopState *)loop);
  if (loop1->unsure == 0)
    loop1->hasbreak = 2;
  else if (loop1->hasbreak == 0)
    loop1->hasbreak = 1;
  if (loop1->hasbreak == 2)
    set_ins_property(fs, pc, INS_BREAKSTAT);
}


static LoopState *pushloopstate1(DFuncState *fs, int kind, int start, int end)
{
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  LoopState *loop;
  growvector(H, D->loopstk, LoopState);
  loop = &D->loopstk.s[D->loopstk.used++];
  loop->kind = kind;
  loop->startlabel = start;
  loop->endlabel = end;
  loop->unsure = 0;
  loop->hasbreak = 0;
  setlooplabels(fs, loop);
  return loop;
}

#define poploopstate1(fs)  popnloopstate1(fs, 1)
static LoopState *popnloopstate1(DFuncState *fs, int n)
{
  DecompState *D = fs->D;
  lua_assert(D->loopstk.used >= n);
  return &D->loopstk.s[(D->loopstk.used -= n)];
}


#define getcurrloop1(fs)  gettoploop1(fs, 1)

static const LoopState *gettoploop1(DFuncState *fs, int n)
{
  DecompState *D = fs->D;
  lua_assert(n > 0);
  if (D->loopstk.used < n)
    return &dummyloop;
  return &D->loopstk.s[D->loopstk.used-n];
}


static BlockNode *finalizeloopstate1(DFuncState *fs, BlockNode *nextnode)
{
  const LoopState *loop = getcurrloop1(fs);
  int kind = loop->kind;
  int startpc = loop->startlabel;
  int endpc = loop->endlabel-1;
  BlockNode *new_node;
  int skip = (loop->unsure && loop->hasbreak < 2);
  poploopstate1(fs);
  if (skip)
    return nextnode;
  new_node = addblnode(fs, startpc, endpc, kind);
  if (nextnode != NULL && nextnode->endpc <= endpc) {
    BlockNode *temp;
    new_node->firstchild = nextnode;
    /* find the first child that is outside of this new node and make it the
       next sibling */
    while (nextnode->nextsibling && nextnode->nextsibling->endpc <= endpc)
      nextnode = nextnode->nextsibling;
    temp = nextnode->nextsibling;
    nextnode->nextsibling = NULL;
    nextnode = temp;
  }
  new_node->nextsibling = nextnode;
  switch (kind) {
    case BL_WHILE: set_ins_property(fs, startpc, INS_WHILESTAT); break;
    case BL_REPEAT: set_ins_property(fs, startpc, INS_REPEATSTAT); break;
    case BL_FORNUM: set_ins_property(fs, startpc, INS_FORNUM); break;
    case BL_FORLIST: set_ins_property(fs, startpc, INS_FORLIST); break;
  }
  set_ins_property(fs, endpc, INS_LOOPEND);
  return new_node;
}


/*
** returns non-NULL if TARGET is any of the exit-labels of LOOP, returning the
** pointer to the first label that matches TARGET
*/
static const int *isloopexit(const LoopState *loop, int target)
{
  if (target == loop->exitlabel)
    return &loop->exitlabel;
  else if (target == loop->breaklabel)
    return &loop->breaklabel;
  else if (target == loop->endlabel)
    return &loop->endlabel;
  return NULL;
}


static int isloopbreak(const LoopState *loop, int target)
{
  const int *label = isloopexit(loop, target);
  return (label && *label == loop->breaklabel);
}


static void
calcloopunsure(DFuncState *fs, LoopState *loop, const LoopState *outerloop)
{
  int target = getjump(fs, loop->endlabel-1);
  if (target == outerloop->startlabel) {
    if (outerloop->kind == BL_WHILE) {
      if (fs->D->usedebuginfo)
        loop->unsure = !isjumplinefixed(fs, loop->endlabel-1, target);
      else
        loop->unsure = 1;
    }
    else if (outerloop->kind == BL_REPEAT &&
             getjumpcontrol(fs, outerloop->endlabel-1) == NULL)
      loop->unsure = 1;
  }
}


static void jump1(DFuncState *fs, int pc, int offs)
{
  DecompState *D = fs->D;
  const LoopState *currloop = getcurrloop1(fs);
  int loopkind = -1;
  int target = pc + 1 + offs; /* the jump target pc */
  const Instruction *jc = getjumpcontrol(fs, pc);
  int tested = (jc != NULL);  /* is it a tested jump */
  /* check if this jump interacts with bool labels */
  if (test_ins_property(fs, target, INS_BOOLLABEL) ||
      test_ins_property(fs, pc, INS_SKIPBOOLLABEL))
    return; /* this jump is part of a boolean expression */
  if (offs < 0 && tested) {
    if (GET_OPCODE(*jc) == OP_TFORLOOP)
      loopkind = BL_FORLIST;
    else {  /* possibly a repeat-loop jump */
      if (target > currloop->startlabel) {
        const int *label = isloopexit(currloop, target);
        if (label == NULL || *label != currloop->exitlabel)
          loopkind = BL_REPEAT;
      }
    }
  }
  else {
    if (!tested && isloopbreak(currloop, target)) {
      setloophasbreak(fs, currloop, pc);
      return;
    }
    if (offs < 0) {
      /* the final jump in a while-loop will have its line fixed to the start
         line of the loop; if the line for this jump is greater than the start
         line, this cannot be a while-loop */
      if (D->usedebuginfo && getline1(fs, pc) > getline1(fs, target))
        loopkind = BL_REPEAT;
      else
        loopkind = BL_WHILE;
    }
    else if (currloop->unsure && !isloopexit(currloop, target)) {
      const LoopState *loop;
      int i;
      /* check if this jump breaks from an outer loop and not the current one */
      for (i = 2; (loop = gettoploop1(fs, i))->kind != BL_FUNCTION; i++) {
        if (isloopexit(loop, target)) {
          popnloopstate1(fs, i-1);
          jump1(fs, pc, offs);
          return;
        }
      }
    }
    /* check is there is a break out of an outer repeat-loop; in that case, the
       outer loop and current loop are one loop */
    else if (!tested &&
             currloop->kind == BL_REPEAT && !isloopbreak(currloop, target)) {
      const LoopState *loop;
      int i;
      for (i = 2; (loop = gettoploop1(fs, i))->kind == BL_REPEAT; i++) {
        if (loop->startlabel != currloop->startlabel)
          break;
        if (isloopbreak(loop, target)) {
          popnloopstate1(fs, i-1);
          jump1(fs, pc, offs);
          return;
        }
      }
    }
  }
  if (loopkind != -1) {
    pushloopstate1(fs, loopkind, target, pc+1);
    calcloopunsure(fs, (LoopState*)getcurrloop1(fs), gettoploop1(fs, 2));
  }
}


static void setlooplabels(DFuncState *fs, LoopState *loop)
{
  int endlabel = loop->endlabel;
  lua_assert(ispcvalid(fs, endlabel));
  loop->breaklabel = loop->exitlabel = endlabel;
  if (GET_OPCODE(fs->f->code[endlabel]) == OP_JMP) {
    /* breaks will jump to the label targeted by the jump at ENDLABEL */
    int breaklabel = getjump(fs, endlabel);
    loop->breaklabel = breaklabel;
    loop->exitlabel = breaklabel;
    /* if the break label is a bool label, and the bool labels have a jump
       before them to skip the labels, exits from the loop condition will
       target the end of the bool labels, that is, 1 pc after the second bool
       label, not the jump target of the jump which skips them */
    if (test_ins_property(fs, breaklabel, INS_BOOLLABEL)) {
      int prevpc = breaklabel - 1 - !GETARG_C(fs->f->code[breaklabel]);
      if (test_ins_property(fs, prevpc, INS_SKIPBOOLLABEL))
        loop->exitlabel = prevpc + 3;
    }
  }
}


/*
** record open expressions, loops, and testset expressions
*/
static void simloop1(DFuncState *fs)
{
  const Instruction *code = fs->f->code;
  BlockNode *nextnode = NULL;
  int pendingbreak = -1;
  /*int n, r1, r2, r3;*/
  /* walk through code backwards */
  fs->pc = fs->f->sizecode-1;
  for (fs->pc--; fs->pc >= 0; fs->pc--) {
    int pc = fs->pc;
    OpCode o = GET_OPCODE(code[pc]);
    int a = GETARG_A(code[pc]);
    int b = GETARG_B(code[pc]);
    int c = GETARG_C(code[pc]);
    int bx = GETARG_Bx(code[pc]);
    int sbx = GETARG_sBx(code[pc]);
    /* get the first local variable that starts on the next pc */
    int nvars;
    LocVar *nextvar = updateactvar1(fs, pc+1, &nvars);
    if (nextvar) {
      if (o != OP_FORPREP && !test_ins_property(fs, pc+1, INS_FORLIST))
        locvarexpr1(fs, nvars, nextvar);
    }
    switch (o) {
      case OP_JMP: {
        const Instruction *jc = getjumpcontrol(fs, pc);
        onjump1(fs, pc, sbx);
        if (test_ins_property(fs, pc+1, INS_FORLIST)) {
          updateactvar1(fs, pc, &nvars);
          openexpr1(fs, GETARG_A(code[pc+1+sbx]), FORLISTPREP, NULL);
        }
        else if (jc == NULL || GET_OPCODE(*jc) != OP_TESTSET)
          jump1(fs, pc, sbx);
        break;
      }
      case OP_FORLOOP: { /* a numeric for-loop */
        int target = pc + 1 + sbx; /* start of for-loop */
        pushloopstate1(fs, BL_FORNUM, target, pc+1);
        set_ins_property(fs, pc+1, INS_LEADER);
        set_ins_property(fs, target, INS_LEADER);
        break;
      }
      case OP_FORPREP: { /* mark the start of preparation code */
        int target = pc + 1 + sbx; /* start of for-loop */
        updateactvar1(fs, pc, &nvars);
        openexpr1(fs, a, FORNUMPREP, NULL);
        set_ins_property(fs, pc+1, INS_LEADER);
        set_ins_property(fs, target, INS_LEADER);
        break;
      }
      /* mark open expressions */
      CASE_OP_CALL:
        openexpr1(fs, a, CALLPREP, NULL);
        break;
      case OP_CONCAT:
        openexpr1(fs, b, CONCATPREP, NULL);
        break;
      case OP_RETURN: {
        int nret = b-1;
        if (nret == 0)
          break;
        /* return statements with more than 1 value always push the returned
           values to the stack; if using debug info, a single-return can also be
           differentiated between an open expression and a local variable */
        if (nret != 1 || (fs->D->usedebuginfo && a >= fs->nactvar))
          openexpr1(fs, a, RETPREP, NULL);
        break;
      }
      case OP_SETLIST:
        openexpr1(fs, a, SETLISTPREP, NULL);
        break;
      case OP_NEWTABLE:
        if (c)
          hashtableexpr1(fs, c, a, pc);
        else
          /* array-only tables are already handled with OP_SETLIST */
          newopenexpr(fs, a, pc, pc, EMPTYTABLE);
        break;
      /* mark bool labels */
      case OP_LOADBOOL:
        onloadbool1(fs, pc, c);
        break;
      /* mark nil labels */
      case OP_LOADNIL:
        /* LOADNIL cannot be the last instruction */
        if (GET_OPCODE(code[pc+1]) == OP_LOADNIL) {
          int nextA = GETARG_A(code[pc+1]);
          /* if these 2 nil codes could have been optimized, the next pc must
             have been a possible leader when the code was generated; but it may
             not actually have been rendered a leader if the basic block
             boundary was optimized away, for example, in an if-true block:
                local a = nil;
                if true then -- falls through with no jump emitted in bytecode
                    local b = nil;
                end */
          if (nextA >= a && nextA <= b+1)
            set_ins_property(fs, pc+1, INS_NILLABEL);
        }
        break;
      /* note which registers a closure uses as upvalues and if it uses its own
         register as an upvalue */
      case OP_CLOSURE: {
        int nup = fs->f->p[bx]->nups;
        int nupn;
        for (nupn = nup; nupn > 0; nupn--)
          if (GETARG_A(code[pc+nupn]) == 1 && GETARG_Bx(code[pc+nupn]) == a)
            set_ins_property(fs, pc, INS_SELFUPVAL);
        break;
      }
      default: {
        if (isstorecode(o)) {
          storeexpr1(fs, o, a, c);
        }
      }
    }
    /* update vars after possibly traversing extra codes */
    pc = fs->pc;
    o = GET_OPCODE(code[pc]);
    /*n = referencesslot(code[fs->pc], &r1, &r2, &r3);*/
    if (getcurrloop1(fs)->hasbreak == 1) {
      if (pendingbreak == -1)
        pendingbreak = pc;
      if (o != OP_JMP && testTMode(o) == 0) {
        ((LoopState *)getcurrloop1(fs))->hasbreak = 2;
        set_ins_property(fs, pendingbreak, INS_BREAKSTAT);
        pendingbreak = -1;
      }
    }
    while (pc == getcurrloop1(fs)->startlabel)
      nextnode = finalizeloopstate1(fs, nextnode);
  }
  fs->root = addblnode(fs, 0, fs->f->sizecode-1, BL_FUNCTION);
  fs->root->firstchild = nextnode;
  fs->D->loopstk.used = 0;
  set_ins_property(fs, 0, INS_LEADER);
}

static const OpenExpr dummyexpr = {-1, -1, VOIDPREP, -1, 0};


static void updatenextopenexpr0(DecompState *D)
{
  DFuncState *fs = D->fs;
  lua_assert(fs->nopencalls >= 0);
  if (fs->nopencalls < fs->a->sizeopencalls)
    D->a.openexpr = &fs->a->opencalls[fs->nopencalls++];
  else
    D->a.openexpr = &dummyexpr;
}

static void rescanloops(DFuncState *fs)
{
  DecompState *D = fs->D;
  const LoopState *currloop = getcurrloop1(fs);
  const Instruction *code = fs->f->code;
  int pc;
  fs->nopencalls = 0;
  updatenextopenexpr0(D);
  /* walk through code backwards */
  for (pc = fs->f->sizecode-1; pc >= 0; pc--) {
    OpCode o = GET_OPCODE(code[pc]);
    int sbx = GETARG_sBx(code[pc]);
    int target = pc+1+sbx;
    const Instruction *jc;
    /* only 1 loop can end per instruction; */
    if (test_ins_property(fs, pc, INS_LOOPEND)) {
      /* loop kind doesn't matter, just record the start and end labels */
      currloop = pushloopstate1(fs, BL_WHILE, target, pc+1);
      continue;
    }
    /* check if entering an open expression */
    if (pc == D->a.openexpr->endpc) {
      /* skip to the start of the expression, avoiding any jumps within it */
      pc = D->a.openexpr->startpc;
      /* update next open expression */
      updatenextopenexpr0(D);
      goto l1;
    }
    jc = getjumpcontrol(fs, pc);
    if (o != OP_JMP ||
        test_ins_property(fs, pc, INS_BREAKSTAT) ||
        test_ins_property(fs, target, INS_BOOLLABEL) ||
        test_ins_property(fs, pc, INS_SKIPBOOLLABEL) ||
        (jc && GET_OPCODE(*jc) == OP_TESTSET))
      goto l1;
    /* set properties for this jump instruction */
    if (sbx < 0 &&
        (target == currloop->startlabel || isloopexit(currloop, target)))
      set_ins_property(fs, pc, INS_FAILJUMP);
    else if (sbx >= 0) {
      if (target > pc+2 && GET_OPCODE(fs->f->code[target-1]) == OP_JMP) {
        const Instruction *jc2 = getjumpcontrol(fs, target-1);
        if (jc2 && GET_OPCODE(*jc2) == OP_TESTSET) {
          set_ins_property(fs, pc, INS_TESTSETJUMP);
          goto l1;
        }
      }
      if (test_ins_property(fs, target-1, INS_FAILJUMP)) {
        if (getjumpcontrol(fs, target-1) != NULL)
          set_ins_property(fs, pc, INS_PASSJUMP);
        else {
          /* TODO: mark else-branch point if needed at (target-1) */
          unset_ins_property(fs, target-1, INS_FAILJUMP);
          set_ins_property(fs, pc, INS_FAILJUMP);
        }
      }
      else if (test_ins_property(fs, target-1, INS_PASSJUMP))
        set_ins_property(fs, pc, INS_FAILJUMP);
      else
        set_ins_property(fs, pc, INS_FAILJUMP);
    }
    l1:
    /* pop loop states which begin here */
    while (pc == currloop->startlabel) {
      poploopstate1(fs);
      currloop = getcurrloop1(fs);
    }
  }
#ifdef LUA_DEBUG
  for (pc = 0; pc < fs->f->sizecode-1; pc++)
    lua_assert(!(test_ins_property(fs, pc, INS_FAILJUMP) &&
                 test_ins_property(fs, pc, INS_PASSJUMP)));
#endif /* LUA_DEBUG */
  fs->D->loopstk.used = 0;
  fs->nopencalls = fs->a->sizeopencalls;
}


/******************************************************************************/
/* Block Analyzer - generates branches/blocks and variables if needed */
/******************************************************************************/


static int getevalstart(DFuncState *fs, int pc, int firstreg)
{
  const Instruction *code = fs->f->code;
  int nextpossiblestart = pc;
  for (; pc >= 0; pc--) {
    Instruction i = code[pc];
    if (beginstempexpr(fs, i, pc, firstreg, pc+1, &nextpossiblestart)) {
      break;  /* found the beginning */
    }
    else if (GET_OPCODE(i) == OP_LOADNIL && GETARG_A(i) < firstreg) {
      /* found an OP_LOADNIL that clobbers an earlier register; mark
         NEXTPOSSIBLESTART as the start */
      pc = nextpossiblestart;
      break;
    }
  }
  return (pc < 0 ? 0 : pc);
}


typedef struct BlockState {
  BlockNode *node;  /* corresponding node */
  union {
    const LoopState *loop;  /* enclosing loop */
    /* for converting pointer to index when growing the loop stack after the
       intiail pass */
    int loopindex;
  } l;
  /* the true-exit label for an if-statement */
  int t_exitlabel;
  /* the jump label for a false branch-condition */
  int f_exitlabel;
  int exitlabel;
  int lastnecessaryvar;
  int highestclobbered;
  int highestclobberedresetpc;
  int savedlastup;
  int prepstart;
  /* - the highest register clobbered since the last open expression or block
     start, not counting clobbers within open expressions themselves
     HIGHESTCLOBBEREd would tell you which variables need to be duplicated in
     the case where the variable would need to end before the open expression,
     but it started in the parent block. So the variable must end in the parent
     block before entering the child block, then a new variable begins and ends
     in the child block, achieved with a do-statement wrapped around the
     variable.
     For example, consider the following Lua code:
      do
          local _ = 12; -- LOADK 0
          _ = 34; -- LOADK 0
          a = _; -- SETGLOBAL
      end -- variable ends, first temporary is reset to 0
      repeat
          do
              local _ = 12; -- LOADK 0
          end -- variable ends, first temporary is reset to 0
          a(1, 2); -- an open expression starting at 0
      until a;
     In the above case, the decompiler will encounter the first LOADK code
     clobbering register 0, and create a local variable. It encounters the
     second LOADK and SETGLOBAL after, then it enters the repeat-loop. It is now
     in a child block. The `highestclobbered' field for the parent block (the
     main function in this case) will be 0, because that is the highest register
     that had a value written to it. In the repeat-loop, LOADK is encountered,
     and it is treated as an assignment to an existing local variabe, because
     slot 0 accesses the variable `_' as far as the decompiler is aware. But
     then it encounters the open expression given by `a(1, 2)', which uses slot
     0 as a temporary register, which means the variable `_' must be ended early
     via a do-block. 
     */
  lu_byte nactvar;  /* # active locals outside the breakable structure */
  lu_byte nforloopvars;  /* # for-loop variables */
  unsigned seenstat : 1;
  /* a hard statement is a return, break, store outside of a table constructor,
     etc., things that could never be confused with a subexpression; clobbering
     a register is not a hard statement unless debug info shows that the
     register is a local variable, but when not using debug info, the clobber is
     marked as a statement but not a hard statement */
  unsigned seenhardstat : 1;
  unsigned isloop : 1;
  unsigned isbranch : 1;
  unsigned statbeforechild : 1;
} BlockState;


static void setbranchlabels(DFuncState *fs, BlockState *block)
{
  int endpc = block->node->endpc;
  int exitlabel = endpc+1;
  lua_assert(ispcvalid(fs, exitlabel));
  if (GET_OPCODE(fs->f->code[exitlabel]) == OP_JMP) {
    exitlabel = getjump(fs, exitlabel);
    /* if the break label is a bool label, and the bool labels have a jump
       before them to skip the labels, exits from the loop condition will
       target the end of the bool labels, that is, 1 pc after the second bool
       label, not the jump target of the jump which skips them */
    if (test_ins_property(fs, exitlabel, INS_BOOLLABEL)) {
      int prevpc = exitlabel - 1 - !GETARG_C(fs->f->code[exitlabel]);
      if (test_ins_property(fs, prevpc, INS_SKIPBOOLLABEL))
        exitlabel = prevpc + 3;
    }
  }
  /* false exit targets the else-part */
  block->f_exitlabel = exitlabel;
  if (GET_OPCODE(fs->f->code[endpc]) == OP_JMP &&
      !test_ins_property(fs, endpc, INS_BREAKSTAT)) {
    /* true exit is the end of the statement */
    block->t_exitlabel = getjump(fs, endpc);
  }
  else {
    /* no else-part */
    block->t_exitlabel = block->f_exitlabel;
  }
}

static int getlastpersistentvar1(DecompState *D, int limit);


static void initblockstate1(DFuncState *fs, BlockState *block, BlockNode *node)
{
  DecompState *D = fs->D;
  block->node = node;
  block->nactvar = fs->nactvar;
  block->seenstat = 0;
  block->seenhardstat = 0;
  block->statbeforechild = 0;
  block->highestclobbered = -1;
  block->highestclobberedresetpc = node->startpc;
  block->savedlastup = -1;
  block->lastnecessaryvar = -1;
  if (D->blockstk.used > 1) {
    BlockState *prev = block-1;
    /* parent block has seen a statement */
    prev->seenstat = 1;
    prev->seenhardstat = 1;
    prev->lastnecessaryvar = getlastpersistentvar1(D, prev->nactvar);
  }
  if (isloopnode(node)) {
    pushloopstate1(fs, node->kind, node->startpc, node->endpc+1);
    block->isloop = 1;
    block->isbranch = 0;
    block->t_exitlabel = block->f_exitlabel = -1;
  }
  else if (node->kind == BL_IF) {
    block->isbranch = 1;
    block->isloop = 0;
    setbranchlabels(fs, block);
  }
  else
    block->isbranch = block->isloop = 0;
  block->l.loop = getcurrloop1(fs);
  block->nforloopvars = 0;
}


static BlockState *pushblockstate1(DFuncState *fs, BlockNode *node)
{
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  BlockState *block;
  lua_assert(node != NULL);
  growvector(H, D->blockstk, BlockState);
  block = &D->blockstk.s[D->blockstk.used++];
  initblockstate1(fs, block, node);
  return block;
}


/*
** insert a new loop into the middle of the block state stack
*/
static BlockState *insertloopstate1(DFuncState *fs, BlockNode *node)
{
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  BlockState *block;
  const LoopState *base = D->loopstk.s;
  int i;
  /* if the loop stack needs to grow, the pointers may be invalidated */
  if (D->loopstk.used >= D->loopstk.alloc) {
    /* covert pointers to indices in BlockStates before growing the stack */
    for (i = 0; i < D->blockstk.used; i++) {
      BlockState *block = &D->blockstk.s[i];
      if (block->l.loop == &dummyloop)
        block->l.loopindex = -1;
      else
        block->l.loopindex = block->l.loop - base;
    }
    growvector(H, D->loopstk, LoopState);
    base = D->loopstk.s;
    /* convert indices back to pointers */
    for (i = 0; i < D->blockstk.used; i++) {
      BlockState *block = &D->blockstk.s[i];
      if (block->l.loopindex == -1)
        block->l.loop = &dummyloop;
      else
        block->l.loop = base + block->l.loopindex;
    }
  }
  growvector(H, D->blockstk, BlockState);
  /* make room to insert a block state */
  for (i = D->blockstk.used; i > 0; i--) {
    BlockState *bl = &D->blockstk.s[i-1];
    if (blstartpc(bl->node) < node->startpc)
      break;
    D->blockstk.s[i] = D->blockstk.s[i-1];
  }
  D->a.bl = &D->blockstk.s[D->blockstk.used++];
  block = &D->blockstk.s[i];
  initblockstate1(fs, block, node);
  return block;
}


static void resethighestclob1(DecompState *D, int pc)
{
  D->a.bl->highestclobbered = -1;
  D->a.bl->highestclobberedresetpc = pc;
  D->a.bl->savedlastup = D->a.lastup;
}


static BlockState *popblockstate1(DFuncState *fs)
{
  DecompState *D = fs->D;
  BlockState *block;
  lua_assert(D->blockstk.used > 0);
  block = &D->blockstk.s[--D->blockstk.used];
  fs->nactvar = block->nactvar;
  if (isloopnode(block->node)) {
    if (isforloop(block->node))
      fs->nactvar -= 3;
    poploopstate1(fs);
  }
  return block;
}


#define getcurrblock1(fs)  gettopblock1(fs, 1)

static BlockState *gettopblock1(DFuncState *fs, int n)
{
  DecompState *D = fs->D;
  lua_assert(n > 0);
  if (D->blockstk.used < n)
    return NULL;
  return &D->blockstk.s[D->blockstk.used-n];
}


static void updateinsn1(DecompState *D, DFuncState *fs)
{
  Instruction i;
  lua_assert(ispcvalid(fs, fs->pc));
  i = fs->f->code[fs->pc];
  D->a.insn.o = GET_OPCODE(i);
  D->a.insn.a = GETARG_A(i);
  D->a.insn.b = GETARG_B(i);
  D->a.insn.c = GETARG_C(i);
  D->a.insn.bx = GETARG_Bx(i);
  D->a.insn.sbx = GETARG_sBx(i);
}


#define ispersistent(note) ((note) != GENVAR_DISCHARGED)

/*
** Variable generation notes
** These notes describe the level of necessity of a variable's creation at a
** particular pc
**
** The different states of a created variable
** - clobbered
** - discharged
** - referenced, at this point the variable must be committed
**
** if you clobber a register than reference it, it does not need to be a
** variable, but if it is referenced without being clobbered immediately before,
** it must be a variable
**
** a stack value becomes discharged if it is clobbered without reading it, or
** a lower slot is clobbered without reading it; if another value is pushed, the
** slot is still pending discharge
*/
enum GENVARNOTE {
  /* FILL is used when there is stack space between the last variable and the
     next variable to create; the actual note for this variable defers to the
     next non-FILL variable */
  GENVAR_FILL,
  /* value is on the stack, yet to be used; if a variable ends up with this note
     by the end of the analysis, it must not be pruned */
  GENVAR_ONSTACK,
  /* value on the stack was used immediately; if a variable ends up with this
     note by the end of the analysis, it may be pruned */
  GENVAR_DISCHARGED,
  /* the below notes are for variables which must not be pruned from the
     generated variable info */
  GENVAR_PERSISTENT,  /* this variable must exist */
  GENVAR_UPVALUE,  /* same as PERSISTENT, but also an upvalue */
  /* the below notes indicate not to modify the startpc that was given to the
     variable when it was created (in updatevarstartpcs1) */
  GENVAR_FORLOOPVAR,  /* user variables in a for-loop */
  GENVAR_INITIAL,  /* initial nil variables (only in regular Lua) */
  /* the rest of the notes are used to determine how to name the variabless;
     PARAM variables will be named following the pattern "f%d_par%d" */
  GENVAR_PARAM,  /* function parameters */
  GENVAR_FORNUM,  /* augmented numeric for-loop variables */
  GENVAR_FORLIST  /* augmented list for-loop variables */
};

#ifdef LUA_DEBUG
static const char *const varnotenames[] = {
  "GENVAR_FILL", "GENVAR_ONSTACK", "GENVAR_DISCHARGED", "GENVAR_PERSISTENT",
  "GENVAR_UPVALUE", "GENVAR_FORLOOPVAR", "GENVAR_INITIAL", "GENVAR_PARAM",
  "GENVAR_FORNUM", "GENVAR_FORLIST"
};
#endif /* LUA_DEBUG */


typedef struct GenLocVar {
  union {
    enum GENVARNOTE varnote;
    TString *varname;
  } u;
  int startpc;  /* first point where variable is active */
  int endpc;    /* first point where variable is dead */
} GenLocVar;


/* check if GenLocVar will be compatible with LocVar, and in case it is not, a
   separate vector will be used for variable notes */
static const int mem_separate_varnotes = sizeof(GenLocVar) != sizeof(LocVar);


static LocVar *getlocvar1(DecompState *D, int r)
{
  return &D->fs->locvars[D->fs->a->actvar[r]];
}


static LocVar *getlastlocvar1(DecompState *D)
{
  lua_assert(D->fs->nactvar > 0);
  return getlocvar1(D, D->fs->nactvar-1);
}


/*
** I store the NOTE in the VARNAME field, as the notes are only before variable
** names are generated
*/
static enum GENVARNOTE getvarnote1(DecompState *D, struct LocVar *var)
{
  if (mem_separate_varnotes) {
    int index = var - D->fs->locvars;
    lua_assert(index >= 0 && index < D->varnotes.used);
    return cast(enum GENVARNOTE, D->varnotes.s[index]);
  }
  else {
    GenLocVar *genvar = cast(void *, var);
    return genvar->u.varnote;
  }
}


static void setvarnote1(DecompState *D, int r, enum GENVARNOTE note)
{
  if (mem_separate_varnotes) {
    int index = D->fs->a->actvar[r];
    lua_assert(index < D->varnotes.used);
    D->varnotes.s[index] = cast_byte(note);
  }
  else {
    GenLocVar *var = cast(void *, getlocvar1(D, r));
    var->u.varnote = note;
  }
}


static void promotevar1(DecompState *D, int r, enum GENVARNOTE newnote)
{
  LocVar *var = getlocvar1(D, r);
  enum GENVARNOTE note = getvarnote1(D, var);
  if (note < newnote)
    setvarnote1(D, r, newnote);
}


/*
** data for the pending store chain when generating variables
*/
struct pendingstorechain1 {
  int pc;  /* current pc */
  int startpc;  /* pc of first store code */
  int firstreg;  /* first reg used to evaluate RHS */
  int lastreg;  /* last reg used to evaluate RHS */
  int laststat;  /* pc of previous statement if any */
  int lastup;  /* highest referenced upvalue before the store chain */
  int upval;  /* true if there is an OP_SETUPVAL code in the store chain */
};


static void updatenextopenexpr1(DecompState *D)
{
  DFuncState *fs = D->fs;
  lua_assert(fs->nopencalls >= 0);
  if (fs->nopencalls == 0)
    D->a.openexpr = &dummyexpr;
  else
    D->a.openexpr = &fs->a->opencalls[--fs->nopencalls];
}


/*
** record a local variable starting at PC with the given NOTE
*/
static LocVar *genvar1(DecompState *D, int pc, enum GENVARNOTE note)
{
  DFuncState *fs = D->fs;
  Analyzer *a = fs->a;
  struct LocVar *var;
  /* never generate a variable if using debug info */
  lua_assert(D->usedebuginfo == 0);
  luaM_growvector(fs->H, a->locvars, fs->nlocvars, a->sizelocvars, LocVar,
                  SHRT_MAX, "");
  fs->locvars = a->locvars;
  fs->sizelocvars = a->sizelocvars;
  var = &a->locvars[fs->nlocvars];
  var->startpc = pc;
  var->endpc = getnaturalvarendpc(D->a.bl->node);
  if (mem_separate_varnotes) {
    D->varnotes.used = fs->nlocvars;
    growvector(fs->H, D->varnotes, lu_byte);
    D->varnotes.s[D->varnotes.used] = cast_byte(note);
    var->varname = NULL;
  }
  else {
    GenLocVar *genvar = cast(void *, var);
    genvar->u.varnote = note;
  }
  a->actvar[fs->nactvar] = fs->nlocvars++;
  fs->nactvar++;
  return var;
}


/*
** use this when you want the next instruction that is not OP_DATA; for example,
** variables that are initialized by OP_GETTABLE will start after the 2 OP_DATA
** codes, and variables initialized by OP_GETGLOBAL_MEM will start after the
** OP_DATA code
*/
static int getnextpc1(DFuncState *fs, int pc)
{
  int nextpc = pc;
  do {
    nextpc++;
    if (nextpc >= fs->f->sizecode) break;
  } while (GET_OPCODE(fs->f->code[nextpc]) == OP_DATA);
  return nextpc;
}


/*
** add a new local variable in reg R, where PC is the current pc
*/
static void newlocalvar1(DecompState *D, int r, int pc)
{
  /* initially I have the startpc as the first evluation code so that I know
     where the local statement would start. Then later I will update them to
     their actual startpc */
  int startpc = pc /*getnextpc1(D->fs, pc)*/;
  int i;
  if (D->a.insn.o == OP_LOADNIL) {
    /* when an OP_LOADNIL is shared by a local variable statement and an open
       expression, such as `local a, b, c; (nil)(1, 2);', variables should not
       be created for the extra nil slots used by the open expression */
    if (D->a.openexpr->startpc == pc+1 && D->a.openexpr->sharednil) {
      int firsttemp = D->a.openexpr->firstreg;
      /* if rescanning, the next open expression should never start after PC */
      lua_assert(D->rescan == 0);
      if (r >= firsttemp)
        return;
    }
  }
  for (i = D->fs->nactvar; i < r; i++)
    genvar1(D, startpc, GENVAR_FILL);
  /* if the position below R is free, then it must be a local if it is not being
     used in favor of R */
  if (r > 0 && getvarnote1(D, getlocvar1(D, r-1)) == GENVAR_DISCHARGED)
    setvarnote1(D, r-1, GENVAR_PERSISTENT);
  genvar1(D, startpc, GENVAR_ONSTACK);
  (void)getnextpc1;
}


/*
** revert fs->nactvar back to N, erasing any active variables that have been
** created from that point
*/
static void rollbackvars1(DecompState *D, lu_byte n)
{
  DFuncState *fs = D->fs;
  int numtodelete = (lua_assert(fs->nactvar >= n), fs->nactvar - n);
  if (numtodelete == 0)
    return;
  fs->nlocvars -= numtodelete;
  fs->nactvar = n;
}


/*
** update the last statement-ending pc to PC
*/
static void setlaststat1(DecompState *D, int pc, int hardstat)
{
  D->a.laststat = pc;
  D->a.bl->seenstat = 1;
  if (hardstat) D->a.bl->seenhardstat = 1;
}


/*
** return true if the pending assignments can be grouped as one statement
*/
static int storeischainable1(DecompState *D, struct pendingstorechain1 *store)
{
  return (D->a.skippedstorerefpc >= store->startpc ||
          (store->upval && D->a.lastup == store->lastup));
}


static void dischargestores1(DecompState *D)
{
  DFuncState *fs = D->fs;
  struct pendingstorechain1 *store = D->a.store;
  lua_assert(store->pc != -1);
  lua_assert(store->startpc != -1);
  lua_assert(store->lastreg != -1);
  lua_assert(store->firstreg != -1);
  /* if the store chain lasts more than 1 pc and it is not chainable, make sure
     any registers used as RHS values are made local variables */
  if (store->startpc != store->pc && !storeischainable1(D, store)) {
    int i;
    for (i = store->firstreg; i < fs->nactvar; i++) {
      LocVar *var = getlocvar1(D, i);
      enum GENVARNOTE note = getvarnote1(D, var);
      if (note < GENVAR_DISCHARGED)
        setvarnote1(D, i, GENVAR_DISCHARGED);
    }
  }
  /* if the store is chainable, make sure the registers used to evaluate RHS are
     temporary */
  else if (store->startpc != store->pc &&
           fs->nactvar && fs->nactvar-1 == store->lastreg) {
    int i, numtodelete = 0;
    for (i = fs->nactvar-1; i >= store->firstreg; i--, numtodelete++) {
      LocVar *var = getlocvar1(D, i);
      enum GENVARNOTE note = getvarnote1(D, var);
      if ((var->startpc < store->startpc && var->startpc <= store->laststat) ||
          ispersistent(note)) {
        break;
      }
    }
    if (numtodelete) {
      fs->nlocvars -= numtodelete;
      fs->nactvar -= numtodelete;
    }
  }
  /* clear pending data */
  store->pc = store->startpc = store->firstreg = store->lastreg = -1;
  store->laststat = store->lastup = -1;
  store->upval = 0;
}


/*
** returns the number of register operands in a store instruction, and populates
** the array OPERANDS with the operand slot numbers
*/
static int getstoreoperands(OpCode o, int a, int b, int c, int operands[3])
{
  int n = 1;  /* number of register operands, between 1 and 3 */
  operands[0] = a;
  if (IS_OP_SETTABLE(o)) {
    if (o != OP_SETFIELD && o != OP_SETFIELD_R1 && !ISK(b))
      operands[n++] = b;
    if (!ISK(c))
      operands[n++] = c;
  }
  else if (o == OP_SETSLOTI || o == OP_SETSLOT || o == OP_SETSLOTS ||
           o == OP_SETSLOTMT) {
    if (!ISK(c))
      operands[n++] = c;
  }
  return n;
}


/*
** apply updates to variables depending on which registers are referenced in a
** store instruction given by O, A, B, and C
*/
static void checkstoreoperands(DecompState *D, OpCode o, int a, int b, int c)
{
  int i, j;
  int operands[3];
  int noperands = getstoreoperands(o, a, b, c, operands);
  DFuncState *fs = D->fs;
  for (i = noperands-1, j = 1; i >= 0; i--) {
    LocVar *var = getlocvar1(D, operands[i]);
    enum GENVARNOTE note = getvarnote1(D, var);
    if (operands[i] < fs->nactvar - j) {
      LocVar *lastvar = getlastlocvar1(D);
      int k;
      for (k = fs->nactvar - 1; k > operands[i]; k--)
        if (getvarnote1(D, getlocvar1(D, k)) != GENVAR_DISCHARGED) break;
      if (k == operands[i] && note == GENVAR_ONSTACK /*&&
          getnextpc1(fs, var->startpc) >= fs->pc*/) {
        goto stackvardischarged;
      }
      else if (getvarnote1(D, lastvar) <= GENVAR_DISCHARGED) {
        int k;
        setvarnote1(D, fs->nactvar - 1, GENVAR_PERSISTENT);
        for (k = fs->nactvar - 2; k >= 0; k--)
          promotevar1(D, k, GENVAR_PERSISTENT);
      }
    }
    else {
      if (note == GENVAR_ONSTACK) {
        stackvardischarged:
        if (var->startpc >= D->a.bl->node->startpc) {
          setvarnote1(D, operands[i], GENVAR_DISCHARGED);
          j++;
        }
        else
          setvarnote1(D, operands[i], GENVAR_PERSISTENT);
      }
      else if (note == GENVAR_DISCHARGED)
        setvarnote1(D, operands[i], GENVAR_PERSISTENT);
    }
  }
}


/*
** call this when encountering a statement which does not reference one of the
** pending slots on the stack, which can be more than one in the case of a
** comparison or a table assignment; it could also be an opcode that doesn't
** reference any registers such as returning 0 values or a jump
*/
static void commitvarsonstack1(DecompState *D)
{
  DFuncState *fs = D->fs;
  int i;
  for (i = cast_int(fs->nactvar)-1; i >= 0; i--) {
    LocVar *var = getlocvar1(D, i);
    enum GENVARNOTE note = getvarnote1(D, var);
    if (note != GENVAR_DISCHARGED)
      promotevar1(D, i, GENVAR_PERSISTENT);
  }
}


/*
** TEMPPOS is the slot which R would equal if it were eligible to be a temporary
** value
*/
static void onvarreferenced1(DecompState *D, int r, int temppos)
{
  LocVar *var = getlocvar1(D, r);
  enum GENVARNOTE note = getvarnote1(D, var);
  if (note <= GENVAR_ONSTACK && var->startpc >= D->a.bl->node->startpc &&
      r == temppos)
    setvarnote1(D, r, GENVAR_DISCHARGED);
  else {
    promotevar1(D, r, GENVAR_PERSISTENT);
    commitvarsonstack1(D);
  }
}


/*
** check the current block's children and see if any blocks can be folded into
** their parent block; this should be called after a variable's startpc is
** updated or after variables have been pruned when leaving the current block
*/
static void recheckfoldableblocks1(DecompState *D)
{
  DFuncState *fs = D->fs;
  BlockNode *node = D->a.bl->node;
  BlockNode *nextchild = node->firstchild;
  while (nextchild != NULL) {
    if (nextchild->kind == BL_IF && nextchild->firstchild != NULL &&
        nextchild->firstchild->kind == BL_IF &&
        nextchild->firstchild->endpc == nextchild->endpc &&
        nextchild->firstchild->nextsibling == NULL &&
        nextchild->hardstatbeforechild == 0) {
      BlockNode *child = nextchild->firstchild;
      int limitpc = child->startpc-1;
      lu_byte nactvar, nactvar2;
      /* check if there are variables starting between the 2 endpoints */
      getactvar(fs, nextchild->startpc-1, NULL, &nactvar);
      getactvar(fs, limitpc, NULL, &nactvar2);
      if (nactvar == nactvar2) {
        int prevpc = fs->pc;
        /* no new variables; check if existing locals are clobbered between the
           2 endpoints */
        for (fs->pc = nextchild->startpc; fs->pc < limitpc; fs->pc++) {
          updateinsn1(D, fs);
          if (beginseval(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c, 0))
            if (D->a.insn.a < nactvar)
              break;
        }
        if (fs->pc == limitpc) {
          /* no new locals were clobbered (the break didn't happen), so the
             child if-block of this if-block can be melded into it */
          nextchild->startpc = child->startpc;
          nextchild->firstchild = child->firstchild;
          freeblnode(fs, child);
        }
        /* restore pc */
        fs->pc = prevpc;
        updateinsn1(D, fs);
      }
    }
    nextchild = nextchild->nextsibling;
  }
}


static void onvarclobber1(DecompState *D, int r, int pc)
{
  LocVar *var = getlocvar1(D, r);
  enum GENVARNOTE note = getvarnote1(D, var);
  if (note <= GENVAR_ONSTACK)
    setvarnote1(D, r, GENVAR_PERSISTENT);
  else if (note == GENVAR_DISCHARGED) {
    int i;
    for (i = D->fs->nactvar-1; i > r; i--)
      if (getvarnote1(D, getlocvar1(D, i)) != GENVAR_DISCHARGED)
        break;
    if (i != r) {
      setvarnote1(D, r, GENVAR_PERSISTENT);
      return;
    }
    setvarnote1(D, r, GENVAR_ONSTACK);
    if (var->startpc >= D->a.bl->node->startpc) {
      var->startpc = pc;
      /* I want to rescan if-block pairs that can potentially be melded together
         as early as possible so the freed block nodes have a better chance of
         being reused */
      recheckfoldableblocks1(D);
    }
  }
}


/*
** update variable references for opcodes which may not reference any registers
*/
static void updatevarreference1(DecompState *D, OpCode o, int a, int b, int c)
{
  DFuncState *fs = D->fs;
  switch (o) {
    case OP_RETURN: {
      int nret = b-1;
      if (nret != 0) {
        /* multiple returns are handled elsewhere as an open expression  */
        lua_assert(nret == 1);
        onvarreferenced1(D, a, fs->nactvar - 1);
        break;
      }
      /* else this is an empty return, go through */
    }
    /* fallthrough */
    case OP_CLOSE:
    case OP_JMP:
      commitvarsonstack1(D);
      break;
    default: break;
  }
  (void)c;
}


static void updatevars1(DecompState *D, DFuncState *fs)
{
  OpCode o = D->a.insn.o;
  int a = D->a.insn.a;
  int b = D->a.insn.b;
  int c = D->a.insn.c;
  struct pendingstorechain1 *store = D->a.store;
  int pc = fs->pc;
  lua_assert(o != OP_SETLIST);
  if (beginseval(o, a, b, c, 0) && o != OP_TESTSET) {
    int r = a;
    int lastreg = r;
    if (o == OP_LOADNIL || o == OP_VARARG)
      lastreg = (o == OP_LOADNIL) ? b : r + b - 2;
    if (r >= fs->nactvar) {  /* clobbering a free register */
      l_addvar:
      /* see if register A can possibly be a local variable in this block */
      newlocalvar1(D, r, pc);
      if (o == OP_LOADNIL || o == OP_VARARG) {
        int i;
        for (i = r+1; i <= lastreg; i++)
          newlocalvar1(D, i, pc);
      }
    }
    else {  /* r < fs->nactvar */
      int laststored = lastreg;
      if (laststored >= fs->nactvar)
        laststored = fs->nactvar-1;
      if (o == OP_LOADNIL && b >= fs->nactvar) {
        int i;
        for (i = fs->nactvar; i <= b; i++)
          newlocalvar1(D, i, pc);
      }
      if (beginseval(o, a, b, c, 1)) {
        int i;
        for (i = r; i <= laststored; i++)
          onvarclobber1(D, i, pc);
      }
      else { /* R is both operand and destination (r == b || r == c) */
        LocVar *var = getlocvar1(D, r);
        /* check if R is referecnes in a way where it must be a variable */
        if (getvarnote1(D, var) == GENVAR_DISCHARGED || (r == b && r == c))
          setvarnote1(D, r, GENVAR_PERSISTENT);
        else {
          int other;  /* the register operand that is not R */
          int haveC = getCMode(o) != OpArgN;
          int top = fs->nactvar-1;
          if (r != b && !ISK(b)) other = b;
          else if (haveC && !ISK(c) && r != c) other = c;
          else other = -1;
          /*int realtop = top;*/
          lua_assert(top >= 0); /* fs->nactvar must be greater than 0 */
          while (top && getvarnote1(D, getlocvar1(D, top)) == GENVAR_DISCHARGED)
            top--;
          /* something like OP_ADD 0 0 1 where top == 1 */
          if (r == top-1 && (other == top || other == -1)) {
            promotevar1(D, top, GENVAR_DISCHARGED);
            /* R now has a new value on the stack */
            /*setvarnote1(D, r, GENVAR_DISCHARGED);*/
            /*onvarclobber1(D, r, pc);*/
          }
          /* something like OP_ADD 1 1 x  or OP_ADD 1 x 1 where top == 1 */
          else if (r == top && (b == top-1 || (haveC && c == top-1))) {
            if (getvarnote1(D, getlocvar1(D, r)) <= GENVAR_DISCHARGED)
              setvarnote1(D, r, GENVAR_ONSTACK);
          }
          else if (r != top || (other != -1 && !ISK(other)))
            /* todo: shpuld this be realtop instead of top? */
            promotevar1(D, top, GENVAR_PERSISTENT);
        }
      }
      /* Note this path is for A-mode clobberring codes, so only OP_MOVE can be
         a store in this case; if there is a pending store and this code is not
         OP_MOVE or OP_MOVE is moving a lower register to a higher register,
         this is not a store */
      if ((o != OP_MOVE || b < a) && store->pc != -1) {
        dischargestores1(D);
        if (r >= fs->nactvar)
          goto l_addvar;
      }
      else {  /* OP_MOVE is a store */
        if (o == OP_MOVE) {
          if (store->lastreg == -1) {
            store->lastreg = store->firstreg = b;
            store->pc = pc;
          }
          else if (b+1 == store->firstreg) {
            store->firstreg = b;
            store->pc = pc;
          }
          if (store->pc == pc && store->startpc == -1) {
            store->startpc = pc;
            store->laststat = D->a.laststat;
            store->lastup = D->a.lastup;
          }
        }
      }
      /* update highest clobbered if needed */
      if (D->rescan == 0) {
        /* update highest clobbered if needed */
        if (D->rescan == 0 && D->a.bl->highestclobbered < lastreg)
          D->a.bl->highestclobbered = lastreg;
        /* update last statement pc */
        setlaststat1(D, pc, 0);
      }
    }
    if (o == OP_CLOSURE) {
      Instruction next;
      while (next = fs->f->code[++pc], GET_OPCODE(next) == OP_DATA) {
        /* mark local variables used as upvalues */
        if (GETARG_A(next) == 1) {
          int reg = GETARG_Bx(next);
          promotevar1(D, reg, GENVAR_UPVALUE);
        }
      }
      pc = fs->pc;
    }
  }
  else if (o == OP_TESTSET) {
    /* A = B if (bool)B equals C */
    /* TESTSET is generated when either A or B or both are local variables */
    /* on the first TESTSET code, testsetendlabel will be -1, while the rest of
       the TESTSET codes within the expression, testsetendlabel will be a valid
       pc */
    if (D->a.testsetendlabel == -1) {
      int top = fs->nactvar-1;
      LocVar *testedvar = (b <= top) ? getlocvar1(D, b) : NULL;
      LocVar *clobberedvar = (a <= top) ? getlocvar1(D, a) : NULL;
      if (clobberedvar != NULL) {
        onvarclobber1(D, a, pc);
      }
      if (clobberedvar == NULL || clobberedvar->startpc == pc) {
        if (clobberedvar == NULL)
          newlocalvar1(D, a, pc);
        /* since A was not a local, B must be a local */
        if (testedvar != NULL)
          promotevar1(D, b, GENVAR_PERSISTENT);
      }
      else {  /* clobberedvar != NULL */
        if (testedvar != NULL)
          onvarreferenced1(D, b, top);
      }
    }
  }
  else if (isstorecode(o)) {
    int srcreg = IS_OP_SETTABLE(o) ? c : a;
    checkstoreoperands(D, o, a, b, c);
    if (store->lastreg == -1) {
      int n = 0;
      if (store->startpc != -1)
        n = pc - store->startpc;
      store->lastreg = store->firstreg = srcreg + n;
      store->pc = pc;
    }
    else if (srcreg+1 == store->firstreg || store->firstreg == -1) {
      store->firstreg = srcreg;
      store->pc = pc;
      if (o == OP_SETUPVAL || o == OP_SETUPVAL_R1)
        store->upval = 1;
    }
    else {
      dischargestores1(D);
    }
    if (store->pc == pc && store->startpc == -1) {
      store->startpc = pc;
      store->laststat = D->a.laststat;
      store->lastup = D->a.lastup;
    }
    setlaststat1(D, pc, 1);
  }
  else if (testTMode(o)) {
    int r1, r2;
    if (store->pc != -1 && store->pc != pc) {
      ;
    }
    if (testAMode(o))
      r1 = a, r2 = -1;
    else
      r1 = ISK(b) ? -1 : b, r2 = ISK(c) ? -1 : c;
    /* sort r1 and r2, r1 must be less */
    if (r1 > r2 && r2 != -1) {
      int temp = r1; r1 = r2; r2 = temp;
    }
    if (r1 != -1)
      onvarreferenced1(D, r1, fs->nactvar - 1 - (r2 != -1));
    if (r2 != -1)
      onvarreferenced1(D, r2, fs->nactvar - 1);
  }
  else if (o != OP_DATA) {
    updatevarreference1(D, o, a, b, c);
  }
  if (o != OP_DATA) {
    if (store->pc != -1 && store->pc != pc) {
      lua_assert(store->pc < pc);
      dischargestores1(D);
    }
  }
}


/*
** if needed, create a new local variable for the result of an open expression,
** the expression must not be a statement such as a return or call statement
*/
static void createexpresult(DecompState *D, int hasendcode, int iscall)
{
  DFuncState *fs = D->fs;
  /* if there is an end-code for this expression, that instruction encodes the
     result register, otherwise the startpc does (in the case of a table
     consttructor without array items) */
  int resultpc = hasendcode ? D->a.openexpr->endpc : D->a.openexpr->startpc;
  int kind = D->a.openexpr->kind;
  int resultslot = GETARG_A(fs->f->code[resultpc]);
  OpCode nextop;
  int nextB;
  {
    Instruction next = fs->f->code[D->a.openexpr->endpc+1];
    nextop = GET_OPCODE(next);
    nextB = GETARG_B(next);
  }
  /* see if the open expression discharges the resulting value to an existing
     local variable; if it doesn't, create a local variable for the result to
     be implicitly stored (i.e. the first free slot)  */
  if ((!exprpushesresult(kind) || nextop != OP_MOVE || nextB != resultslot)) {
    if (resultslot >= fs->nactvar) {
      newlocalvar1(D, resultslot, D->a.openexpr->endpc);
      if (iscall) {
        int nret = GETARG_C(fs->f->code[resultpc]) - 1;
        int i;
        for (i = 1; i < nret; i++)
          newlocalvar1(D, resultslot+i, D->a.openexpr->endpc);
      }
    }
  }
}


static int getnumforloopvars(DFuncState *fs, BlockNode *node)
{
  lua_assert(isforloop(node));
  if (node->kind == BL_FORNUM)
    return 1;
  else
    return GETARG_C(fs->f->code[node->endpc-1]);
}


static void addforloopaugmentedvars(DecompState *D, BlockNode *node)
{
  int note = node->kind == BL_FORNUM ? GENVAR_FORNUM : GENVAR_FORLIST;
  /* reserve space for augmented vars */
  genvar1(D, node->startpc-1, note)->endpc = node->endpc+1;
  genvar1(D, node->startpc-1, note)->endpc = node->endpc+1;
  genvar1(D, node->startpc-1, note)->endpc = node->endpc+1;
}


static void addforloopvars(DecompState *D, BlockNode *node, int nvars)
{
  int i;
  /* todo: how do you handle the for-loop parser bug */
  for (i = 0; i < nvars; i++)
    genvar1(D, node->startpc, GENVAR_FORLOOPVAR);
}


static void createparams1(DecompState *D)
{
  int i;
  for (i = 0; i < D->fs->f->numparams; i++)
    genvar1(D, 0, GENVAR_PARAM);
}


static void createinitiallocals1(DecompState *D)
{
  DFuncState *fs = D->fs;
  if (fs->firstclobnonparam != -1) {
    int reg = GETARG_A(fs->f->code[fs->firstclobnonparam]);
    int i = fs->f->numparams;
    int pc;
    for (pc = fs->firstclob; pc < fs->firstclobnonparam; pc++) {
      if (GET_OPCODE(fs->f->code[pc]) == OP_LOADNIL) {
        int a = GETARG_A(fs->f->code[pc]);
        int b = GETARG_B(fs->f->code[pc]);
        if (a < fs->f->numparams && b >= fs->f->numparams) {
          for (; i <= b; i++)
            genvar1(D, pc, GENVAR_PERSISTENT);
        }
      }
    }
    for (; i < reg; i++)
      genvar1(D, 0, GENVAR_INITIAL);
  }
}


static void updatelastup1(DecompState *D)
{
  OpCode o = D->a.insn.o;
  int b = D->a.insn.b;
  /* update the highest upvalue referenced */
  if (o == OP_GETUPVAL || o == OP_SETUPVAL || o == OP_SETUPVAL_R1) {
    int up = b;
    if (up > D->a.lastup) {
      if (up > D->a.lastup+1 && o != OP_GETUPVAL) {
        D->a.skippedstorerefpc = D->fs->pc;
        /*set_ins_property(D->fs, D->fs->pc, INS_SKIPPEDREF);*/
      }
      D->a.lastup = up;
    }
  }
}


static void updatereferences1(DecompState *D)
{
  OpCode o = D->a.insn.o;
  int b = D->a.insn.b;
  int c = D->a.insn.c;
  int bx = D->a.insn.bx;
  /* mark any constants referenced by the current instruction */
  int res = referencesk(o, b, c, bx);
  int isref = 0;
  if (res & KREF_B) {
    setkreferenced(D, INDEXK(b));
    isref = 1;
  }
  if (res & KREF_C) {
    setkreferenced(D, INDEXK(c));
    isref = 1;
  }
  else if (res & KREF_BX) {
    setkreferenced(D, INDEXK(bx));
    isref = 1;
  }
  if (isstorecode(o) && isref) {
    int k = (res & KREF_C) ? INDEXK(c) : (res & KREF_B) ? INDEXK(b) :INDEXK(bx);
    while (--k >= 0) {
      if (iskreferenced(D, k) == 0) {
        D->a.skippedstorerefpc = D->fs->pc;
        set_ins_property(D->fs, D->fs->pc, INS_SKIPPEDREF);
        break;
      }
    }
  }
  updatelastup1(D);
}


static void checkslotreferences1(DecompState *D)
{
  DFuncState *fs = D->fs;
  int i,n;
  int slots[3];
  n = referencesslot(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c, slots);
  for (i = 0; i < n; i++) {
    if (slots[i] < fs->nactvar) {
      LocVar *var = getlocvar1(D, slots[i]);
      if (var->startpc < fs->pc && getvarnote1(D, var) < GENVAR_PERSISTENT)
        setvarnote1(D, slots[i], GENVAR_PERSISTENT);
    }
  }
}


static TString *genvarname1(DFuncState *fs, int index, int ispar)
{
#define MAX_EXTRA_CHARS 30
  int i = 0;
  TString *name;
  char buff[sizeof("f_local") + (2 *INT_CHAR_MAX_DEC) + MAX_EXTRA_CHARS];
  const char *fmt = ispar ? "f%d_param%d" : "f%d_local%d";
  size_t len;
  sprintf(buff, fmt, fs->idx, index);
  len = strlen(buff);
  name = luaS_newlstr(fs->H, buff, len);
  while (name->tsv.reserved == MARKED_GLOBAL && i < MAX_EXTRA_CHARS) {
    buff[len++] = '_';
    buff[len] = '\0';
    i++;
    name = luaS_newlstr(fs->H, buff, len);
  }
  if (i == MAX_EXTRA_CHARS) {
    name->tsv.reserved = CONFLICTING_GLOBAL;
  }
  return name;
#undef MAX_EXTRA_CHARS
}


#ifdef LUA_DEBUG
static void printvar(LocVar *var, int i, int note)
{
  lprintf("  (%d)  %s  (%i-%i)  %s\n", i+1, getstr(var->varname),
          var->startpc, var->endpc, varnotenames[note]);
}
#endif /* LUA_DEBUG */


static void updatevarstartpcs1(DFuncState *fs)
{
  DecompState *D = fs->D;
  LocVar *vars = fs->locvars;
  int i;
  int slots[3];
  fs->nopencalls = fs->a->sizeopencalls;
  updatenextopenexpr1(D);
  for (i = 0; i < fs->nlocvars; i++) {
    int reg;
    struct LocVar *var = &vars[i];
    int pc;  /* startpc of this variable */
    int nextpc;  /* startpc of next variable */
    enum GENVARNOTE note = getvarnote1(D, var);
    switch (note) {
      case GENVAR_FILL:
      case GENVAR_FORLOOPVAR:
      case GENVAR_INITIAL:
      case GENVAR_PARAM:
      case GENVAR_FORLIST:
      case GENVAR_FORNUM:
        continue; /* these variables have the correct startpc */
      default: break;
    }
    set_ins_property(fs, var->startpc, INS_LOCVAREXPR);
    reg = GETARG_A(fs->f->code[var->startpc]);
    pc = getnextpc1(fs, var->startpc);
    nextpc = i+1 < fs->nlocvars ? vars[i+1].startpc : fs->f->sizecode-1;
    /* fast-forward to the next open expression from PC */
    while (D->a.openexpr->startpc < pc && fs->nopencalls > 0)
      updatenextopenexpr1(D);
    if (GET_OPCODE(fs->f->code[var->startpc]) == OP_TESTSET)
      pc = getjump(fs, var->startpc+1);
    for (fs->pc = pc; fs->pc < nextpc; fs->pc++) {
      int j, nslots;
      updateinsn1(D, fs);
      if (isstorecode(D->a.insn.o))
        break;
      if (fs->pc == D->a.openexpr->startpc) {
        const OpenExpr *expr = D->a.openexpr;
        /* check if the open expression is not part of an expression */
        if (!exprpushesresult(expr->kind) && expr->kind != CONCATPREP)
          break;
        fs->pc = D->a.openexpr->endpc;
        updatenextopenexpr1(D);
        continue;
      }
      if (D->a.insn.o == OP_DATA)
        goto endofregchecks;
      if (!beginseval(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c, 0))
        break;
      if (D->a.insn.a < reg)
        break;
      if (beginseval(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c, 1))
        break;
      pc = getnextpc1(fs, fs->pc);
      nslots = referencesslot(D->a.insn.o, D->a.insn.a, D->a.insn.b,
                              D->a.insn.c, slots);
      for (j = 0; j < nslots; j++) {
        if (slots[j] < reg)
          break;
        if (j > 0 && slots[j] - slots[j-1] > 1)
          break;
      }
      endofregchecks:
      /* if at the end of a block or start of a branch or loop, stop */
      if (test_ins_property(fs, fs->pc, INS_BLOCKEND) ||
          test_ins_property(fs, fs->pc, INS_LOOPEND))
        break;
      if (test_ins_property(fs, fs->pc, INS_WHILESTAT) ||
          test_ins_property(fs, fs->pc, INS_REPEATSTAT))
        break;
      if (test_ins_property(fs, fs->pc, INS_BRANCHFAIL) ||
          test_ins_property(fs, fs->pc, INS_BRANCHPASS) ||
          test_ins_property(fs, fs->pc, INS_LOOPFAIL) ||
          test_ins_property(fs, fs->pc, INS_LOOPPASS))
        break;
    }
    var->startpc = pc;
  }
}


static void finalizevars1(DFuncState *fs)
{
  DecompState *D = fs->D;
  Analyzer *a = fs->a;
  static const char *const forloop_names[] = {
    "(for index)", "(for limit)", "(for step)",
    "(for generator)", "(for state)", "(for control)"
  };
  int i, v, p;
  int nvars = fs->nlocvars;
  LocVar *vars;
  luaM_reallocvector(fs->H, a->locvars, a->sizelocvars, fs->nlocvars, LocVar);
  a->sizelocvars = fs->nlocvars;
  fs->sizelocvars = a->sizelocvars;
  fs->locvars = a->locvars;
  vars = fs->locvars;
  updatevarstartpcs1(fs);
  D(printf("Local variables\n--------------------------\n"));
  for (i = v = p = 0; i < nvars; i++) {
    struct LocVar *var = &vars[i];
    int note = getvarnote1(D, var);
    if (note == GENVAR_FORNUM || note == GENVAR_FORLIST) {
      int j = (note == GENVAR_FORNUM) ? 0 : 3;
      int k = j+ 3;
      for (; j < k; i++, j++) {
        vars[i].varname = luaS_new(fs->H, forloop_names[j]);
        lua_assert(ispcvalid(fs, vars[i].startpc));
        lua_assert(ispcvalid(fs, vars[i].endpc));
        D(printvar(&vars[i], i, note));
      }
      i--;
    }
    else {
      int ispar = (note == GENVAR_PARAM);
      vars[i].varname = genvarname1(fs, ispar ? p : v, ispar);
      if (ispar) p++;
      else v++;
      D(printvar(var, i, note));
    }
  }
  D(printf("--------------------------\n"));
}


#define REPEAT_UNTIL_TRUE_JUMP  (-2)


static int jump2(DFuncState *fs, int pc, int offs)
{
  int real_target;
  DecompState *D = fs->D;
  const LoopState *currloop = getcurrloop1(fs);
  const BlockState *currblock = D->a.bl;
  int target = pc + 1 + offs; /* the jump target pc */
  /*const Instruction *jc = getjumpcontrol(fs, pc);*/
  if (test_ins_property(fs, pc, INS_BREAKSTAT))
    setloophasbreak(fs, currloop, pc);
  if (test_ins_property(fs, pc, INS_LOOPEND) ||
      test_ins_property(fs, pc, INS_BREAKSTAT) ||
      test_ins_property(fs, target, INS_BOOLLABEL) ||
      test_ins_property(fs, pc, INS_SKIPBOOLLABEL))
    return -1;
  if (target == currloop->exitlabel || target == currloop->breaklabel) {
    if (target == currloop->exitlabel &&
        currblock->isloop && currblock->seenstat == 0) {
      /* this can be a loop condition fail */
      set_ins_property(fs, pc, INS_LOOPFAIL);
      return -1;
    }
    if (target == currloop->breaklabel) {
      int pc;
      for (pc = target-1; pc > fs->pc; pc--) {
        if (test_ins_property(fs, pc, INS_BREAKSTAT)) {
          /* if there is an untested non-break jump before the break */
          if (GET_OPCODE(fs->f->code[pc-1]) == OP_JMP &&
              getjumpcontrol(fs, pc-1) == NULL &&
              !test_ins_property(fs, pc-1, INS_BREAKSTAT) &&
              !test_ins_property(fs, pc-1, INS_LOOPEND) &&
              !test_ins_property(fs, pc-1, INS_BOOLLABEL) &&
              !test_ins_property(fs, pc-1, INS_SKIPBOOLLABEL)) {
            unset_ins_property(fs, pc-1, INS_FAILJUMP);
            return pc;
          }
        }
      }
    }
  }
  /* INS_FAILJUMP, INS_PASSJUMP
     else-branch points are not marked */
  if (target == currblock->f_exitlabel)
    real_target = currblock->node->endpc+1;
  else if (target == currloop->startlabel)
    real_target = currloop->endlabel-1;
  else
    real_target = target;
  return real_target;
}


static void rescanvars1(DecompState *D, BlockState *bl);


/*
** return the highest necessary variable slot between lower LIMIT and the top
*/
static int getlastpersistentvar1(DecompState *D, int limit)
{
  DFuncState *fs = D->fs;
  int i;
  for (i = cast_int(fs->nactvar)-1; i >= limit; i--) {
    enum GENVARNOTE note = getvarnote1(D, getlocvar1(D, i));
    if (ispersistent(note))
      break;
  }
  return i;
}


/*
** prune variables that are not necessary down to LIMIT, which is a lower active
** variable limit, the value of which is included in the pruning, i.e. if LIMIT
** is 1, than variable 1 may be pruned, but not variable 0
*/
static int prunevars1(DecompState *D, int limit)
{
  DFuncState *fs = D->fs;
  int newnactvar = getlastpersistentvar1(D, limit)+1;
  int n = fs->nactvar-newnactvar;  /* number of variables to prune */
  if (n) {
    fs->nlocvars -= n;
    fs->nactvar = newnactvar;
  }
  return n;
}


static void addnodetostate1(BlockState *blockstate, BlockNode *newblock)
{
  BlockNode *parent = blockstate->node;
  BlockNode *child = (lua_assert(parent), parent->firstchild);
  if (child == NULL)
    parent->firstchild = newblock;
  else {
    BlockNode *prevchild = NULL;
    BlockNode *nextchild = NULL;
    while (child != NULL && blstartpc(child) < newblock->startpc) {
      prevchild = child;
      child = child->nextsibling;
    }
    nextchild = child;
    while (nextchild != NULL && blstartpc(nextchild) < newblock->endpc)
      nextchild = nextchild->nextsibling;
    if (nextchild != child) {
      newblock->firstchild = child;
      if (child != NULL) {
        while (child->nextsibling != nextchild)
          child = child->nextsibling;
        child->nextsibling = NULL;
      }
    }
    newblock->nextsibling = nextchild;
    if (prevchild != NULL) prevchild->nextsibling = newblock;
    else parent->firstchild = newblock;
  }
}


static int getifprepstart(DFuncState *fs, int endpc, int firsttempreg)
{
  Instruction lastinsn = fs->f->code[endpc];
  OpCode lastop = GET_OPCODE(lastinsn);
  /* either LASTOP is a test instruction or it is OP_JMP */
  if (testTMode(lastop)) {
    int firstreg = -1;
    int r1, r2;
    int n = getregstested(lastinsn, &r1, &r2);
    lua_assert(n > 0);
    if (n == 2 && r1 > r2) {
      /* swap so R2 >= R1 */
      int temp = r1; r1 = r2; r2 = temp;
    }
    if (n == 2 && r2 >= firsttempreg)
      firstreg = r2;
    if (r1 >= firsttempreg)
      firstreg = r1;
    if (firstreg != -1)
      endpc = getevalstart(fs, endpc, firstreg);
  }
  return endpc;
}


/*
** if needed, end existing variables that will be used as temporary slots in the
** pending open expression
*/
static BlockNode *closelocalvars1(DecompState *D, int varlimit, int pc,
                                  int skipcurrblock)
{
  DFuncState *fs = D->fs;
  BlockNode *ret = NULL;
  if (varlimit < fs->nactvar) {
    int lastnecessaryvar = getlastpersistentvar1(D, varlimit);
    int newnactvar = lastnecessaryvar+1;
    /* see if are there variables that need to be killed before entering the
       open expression */
    if (lastnecessaryvar < varlimit) {
      /* no variables need to end early, just roll back the extra variables as
         if they never existed */
      if (!skipcurrblock)
        fs->nlocvars -= (fs->nactvar - varlimit);
    }
    else {
      BlockState *earliestbl;
      BlockState *bl;
      /* the first variable that needs to be ended */
      LocVar *firstvar = getlocvar1(D, varlimit);
      /* find the earliest parent block containing the first variable (note that
         the startpc of a local variable is initially set as the clobbering
         instruction, so a strict less-than works and avoids needing to check if
         BL has reached the root state) */
      for (bl = D->a.bl; firstvar->startpc < bl->node->startpc; bl--)
        ;
      earliestbl = bl;
      /* for each block containing a variable that needs to end, end it and see
         if duplicates need to be created in child blocks */
      for (;;) {
        int i;
        int iscurrblock = bl == D->a.bl;
        BlockState * nextbl = iscurrblock ? NULL : bl+1;
        int blockendpc = iscurrblock ? pc-1 : nextbl->node->startpc-1;
        /* BLOCKVARLIMIT is an additional constraint when processing a parent
           block, because NACTVAR is up-to-date for the current block */
        int blockvarlimit = iscurrblock ? newnactvar : nextbl->nactvar;
        /* a new lexical block for the variables */
        BlockNode *newblock;
        if (iscurrblock && skipcurrblock)
          return NULL;
        if (bl == earliestbl) {
          /* remove unneeded variables before ending the necessary ones */
          fs->nlocvars -= (fs->nactvar - newnactvar);
          fs->nactvar = newnactvar;
        }
        /* update variable endings for this block state */
        firstvar = NULL;
        if (!iscurrblock && bl->lastnecessaryvar < varlimit) {
          fs->nlocvars -= (fs->nactvar - varlimit);
          fs->nactvar = nextbl->nactvar = varlimit;
        }
        else {
          int isclose, closereg;
          if (!iscurrblock) {
            int kind = nextbl->node->kind;
            /* for-loops and if-blocks actually start earlier than their startpc
            */
            if (kind == BL_FORNUM || kind == BL_FORLIST)
              blockendpc = nextbl->prepstart-1;
            /* for if-blocks, PREPSTART is not necessarily the actual start, it
               is just the latest possible start that can be calculated without
               stack analysis, which isn't done until now since only now is the
               variable information up-to-date for the given block */
            else if (kind == BL_IF)
              blockendpc = getifprepstart(fs, nextbl->prepstart, varlimit)-1;
            /* OP_CLOSE is not important in parent blocks, since it was already
               handled earlier */
            isclose = 0;
          }
          else {  /* updating current block */
            /* in the current block, if the current code is OP_CLOSE, that is
               the pc where both the block and its variables will end */
            isclose = GET_OPCODE(fs->f->code[blockendpc]) == OP_CLOSE;
            closereg = GETARG_A(fs->f->code[blockendpc]);
          }
          for (i = varlimit; i < fs->nactvar && i < blockvarlimit; i++) {
            LocVar *var = getlocvar1(D, i);
            if (var->startpc > blockendpc) break;
            /* if OP_CLOSE ends the variable, the endpc is the same as OP_CLOSE,
               or if this is in a parent block, use BLOCKENDPC+1*/
            if (!iscurrblock)
              var->endpc = blockendpc+1;
            else if (isclose && i >= closereg)
              var->endpc = blockendpc;
            else  /* otherwise it ends `here' */
              var->endpc = pc;
            if (firstvar == NULL) firstvar = var;
          }
        }
        if (nextbl != NULL) {
          /* if this block has a variable that needs to end early, update
             NACTVAR for FS and NEXTBL */
          if (firstvar != NULL)
            fs->nactvar = nextbl->nactvar = varlimit + (blockvarlimit - i);
          /* if the next block clobbers one of the slots that had a deleted
             variable, rescan the block to generate duplicate variables for
             those clobber operations */
          if (nextbl->highestclobbered >= varlimit)
            rescanvars1(D, nextbl);
        }
        if (firstvar != NULL) {
          /* some variables were ended; a do-block needs to be created */
          newblock = createdoblock(fs, firstvar->startpc, blockendpc);
          /* insert the new block into the sibling chain */
          addnodetostate1(bl, newblock);
          ret = newblock;
        }
        if (iscurrblock)
          break;
        bl = nextbl;
      }
    }
    if (!skipcurrblock)
      fs->nactvar = cast_byte(varlimit);
  }
  return ret;
}


static void updatevarsbeforeopenexpr1(DecompState *D)
{
  const OpenExpr *expr = D->a.openexpr;  /* the pending open expression */
  closelocalvars1(D, expr->firstreg, expr->startpc, 0);
}


/*
** OP_CLOSE handler
*/
static void onclose1(DecompState *D, int withdebug)
{
  DFuncState *fs = D->fs;
  BlockState *currblock = D->a.bl;
  BlockNode *node;
  int reg = D->a.insn.a;
  int pc = fs->pc;
  int initialtop = currblock->nactvar + currblock->nforloopvars;
  lua_assert(D->a.insn.o == OP_CLOSE);
  /* adjust local variables in parent blocks before checking if a do-block is
     really needed in the current block */
  closelocalvars1(D, reg, pc+1, 1);
  if (initialtop == reg && getnaturalclosepc(currblock->node) == pc &&
      currblock->node->kind != BL_FUNCTION) {
    /* the current loop or if-statement will generate this OP_CLOSE code, no
       need to add a do-block */
    currblock->node->upval = 1;
    return;
  }
  if (withdebug) {
    const LocVar *firstvar = getlocvar1(D, reg);
    /* find the startpc of the do-block */
    int startpc;
    for (startpc = firstvar->startpc-1; startpc >= 0; --startpc)
      if (test_ins_property(fs, startpc, INS_ASSIGNSTART)) break;
    node = createdoblock(fs, startpc, pc);
    node->upval = 1;
    addnodetostate1(D->a.bl, node);
  }
  else {
    node = closelocalvars1(D, reg, pc+1, 0);
    if (node) {  /* should never be NULL, but just to be safe */
      node->upval = 1;
    }
  }
}


/*
** scans the instructions from a pass-jump up to its target, returns the next pc
** to process
*/
static int
scanpassjump(DecompState *D, DFuncState *fs, int target, int needvars)
{
  int jumppc = fs->pc;
  int pc = jumppc;
  int lowestclobberedreg = -1;
  /* scan the codes leading up to the next fail-jump */
  lua_assert(target > pc);
  do {
    pc = ++fs->pc;
    updateinsn1(D, fs);
    updatereferences1(D);
    /* update lowestclobberedreg */
    if (beginseval(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c, 0)
        && lowestclobberedreg < D->a.insn.a) {
      lowestclobberedreg = D->a.insn.a;
    }
  } while (pc < target-1);
  /* see if any variables need to be erased or ended before here */
  if (lowestclobberedreg != -1 && lowestclobberedreg < fs->nactvar) {
    if (needvars) {
      int lastnecessaryvar = getlastpersistentvar1(D, lowestclobberedreg);
      /* if there are variables that must be kept, but need to be dead by this
         point, append a do-block which ends before this branch so the registers
         will be free */
      if (lastnecessaryvar != -1) {
        int endpc = jumppc - (getjumpcontrol(fs, jumppc) != NULL);
        endpc = getifprepstart(fs, endpc, lastnecessaryvar+1);
        closelocalvars1(D, lowestclobberedreg, endpc, 0);
      }
      (void)rollbackvars1;
    }
  }
  return pc;
}


static void checktestsetstart1(DecompState *D, int pc)
{
  DFuncState *fs = D->fs;
  int label;
  if (D->a.insn.o == OP_TESTSET) {
    label = getjump(fs, pc+1);
    settestsetlabel:
    if (label > D->a.testsetendlabel)
      D->a.testsetendlabel = label;
  }
  else if (test_ins_property(fs, pc, INS_TESTSETJUMP)) {
    lua_assert(D->a.insn.o == OP_JMP);
    label = getjump(fs, getjump(fs, pc)-1);
    goto settestsetlabel;
  }
}


static void checktestsetend1(DecompState *D, int pc)
{
  if (pc == D->a.testsetendlabel) {
    D->a.testsetendlabel = -1;
    setlaststat1(D, pc, 1);
  }
}


static void rescanvars1(DecompState *D, BlockState *bl)
{
  DFuncState *fs = D->fs;
  int startnactvar = fs->nactvar;
  int savedlastup = D->a.lastup;
  int savedpc = fs->pc;
  int pc = bl->highestclobberedresetpc;
  int limitpc = bl == D->a.bl ? fs->pc : (bl+1)->node->startpc;
  BlockNode *nextchild = bl->node->firstchild;
  int nextchildstartpc;
  D->rescan = 1;
  D->a.lastup = bl->savedlastup;
  /* find the next node that comes after PC */
  while (nextchild != NULL && blstartpc(nextchild) < pc)
    nextchild = nextchild->nextsibling;
  nextchildstartpc = nextchild ? nextchild->startpc : -1;
  for (fs->pc = pc; pc < limitpc; pc = ++fs->pc) {
    if (pc == nextchildstartpc) {
      fs->pc = nextchild->endpc;
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      continue;
    }
    updateinsn1(D, fs);
    checktestsetend1(D, pc);
    updatelastup1(D);
    if (test_ins_property(fs, pc, INS_SKIPPEDREF))
      D->a.skippedstorerefpc = pc;
    updatevars1(D, fs);
    /* check for TESTSET start AFTER updating generated variables */
    checktestsetstart1(D, pc);
  }
  prunevars1(D, startnactvar);
  D->rescan = 0;
  D->a.lastup = savedlastup;
  fs->pc = savedpc;
}


/*
** map the N newest local variables from their corresponding registers to their
** positions in the LocVar vector
*/
static void addlocalvars1(DFuncState *fs, int n)
{
  int i;
  lua_assert(fs->nactvar >= 0 && fs->nactvar <= fs->sizelocvars);
  lua_assert(fs->nlocvars >= 0 && fs->nlocvars <= fs->sizelocvars);
  lua_assert(fs->nlocvars >= n);
  lua_assert(fs->nactvar >= n);
  lua_assert(fs->nactvar-1 <= fs->a->sizeactvar);
  for (i = 0; i < n; i++)
    fs->a->actvar[fs->nactvar-n+i] = fs->nlocvars-n+i;
}


static void
chainnode1(BlockNode *parent, BlockNode *next, BlockNode *prev, BlockNode *node)
{
  lua_assert(node != NULL);
  lua_assert(parent != NULL);
  if (next != NULL) {
    if (blstartpc(next) > node->endpc)
      node->nextsibling = next;  /* make it a sibling */
    else {
      BlockNode *lastchild = NULL;
      node->firstchild = next;
      /* find the last child for NODE */
      while (next != NULL && blstartpc(next) < node->startpc) {
        lastchild = next;
        next = next->nextsibling;
      }
      if (lastchild != NULL) {
        node->nextsibling = lastchild->nextsibling;
        lastchild->nextsibling = NULL;
      }
    }
  }
  if (prev != NULL)
    prev->nextsibling = node;
  else
    parent->firstchild = node;
}


static void attachnode1(BlockNode *parent, BlockNode *next, BlockNode *node)
{
  BlockNode *prev = NULL;
  lua_assert(node != NULL);
  lua_assert(parent != NULL);
  if (next != NULL) {
    prev = parent->firstchild;
    lua_assert(prev != NULL);
    if (prev == next)
      prev = NULL;
    else while (prev->nextsibling != next && prev->endpc < node->startpc)
      prev = prev->nextsibling;
  }
  chainnode1(parent, next, prev, node);
}


static void appendifbranch1(DecompState *D, int startpc, int endpc)
{
  DFuncState *fs = D->fs;
  BlockNode *node = addblnode(fs, startpc, endpc, BL_IF);
  BlockNode *parent = D->a.bl->node;
  lua_assert(parent->endpc >= endpc);
  chainnode1(parent, D->a.nextnode, D->a.prevnode, node);
  D->a.nextnode = node;
}


static void appendelsebranch1(DecompState *D, int startpc, int endpc)
{
  DFuncState *fs = D->fs;
  BlockNode *node = addblnode(fs, startpc, endpc, BL_ELSE);
  BlockNode *ifpart = D->a.bl->node;
  BlockNode *parent = (D->a.bl-1)->node;
  lua_assert(ifpart->kind == BL_IF);
  chainnode1(parent, ifpart->nextsibling, D->a.bl->node, node);
}


/*
** make any necessary variable adjustments before pushing a new block state
*/
static void adjustvarsbeforechildblock1(DecompState *D, BlockNode *node)
{
  DFuncState *fs = D->fs;
  int i, base = 0;
  if (isforloop(node)) {
    base = getforloopbase(fs->f->code, node);
    for (i = fs->nactvar; i < base; i++)
      genvar1(D, fs->pc, GENVAR_FILL);
    /* augmented vars start before entering the block */
    addforloopaugmentedvars(D, node);
    return;
  }
}


static int enterblock1(DecompState *D, DFuncState *fs, int needvars)
{
  BlockNode *nextnode = D->a.nextnode;
  if (needvars)
    adjustvarsbeforechildblock1(D, nextnode);
  D->a.bl = pushblockstate1(fs, nextnode);
  /* add the rest of the for-loop variables after entering the block */
  if (isforloop(nextnode)) {
    applyprepstart(D);
    D->a.bl->nforloopvars = getnumforloopvars(fs, nextnode);
    if (needvars)
      addforloopvars(D, nextnode, D->a.bl->nforloopvars);
  }
  else if (nextnode->kind == BL_IF)
    applyprepstart(D);
  D->a.prevnode = NULL;
  D->a.nextnode = D->a.nextnode->firstchild;
  return NODE_STARTPC(D->a.nextnode);
}


static int leaveblock1(DecompState *D, DFuncState *fs)
{
  BlockState *bl = D->a.bl;
  BlockNode *node = bl->node;
  BlockNode *const prevnode = D->a.prevnode;
  int nvars = fs->nactvar - bl->nactvar;
  int laststat = D->a.laststat;
  switch (node->kind) {
    case BL_REPEAT: {
      /* combine repeat-loops if possible */
      if (prevnode != NULL &&
          prevnode->kind == BL_REPEAT && prevnode->startpc == node->startpc &&
          laststat <= prevnode->endpc) {
        /* combine the repeat-loops */
        lua_assert(node->firstchild == prevnode);
        node->firstchild = prevnode->firstchild;
        freeblnode(fs, prevnode);
      }
      break;
    }
    case BL_IF: {
      Instruction nextinsn = fs->f->code[node->endpc+1];
      OpCode nextop = GET_OPCODE(nextinsn);
      int nextA = GETARG_A(nextinsn);
      int nextB = GETARG_B(nextinsn);
      int nextC = GETARG_C(nextinsn);
      if (isstorecode(nextop) || nextop == OP_MOVE) {
        int i, noperands;
        int operands[3];
        if (nextop == OP_MOVE) {
          noperands = 1;
          operands[0] = nextB;
        }
        else
          noperands = getstoreoperands(nextop, nextA, nextB, nextC, operands);
        /* see if any register operand is a local variable in this block, if so,
           then this block must be destroyed so the variable can live outside of
           it and be used in the next store operation */
        for (i = 0; i < noperands; i++)
          if (operands[i] >= bl->nactvar) break;
        if (i < noperands) {
          BlockNode *const parentnode = (bl-1)->node;
          /* destroy the block */
          /* PREVNODE is the last child of NODE */
          if (prevnode != NULL)
            prevnode->nextsibling = node->nextsibling;
          /* find the previous sibling of NODE and disconnect it */
          if (parentnode->firstchild == node)
            parentnode->firstchild = NULL;
          else {
            BlockNode *prevsibling = parentnode->firstchild;
            lua_assert(prevsibling != NULL);
            while (prevsibling->nextsibling != NULL &&
                   prevsibling->nextsibling != node)
              prevsibling = prevsibling->nextsibling;
            /* the first child of NODE will be the new next sibling */
            prevsibling->nextsibling = node->firstchild;
          }
          freeblnode(fs, node);
          /* D->a.prevnode should not change in this case */
          node = prevnode;
        }
      }
      break;
    }
    default: break;
  }
  prunevars1(D, bl->nactvar);
  recheckfoldableblocks1(D);
  D->a.prevnode = node;
  D->a.nextnode = node->nextsibling;
  popblockstate1(fs);
  D->a.bl = getcurrblock1(fs);
  (void)nvars;
  return NODE_STARTPC(D->a.nextnode);
}


static void simblock1(DFuncState *fs)
{
  DecompState *D = fs->D;
  struct pendingstorechain1 store = {-1,-1,-1,-1,-1,-1,0};
  const int needvars = (D->usedebuginfo == 0);
  int nextnodestart;
  int inassignment = 0;
  fs->nactvar =0;
  fs->nlocvars = 0;
  D->a.genprepstart = -1;
  D->a.bl = pushblockstate1(fs,fs->root);
  D->a.store = &store;
  D->a.skippedstorerefpc = -1;
  D->a.lastup = 0;
  D->a.laststat = -1;
  D->a.testsetendlabel = -1;
  D->a.prevnode = NULL;
  /* set the next node to enter */
  D->a.nextnode = fs->root->firstchild;
  nextnodestart = NODE_STARTPC(D->a.nextnode);
  /* intiailize referenced-constants bitmap */
  allockmap(fs->D, fs->f->sizek);
  /* set starting position of open expressions, which is the last one, as they
     are created in reverse order */
  updatenextopenexpr1(D);
  /* generate initial local variables if needed */
  if (needvars) {
    createparams1(D);
    createinitiallocals1(D);
  }
  /* main loop */
  for (fs->pc = 0; fs->pc < fs->f->sizecode; fs->pc++ ) {
    int nvars = 0;  /* number of new variables that start here */
    int isstat = 0;
    int pc = fs->pc;
    updateinsn1(D, fs);
    updateactvar1(fs, pc, &nvars);
    if (nvars) {  /* new vars are detected if using debug info */
      fs->nlocvars += nvars;
      addlocalvars1(fs, nvars);
      if (pc > 0)
        setlaststat1(D, pc-1, 1);  /* a local statement here */
    }
    /* push a new block state if entering the next node */
    while (pc == nextnodestart)
      nextnodestart = enterblock1(D, fs, needvars);
    /* do not process jumps inside a local statement or store statement - these
       kinds of statements are only previously detected when using debug info */
    if (inassignment) {
      if (test_ins_property(fs, pc, INS_ASSIGNEND)) {
        setlaststat1(D, pc, 1);
        inassignment = 0;
      }
      else
        continue;
    }
    else if (test_ins_property(fs, pc, INS_ASSIGNSTART)) {
      setlaststat1(D, pc, 1);
      if (!test_ins_property(fs, pc, INS_ASSIGNEND)) {
        inassignment = 1;
        continue;
      }
    }
    /* check if exiting the current TESTSET expression */
    checktestsetend1(D, pc);
    /* check if entering an open expression */
    if (pc == D->a.openexpr->startpc) {
      int kind = D->a.openexpr->kind;
      int iscall = (kind == CALLPREP);
      if (kind == FORNUMPREP || kind == FORLISTPREP)
        /* set the start of preparation code for the upcoming for-loop */
        D->a.genprepstart = pc;
      if (needvars) {
        /* first handle any pending stores, than see if variables need to be
           ended before the open expression starts */
        if (D->a.store->pc != -1)
          dischargestores1(D);
        updatevarsbeforeopenexpr1(D);
      }
      if (iscall)
        isstat = iscallstat(fs, D->a.openexpr->endpc);
      else
        isstat = (kind == RETPREP || kind == FORNUMPREP || kind == FORLISTPREP);
      if (isstat)
        setlaststat1(D, D->a.openexpr->endpc, 1);
      if (!isstat && needvars)
        createexpresult(D, kind != HASHTABLEPREP && kind != EMPTYTABLE, iscall);
      /* advance to the end of the expression */
      while (pc != D->a.openexpr->endpc) {
        updatereferences1(D);
        pc = ++fs->pc;
        updateinsn1(D, fs);
      }
      /* update constant/upvalue references for the last expression code */
      updatereferences1(D);
      /* update next open expression */
      updatenextopenexpr1(D);
      /* reset highest clobbered */
      resethighestclob1(D, fs->pc+1);
      goto continueloop;
    }
    if (needvars) {
      if (D->a.testsetendlabel == -1)
        updatevars1(D, fs);
    }
    else {  /* have debug info */
      /* if a new variable starts here or an active variable is clobbered, mark
         a new statement (either local statement or local assignment) */
      if (D->a.insn.o != OP_TESTSET)
      if (clobberslocal(fs, D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c))
        setlaststat1(D, pc, 1);
    }
    /* check TESTSET start AFTER updating generated variables */
    checktestsetstart1(D, pc);
    /* mark a return statement that is not an open expression */
    if (D->a.insn.o == OP_RETURN) {
      /* must return 0 or 1 value */
      lua_assert(D->a.insn.b == 1 || D->a.insn.b == 2);
      setlaststat1(D, pc, 1);
    }
    updatereferences1(D);
    /*if (needvars)
      checkslotreferences1(D);*/ (void)checkslotreferences1;
    if (D->a.insn.o == OP_CLOSE)
      onclose1(D, !needvars);
    /* process jumps */
    if (D->a.insn.o == OP_JMP && D->a.testsetendlabel == -1) {
      const Instruction *jc = getjumpcontrol(fs, pc);
      /*int target = pc+1+D->a.insn.sbx;*/
      if (jc != NULL && GET_OPCODE(*jc) == OP_TESTSET) {
        ;
      }
      else {
        int passjump = -1;
        int target = pc + 1 + D->a.insn.sbx;
        int real_target = jump2(fs, pc, D->a.insn.sbx);
        if (test_ins_property(fs, pc, INS_PASSJUMP)) {
          passjump = pc;
          pc = scanpassjump(D, fs, real_target, needvars);
          real_target = jump2(fs, pc, D->a.insn.sbx);
          if (test_ins_property(fs, pc, INS_LOOPFAIL))
            set_ins_property(fs, passjump, INS_LOOPPASS);
        }
        if (ispcvalid(fs, real_target)) {
          if (test_ins_property(fs, pc, INS_FAILJUMP)) {
            BlockState *bl = D->a.bl;
            if (jc == NULL && bl->isbranch && real_target >= bl->t_exitlabel &&
                (pc != bl->node->endpc || real_target > bl->t_exitlabel)) {
              int loopstartpc, loopendpc;
              BlockNode *repeatloop;
              addrepuntiltrue:
              loopendpc = real_target-1;
              unset_ins_property(fs, pc, INS_FAILJUMP);
              set_ins_property(fs, pc, INS_BREAKSTAT);
              for (bl--; bl->isbranch; bl--)
                if (loopendpc < bl->node->endpc) break;
              loopstartpc = (bl+1)->prepstart;
              repeatloop = addblnode(fs, loopstartpc, loopendpc, BL_REPEAT);
              repeatloop->repuntiltrue = 1;
              attachnode1(bl->node, (bl+1)->node, repeatloop);
              /* INS_REPEATSTAT is marked later when the startpc is verified */
              set_ins_property(fs, loopendpc, INS_LOOPEND);
              insertloopstate1(fs, repeatloop);
              goto continueloop;
            }
            if (passjump != -1) {
              set_ins_property(fs, passjump, INS_BRANCHPASS);
              D->a.genprepstart = passjump;
            }
            else
              D->a.genprepstart = pc;
            D->a.genprepstart -= getjumpcontrol(fs, D->a.genprepstart) != NULL;
            set_ins_property(fs, pc, INS_BRANCHFAIL);
            /* this check avoids creating 2 blocks for a single if-statement
               that uses the `and' operator, but if not using debug info, and
               variables have been craeted between the 2 blocks, the check has
               to be done later, when the existence if the variables is
               confirmed; the member `hardstatbeforechild' is a flag for the
               later check which says that the 2 blocks can definitely not be
               combined, because there is a definite statement between the 2
               blocks */
            if (bl->node->kind == BL_IF && target == bl->f_exitlabel) {
              if (bl->seenstat) {
                bl->statbeforechild = 1;
                if (bl->seenhardstat)
                  /* the block state will be popped, so apply the flag to the
                     block node */
                  bl->node->hardstatbeforechild = 1;
                goto l_appendifbranch;
              }
              bl->node->startpc = pc+1;
              recalcemptiness(bl->node);
            }
            else {
              l_appendifbranch:
              /* add a block node for the branch and update NEXTNODESTART */
              appendifbranch1(D, pc+1, real_target-1);
              nextnodestart = NODE_STARTPC(D->a.nextnode);
            }
          }
          else if (!test_ins_property(fs, pc, INS_LOOPFAIL) &&
                   D->a.bl->isbranch && pc == D->a.bl->node->endpc) {
            int i,n;
            lua_assert(D->a.bl->node->kind == BL_IF);
            for (n = 0; ; n++) {
              BlockState *prevbl = D->a.bl-1-n;
              BlockNode *currnode = D->a.bl->node;
              BlockNode *prevnode = prevbl->node;
              if (prevnode->firstchild != currnode || prevnode->kind != BL_IF)
                break;
              if (prevbl->statbeforechild) {
                goto addrepuntiltrue;
                /* todo: this is actually a break out of repeat-until-true */
              }
            }
            /* now that it's confirmed that this jump is a valid else-block,
               pop the block states */
            for (i = 0; i < n; i++) {
              BlockState *prevbl = D->a.bl-1;
              BlockNode *currnode = D->a.bl->node;
              BlockNode *prevnode = prevbl->node;
              lua_assert(currnode->nextsibling == NULL);
              prevnode->firstchild = currnode->firstchild;
              prevnode->startpc = currnode->startpc;
              prevnode->upval |= currnode->upval;
              recalcemptiness(prevnode);
              freeblnode(fs, currnode);
              popblockstate1(fs);
              D->a.bl = getcurrblock1(fs);
            }
            appendelsebranch1(D, pc+1, real_target-1);
          }
        }
      }
    }
    continueloop:
    /* pop the current block state for each block that ends at PC */
    while (pc == D->a.bl->node->endpc) {
      nextnodestart = leaveblock1(D, fs);
      if (D->a.bl == NULL) {
        lua_assert(pc == fs->f->sizecode-1);
        break;
      }
    }
  }
  if (needvars) {
    finalizevars1(fs);
  }
  memset(fs->a->actvar, 0, fs->a->sizeactvar * sizeof(unsigned short));
}


static int getconditionstart(DFuncState *fs, int pc, int startlimit);


static void correctrutstart(DFuncState *fs, BlockNode *node, int minstartpc)
{
  if (testTMode(GET_OPCODE(fs->f->code[node->startpc])))
    node->startpc = getconditionstart(fs, node->startpc+1, minstartpc);
  set_ins_property(fs, node->startpc, INS_REPEATSTAT);
}


/*
** create extra blocks to account for LOADNIL labels which do not already exist
** on a basic block boundary
** remove if-blocks that will not be able to generate matching code due to
** compiler optimizations and turn them into expressions
*/
static void finalizelexicalblocks1(DFuncState *fs, BlockNode *node)
{
  BlockNode *prevchild = NULL;
  BlockNode *nextchild = node->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  int pc;
  int naturaldebuginfo = fs->D->usedebuginfo;
  if (nextchild != NULL && nextchild->repuntiltrue)
    correctrutstart(fs, nextchild, node->startpc);
  for (pc = node->startpc; pc <= node->endpc; pc++) {
    if (pc == nextchildstartpc) {
      finalizelexicalblocks1(fs, nextchild);
      /* note that if NEXTCHILD is empty, pc will end up being decremented by 1,
         then incremented by 1 when continuing, so that this pc is processed
         again under the current node, because it wouldn't be processed by the
         empty child block */
      pc = nextchild->endpc;
      prevchild = nextchild;
      nextchild = nextchild->nextsibling;
      if (nextchild != NULL && nextchild->repuntiltrue)
        correctrutstart(fs, nextchild, prevchild->endpc+1);
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
        int endpc = getnaturalclosepc(node);
        /* a block node needs to be created so that a basic block boundary can
           exist between the 2 LOADNIL codes */
        BlockNode *new_node = addblnode(fs, pc, endpc, BL_IF);  /* if-true */
        new_node->firstchild = nextchild;
        if (prevchild != NULL)
          prevchild->nextsibling = new_node;
        else
          node->firstchild = new_node;
        prevchild = NULL;
        node = new_node;
      }
    }
    /* OP_LOADK is optimized differently depending on if it is part of an
       if-statement condition or a conditional expression; if part of an
       if-statement, the branch is optimized to always go through, i.e. no jump
       is emitted, but if part of a conditional expression such as the
       following:
          local a = 1 or 5;
       the conditional jump is not optimized to go through; because of this
       distinction, the analyzer needs to check OP_LOADK codes that are followed
       by branches, and see if the source code that it would generate will
       compile to the same code; if the LOADK uses a temporary register, then
       the generated source code will be of the form `if <constant> then' and it
       will compile to different code, so the branch is removed from the chain
       so that the code can be interpreted as an expression in pass 2 */
    if (naturaldebuginfo && GET_OPCODE(fs->f->code[pc]) == OP_LOADK &&
        (GET_OPCODE(fs->f->code[pc+1]) == OP_TEST ||
         GET_OPCODE(fs->f->code[pc+1]) == OP_TEST_R1)) {
      if (nextchild != NULL && nextchild->kind == BL_IF &&
          nextchild->startpc == pc+3) {
        int reg = GETARG_A(fs->f->code[pc]);
        lu_byte nactvar;
        getactvar(fs, pc+1, NULL, &nactvar);
        if (reg >= nactvar) {
          BlockNode *sibling = nextchild->nextsibling;
          BlockNode *child = nextchild->firstchild;
          if (child == NULL)
            child = sibling;
          if (prevchild)
            prevchild->nextsibling = child;
          else
            node->firstchild = child;
          freeblnode(fs, nextchild);
          nextchild = child;
          nextchildstartpc = nextchild ? nextchild->startpc : -1;
        }
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
  if (node->kind != BL_WHILE)
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
** get the `follow block' pc, which is the pc where a `return' or `break' can
** exist without needing to augment a do-block around it
*/
static int getblockfollowpc(const BlockNode *node)
{
  int pc;
  switch (node->kind) {
    /* for REPEAT, it needs to be calculated with stack analysis */
    case BL_REPEAT: return node->repuntiltrue ? node->endpc : -1;
    /* for-list ends with TFORLOOP and JMP; subtract 2 from endpc */
    case BL_FORLIST: pc = node->endpc-2; break;
    /* for do-blocks which have OP_CLOSE, subtract 1 from endpc */
    case BL_DO: pc = node->endpc; break;
    /* if-blocks with else parts end with JMP; subtract 1 if needed */
    case BL_IF: pc = node->endpc-haselsepart(node); break;
    case BL_ELSE: pc = node->endpc; break;
    /* the rest have single termination code; subtract 1 */
    default: /* FUNCTION, WHILE, FORNUM */ pc = node->endpc-1; break;
  }
  return pc - node->upval;
}


/*
** restructures the lexical scope hierarchy to preserve local variable info
*/
static void fixblockendings1(DFuncState *fs, BlockNode *node)
{
  /* variables for the most recently processed child block and the next child
     block to process for the current NODE */
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
  int endpc = (node->kind == BL_FUNCTION) ? node->endpc-1 : node->endpc;
  int pc;
  for (pc = node->startpc; pc <= endpc; pc++) {
    struct LocVar *var;
    short nvarshere = 0;
    if (pc == nextchildstartpc) {
      fixblockendings1(fs, nextchild);
      pc = nextchild->endpc;
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
          if (node->kind == BL_IF) {
            if (nextsibling != NULL && nextsibling->kind == BL_IF) {
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
          new_node = createdoblock(fs, new_node_startpc, var->endpc-1);
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
            new_node->firstchild = child;
            prevchild = NULL;
            while (child != NULL && child->endpc <= new_node->endpc) {
              prevchild = child;
              child = child->nextsibling;
            }
            if (prevchild != NULL) {
              new_node->nextsibling = prevchild->nextsibling;
              prevchild->nextsibling = NULL;
            }
          }
          new_node->parentnilvars = nvarshere;
        }
        /* now redo this call; reset fs->nlocvars and do a tailcall with the
           same arguments */
        fs->nlocvars = nlocvars;
        fixblockendings1(fs, node);
        return;
      }
      nvarshere++;
    }
  }
}


static int getconditionstart(DFuncState *fs, int pc, int startlimit)
{
  int i;
  lu_byte nactvar = 0;
  const Instruction *jc;
  lua_assert(ispcvalid(fs, pc));
  lua_assert(ispcvalid(fs, startlimit));
  lua_assert(GET_OPCODE(fs->f->code[pc]) == OP_JMP);
  /* update number of active locals */
  for (i = 0; i < fs->sizelocvars; i++) {
    struct LocVar *var = &fs->locvars[i];
    if (var->startpc <= pc && pc <= var->endpc)
      nactvar++;
  }
  lua_assert(GET_OPCODE(fs->f->code[pc]) == OP_JMP);
  jc = getjumpcontrol(fs,pc);
  if (jc == NULL)  /* unconditional jump */ {
    return pc;  /* the last pc is the follow-block pc */
  }
  else {  /* conditional jump */
    int r, r2;
    int numregs = getregstested(*jc, &r, &r2);
    if (numregs == 1)
      r2 = r;
    if (numregs == 0 || (r < nactvar && r2 < nactvar)) {
      return pc-1;  /* pc before TEST and JMP */
    }
    /* find the lowest temporary register of the 2 that are tested */
    if (r < nactvar)
      r = r2;
    else if (r2 >= nactvar)
      r = (r < r2) ? r : r2;
    /* now walk back down the code to the start of the evluation of R */
    for (pc -= 2; pc > startlimit; pc--) {
      OpCode o = GET_OPCODE(fs->f->code[pc]);
      int a = GETARG_A(fs->f->code[pc]);
      int b = GETARG_B(fs->f->code[pc]);
      int c = GETARG_C(fs->f->code[pc]);
      if (!beginseval(o, a, b, c, 0) || a < nactvar)
        return pc+1;  /* found a code that is not part of the condition */
    }
    return pc;
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
  int followblockpc = -1;
  int lastreturn;
  lu_byte state = 0;  /* state-machine variable */
  lua_assert(node->kind == BL_REPEAT);
  lua_assert(node->repuntiltrue == 0);
  for (pc = node->startpc; pc < node->endpc; pc++) {
    if (pc == nextchildstartpc) {
      /* skip over child blocks */
      pc = nextchild->endpc;
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      state = 0;
      continue;
    }
    if (GET_OPCODE(fs->f->code[pc]) == OP_RETURN ||
        test_ins_property(fs, pc, INS_BREAKSTAT)) {
      /* found a return code; check if it exists on a follow-block pc */
      state = 1;
      lastreturn = pc;
    }
    if (state) {
      /* see if the `until' part has been reached */
      if (test_ins_property(fs, pc, INS_LOOPFAIL) ||
          test_ins_property(fs, pc, INS_LOOPPASS)) {
        followblockpc = getconditionstart(fs, pc, lastreturn) - 1;
        break;
      }
      else if (pc+1 == node->endpc && lastreturn == pc) {
        followblockpc = lastreturn;
        break;
      }
    }
  }
  return followblockpc == -1 ? -1 : followblockpc - node->upval;
}


/*
** mark the `follow block' pc for a node and each of its children
*/
static void markfollowblock1(DFuncState *fs, BlockNode *node)
{
  BlockNode *child = node->firstchild;
  int followblockpc = getblockfollowpc(node);
  if (!ispcvalid(fs, followblockpc) && node->kind == BL_REPEAT)
    followblockpc = findrepeatfollowblock1(fs, node);
  if (ispcvalid(fs, followblockpc))
    set_ins_property(fs, followblockpc, INS_BLOCKFOLLOW);
  while (child != NULL) {
    markfollowblock1(fs, child);
    child = child->nextsibling;
  }
}


/*
** free memory for first pass
*/
static void finalizevectors1(DFuncState *fs)
{
  hksc_State *H = fs->H;
  Analyzer *a = fs->a;
  int newsize = fs->nopencalls;
  /* resize the open expression array */
  luaM_reallocvector(H, a->opencalls, a->sizeopencalls, newsize, OpenExpr);
  a->sizeopencalls = newsize;
}


/*
** The code analyzer works in the first pass and detects the beginnings and ends
** of loops (and determines the type of each loop), the ends of blocks, branch
** jumps, break jumps, and the beginnings of for-loop control variable
** evaluations. It walks through the code backwards, which makes differentiating
** branch tests from loop tests a simpler task.
*/
static void pass1(DFuncState *fs)
{
  BlockNode *func;
  simloop1(fs);
  finalizevectors1(fs);
  rescanloops(fs);
  simblock1(fs);
  func = fs->root;
  lua_assert(func != NULL);
  lua_assert(func->nextsibling == NULL);
  lua_assert(func->kind == BL_FUNCTION);
  /* add post-processing functions here */
  finalizelexicalblocks1(fs, func);
  fs->nlocvars = 0;
  fixblockendings1(fs, func);
  markfollowblock1(fs, func);
  /* end of pass1 post-processing */
  (void)badcode;
  (void)updatefirstclob1;
}


typedef struct StackAnalyzer {
  const Instruction *code;
  BlockNode *currbl;
  BlockNode *currparent;
  struct HoldItem *currheader;  /* current block header hold item */
  int laststore;  /* exp index of last store node */
  int deferleaveblock;  /* how many child blocks have deferred to the parent to
                           leave their block */
  struct {
    int e, target;
  } pendingcond;
  SlotDesc tempslot;  /* a place to put conditional expressions that do not live
                         in a register, such as comparison operations in branch
                         and loop conditions, or testing local registers in
                         branch and loop conditions */
  int numforloopvars;
  /* this field accounts for a compiler bug in Lua 5.1 and Havok Script where
     a new local variable declares at the start of a for-loop which has more
     then 3 initializer expressions will have its value written to the wrong
     register, a register N greater than the correct one, where N is the bias,
     and can be calculated from the number of intializer expressions, the base
     register of the for-loop control vaiables, and the number of iterator
     variables declared in the loop headerx */
  int forloopbias;
  int nextforloopbase;
  int whilestatbodystart;
  int numblockstartatpc;
  int currheaderline;
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
  lua_assert(fs->nactvar >= 0 && fs->nactvar <= fs->sizelocvars);
  lua_assert(fs->nlocvars >= 0 && fs->nlocvars <= fs->sizelocvars);
  lua_assert(fs->nlocvars >= n);
  lua_assert(fs->nactvar+n-1 <= fs->a->sizeactvar);
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
  if (node->kind == BL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  DumpIndentation(fs->D);
  DumpString(bltypename(node->kind),fs->D);
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
  if (node->kind == BL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  if (node->kind != BL_IF || node->nextsibling == NULL ||
      node->nextsibling->kind != BL_ELSE) { /* block ends with `END' */
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
  int type = node->kind;
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
           nextsibling->kind == BL_ELSE) {
    lua_assert(node->endpc+1 == nextsibling->startpc);
  }
}

static void printinsn2(DFuncState *fs, BlockNode *node, int pc, Instruction i)
{
  int type = node->kind;
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

static void dischargestores2(StackAnalyzer *sa, DFuncState *fs);

/*
** adds a new store node to the pending chain
*/
static ExpNode *updatelaststore2(StackAnalyzer *sa,DFuncState *fs,ExpNode *exp)
{
  int prevlaststore = sa->laststore;
  int chainempty = prevlaststore == 0;
  lua_assert(exp != NULL);
  lua_assert(exp->kind == ESTORE);
  exp->previndex = sa->laststore;
  sa->laststore = exp2index(fs, exp);
  /* when preserving line info, if the last expression has a different line
     than this store, add extra parens around it to make it end on the line that
     this store is mapped to */
  if (fs->D->matchlineinfo && chainempty) {
    ExpNode *src = index2exp(fs, sa->lastexpindex);
    if (src != NULL) {
      int line = getexpline(src);
      if (line != exp->line && line != 0) {
        if (src->kind == ECONSTRUCTOR && src->aux <= 0)
          src->aux = exp->line;
        else
          src->closeparenline = exp->line;
      }
    }
  }
  if (chainempty) {
    int issingle;
    int lowesttempreg = exp->u.store.srcreg;
    if (IS_OP_SETTABLE(exp->u.store.rootop) && istempreg(fs, exp->u.store.aux2))
      lowesttempreg = exp->u.store.aux2;
    if (!istempreg(fs, lowesttempreg))
      issingle = fs->firstfree <= fs->nactvar;
    else
      issingle = lowesttempreg == fs->nactvar;
    if (issingle) {
      dischargestores2(sa, fs);
      return NULL;
    }
  }
  return exp;
}


/*
** returns true if EXP is an expression which, in a constructor, would only
** generate code when being closed as a list field
*/
static int issimplelistfield(ExpNode *exp)
{
  switch (exp->kind) {
    /* the following expressions will only generate a single code after the
       parser has advanced to the close brace */
    case ENIL: case ETRUE: case EFALSE: case ELITERAL:
    case ELOCAL: case EUPVAL: case EGLOBAL:
      return 1;
    /* a table index may generate some code before the parser advances if the
       table or key are temporary expressions */
    case EINDEXED:
      return (exp->u.indexed.b != -1 && exp->u.indexed.c != -1);
    /* closures and vararg generate an opcode before `closelistfield' is
       called */
    default:
      return 0;
  }
}


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
#define addtsholditem2(D,i,ts,a) addholditem2(D,i,getstr(ts),(ts)->tsv.len,a)
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


static void dumpexpbool2(DecompState *D, DFuncState *fs, ExpNode *exp)
{
  predumpexp2(D,fs,exp);
  if (exp->kind == ETRUE)
    DumpLiteral("true",D);
  else
    DumpLiteral("false",D);
  postdumpexp2(D,fs,exp);
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
  TString *name = exp->u.name;
  predumpexp2(D,fs,exp);
  if (exp->kind == EGLOBAL && name->tsv.reserved == CONFLICTING_GLOBAL)
    DumpLiteral("_G.",D);
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
  D->lastcl.pc = exp->aux;
  D->lastcl.name = exp->u.cl.name;
  D->lastcl.line = exp->line;
  D->lastcl.haveself = exp->u.cl.haveself;
  DecompileFunction(D, exp->u.cl.p);
  postdumpexp2(D,fs,exp);
  if (D->matchlineinfo == 0) {
    int i;
    /* update lines for pending expressions after the line number may have been
       changed by the child function */
    for (i = 1; i <= fs->a->pendingstk.used; i++) {
      ExpNode *exp = index2exp(fs, i);
      if (exp->pending) {
        exp->line = D->nextlinenumber;
        exp->closeparenline = D->nextlinenumber;
      }
    }
  }
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


/*
** clear references to pending expressions in slot REG, up to N slots, without
** clearing the slot flags; call this when expressions in these slots should no
** longer be reachable through the SlotDesc entry
*/
static void clearslots2(DFuncState *fs, int reg, int n)
{
  int i;
  lua_assert(isregvalid(fs, reg));
  lua_assert(n > 0);
  lua_assert(isregvalid(fs, reg + n - 1));
  lua_assert(istempreg(fs, reg));
  for (i = reg; i < reg + n; i++) {
    getslotdesc(fs, i)->u.expindex = exp2index(fs, NULL);
  }
  fs->firstfree = reg;
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
    if (lastreg)
      lastreg->auxlistnext = 0;
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
  fs->D->nextlinenumber++;
  fs->seenstatinblock = 1;
}


static ExpNode *popexp2(DFuncState *fs, int reg)
{
  ExpNode *exp;
  lua_assert(isregvalid(fs, reg));
  lua_assert(!test_reg_property(fs, reg, REG_LOCAL));
  exp = index2exp(fs, getslotdesc(fs, reg)->u.expindex);
  if (exp != NULL)
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


static void holdRK2(DecompState *D, DFuncState *fs, int reg,
                    struct HoldItem *hold, int addspace)
{
  const char *str;
  size_t len;
  if (ISK(reg)) {
    str = TValueToString(&fs->f->k[INDEXK(reg)], D);
    len = strlen(str);
  }
  else {
    TString *varname;
    lua_assert(test_reg_property(fs, reg, REG_LOCAL));
    varname = getslotdesc(fs, reg)->u.locvar->varname;
    str = getstr(varname);
    len = varname->tsv.len;
  }
  addholditem2(D, hold, str, len, addspace);
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
  struct HoldItem hold1, hold2;
  int isfield;
  int rkkey;
  lua_assert(exp->kind == ESTORE);
  isfield = exp->u.store.rootop == OP_SETFIELD;
  rkkey = exp->u.store.aux2;
  if (isfield && ISK(rkkey) && ttisstring(&fs->f->k[INDEXK(rkkey)])) {
    TString *field = rawtsvalue(&fs->f->k[INDEXK(rkkey)]);
    addtsholditem2(D, &hold1, field, 1);
    addliteralholditem2(D, &hold2, "=", 1);
  }
  else {
    struct HoldItem bracket;
    addliteralholditem2(D, &bracket, "[", 0);
    if (istempreg(fs, rkkey))
      dumpexpoperand2(D, fs, index2exp(fs, exp->auxlistprev), exp, 0);
    else {
      if (exp->line)
        checklineneeded2(D, fs, exp);
      dumpRK2(D, fs, rkkey, 0);
    }
    DumpLiteral("] =",D);
    D->needspace = 1;
  }
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
  int nextreg = iter->firstreg+i;
  ExpNode *nextexp;
  lua_assert(iter->nextexp != NULL);
  iter->lastexp = iter->nextexp;
  /* first try to follow the next expression chained to the current one */
  nextexp = index2exp(iter->fs, iter->nextexp->auxlistnext);
  if (nextexp == NULL || nextexp->info > nextreg) {
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
  struct HoldItem holdparen, holdleft;
  lua_assert(exp != NULL);
  lua_assert(exp->pending);
  exp->pending = 0;
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
    case EBINOP: { /* binary operation */
      ExpNode *o1, *o2;
      BinOpr op = exp->u.binop.op;
      struct HoldItem holdbinop;
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
      if (b == -1) {
        /* discharge pending expression in B */
        dumpexpoperand2(D, fs, o1, exp, priority[op].left);
        DumpSpace(D);
        DumpBinOpr(op,D); /* operator */
        D->needspace = 1;
      }
      else {
        const char *binopstring;
        /* dump constant INDEXK(B) or local variable in B */
        holdRK2(D, fs, b, &holdleft, 1); /* first operand */
        binopstring = getbinoprstring(op);
        addholditem2(D, &holdbinop, binopstring, strlen(binopstring), 1);
      }
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
      int needparen;
      o1->leftside = 1;
      if (exp->leftside)
        needparen = (priority[op].right < limit);
      else
        needparen = (priority[op].left <= limit);
      if (needparen || needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      dumpexpoperand2(D, fs, o1, exp, priority[op].left);
      DumpSpace(D);
      DumpBinOpr(op,D);
      D->needspace = 1;
      dumpexpoperand2(D, fs, o2, exp, priority[op].right);
      if (needparen || needparenforlineinfo)
        dumpcloseparen2(D, fs, exp);
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
      if (b == -1) {
        dumpexpoperand2(D, fs, tab, exp, SUBEXPR_PRIORITY);
        D->needspace = 0;
      }
      else
        holdRK2(D, fs, b, &holdleft, 0);
      if (isfield)
        addliteralholditem2(D, &holditem, ".", 0);
      else if (isself == 0)
        addliteralholditem2(D, &holditem, "[", 0);
      else
        addliteralholditem2(D, &holditem, ":", 0);
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
      int totalitems = exp->u.cons.narray + exp->u.cons.nhash;
      if (needparen)
        addliteralholditem2(D, &holdparen, "({", 0);
      else
        addliteralholditem2(D, &holdparen, "{", 0);
      if (totalitems) {
        int i;
        int reg;
        int linestep, bracketline;
        ExpNode *firstitem;
        ExpNode *nextarrayitem = index2exp(fs, exp->u.cons.firstarrayitem);
        ExpNode *nexthashitem = index2exp(fs, exp->u.cons.firsthashitem);
        lua_assert(nextarrayitem != NULL || nexthashitem != NULL);
        if (nextarrayitem != NULL) {
          initexplistiter2(&iter, fs, exp->info+1, nextarrayitem);
          firstitem = nextarrayitem;
        }
        else
          firstitem = nexthashitem;
        if (D->matchlineinfo) {
          if (firstitem) {
            bracketline = exp->line;
            linestep = exp->line < exp->aux;
          }
          else
            linestep = 0;
        }
        else {
          linestep = (totalitems > 3);
          bracketline = D->nextlinenumber;
        }
        if (linestep) {
          updateline2(fs, bracketline, D);
          dischargeholditems2(D);
          D->indentlevel++;
        }
        else if (D->matchlineinfo) {
          checklineneeded2(D, fs, exp);
          dischargeholditems2(D);
        }
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
            if (D->matchlineinfo == 0) {
              nexthashitem->line = D->nextlinenumber += linestep;
              nexthashitem->closeparenline = nexthashitem->line;
            }
            dumphashitem2(D, fs, nexthashitem);
            nexthashitem = index2exp(fs, nexthashitem->auxlistnext);
          }
          else {
            if (i == totalitems - 1) {
              if (D->matchlineinfo && linestep) {
                /* if the final array item is mapped to the same line as the
                   close brace, see if it can be written one line earlier, which
                   is usually the case in source code */
                if (exp->aux && nextarrayitem->line == exp->aux) {
                  /* check if this array item would have only generated code
                     after the parser advanced to the close brace line */
                  if (issimplelistfield(nextarrayitem) &&
                      nextarrayitem->line - 1 > D->linenumber) {
                    nextarrayitem->closeparenline = --nextarrayitem->line;
                  }
                }
              }
            }
            if (D->matchlineinfo == 0) {
              nextarrayitem->line = D->nextlinenumber += linestep;
              nextarrayitem->closeparenline = nextarrayitem->line;
            }
            dumpexp2(D, fs, nextarrayitem, 0);
            reg++;
            if (i == totalitems - 1) {
              nextarrayitem = NULL;
              break;
            }
            {
              ExpNode *nextinlist = getnextexpinlist2(&iter, reg);
              if (nextinlist != nextarrayitem)
                nextarrayitem = nextinlist;
              else {
                nextarrayitem = NULL;
                nextinlist->pending = 0;
              }
            }
          }
        }
        if (linestep) {
          D->indentlevel--;
          if (D->matchlineinfo == 0)
            exp->aux = D->nextlinenumber += linestep;
        }
        /* exp->aux has the line mapping for OP_SETLIST if present */
        if (exp->aux) {
          updateline2(fs, exp->aux, D);
          if (needparen && !needparenforlineinfo)
            exp->closeparenline = D->linenumber;
        }
      }
      else {  /* totalitems == 0 */
        /* discharge hold items */
        predumpexp2(D, fs, exp);
        postdumpexp2(D, fs, exp);
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
        initexplistiter2(&iter, fs, firstexp->info+1+(firstexp->kind == ESELF),
                         firstexp);
        if (firstexp->kind == ESELF) {
          D(lprintf("firstexp->auxlistnext = %d\n", firstexp->auxlistnext));
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
    case ETRUE:
    case EFALSE:
    case ELITERAL:
    case ENIL:
    case EVARARG:
    case ECLOSURE: {
      int needparen = (limit == SUBEXPR_PRIORITY || needparenforlineinfo);
      if (needparen)
        addliteralholditem2(D, &holdparen, "(", 0);
      if (exp->kind == ETRUE || exp->kind == EFALSE)
        dumpexpbool2(D,fs,exp);
      else if (exp->kind == ELITERAL)
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
  int line = getline2(fs, pc);
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
  int line = getline2(fs, pc);
  int needblock;
  lua_assert(isregvalid(fs, reg));
  if (D->matchlineinfo && nret != 0) {
    ExpNode *last = getexpinreg2(fs, (nret == -1) ? reg : reg+nret-1);
    if (last != NULL) {
      int expline = getexpline(last);
      if (last->kind == ECLOSURE)
        expline = getline2(fs, last->aux);
      /* if this return code is mapped to a different line than the last
         expression to be returned, wrap that expression in parens and put the
         closing paren on the line that the return is mapped to; this preserves
         line info when recompiling */
      if (last != NULL && line != expline)
        last->closeparenline = line;
    }
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
    if (nret == -1)
      nret = fs->firstfree - reg;
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
    wasempty = sb->empty;
  }
  if (!isfield) {
    sb->needspace = needspace;
    if (wasempty)
      D->needspace = needspace;
    addchartolhs2(sb, ']');
  }
  if (wasempty) {
    DumpBlock(luaZ_buffer(sb->buff), luaZ_bufflen(sb->buff), D);
    flushlhsbuff2(sb);
    D->needspace = 1;
  }
}


static void addtab2funcname(struct LHSStringBuilder *sb, ExpNode *exp)
{
  DFuncState *fs = sb->fs;
  if (exp->kind == EGLOBAL || exp->kind == EUPVAL) {
    addtstolhs2(sb, exp->u.name);
    sb->needspace = 0;
  }
  else if (exp->kind == EINDEXED) {
    if (exp->u.indexed.b == -1) {
      ExpNode *tab = index2exp(fs, exp->u.indexed.bindex);
      addtab2funcname(sb, tab);
      sb->needspace = 0;
    }
    else {
      LocVar *var = getlocvar2(fs, exp->u.indexed.b);
      addvarnametolhs2(sb, var->varname);
      sb->needspace = 0;
    }
    if (exp->u.indexed.c != -1 && ISK(exp->u.indexed.c) &&
        ttisstring(&fs->f->k[INDEXK(exp->u.indexed.c)])) {
      addtstolhs2(sb, rawtsvalue(&fs->f->k[INDEXK(exp->u.indexed.c)]));
      sb->needspace = 0;
    }
  }
  exp->pending = 0;
  addchartolhs2(sb, '.');
}


static TString *buildfuncname(struct LHSStringBuilder *sb, ExpNode *exp,
                              int needself)
{
  DFuncState *fs = sb->fs;
  if (exp->kind != ESTORE) {
    return getlocvar2(fs, exp->info)->varname;
  }
  else {
    OpCode rootop = exp->u.store.rootop;
    if (rootop == OP_SETGLOBAL) {
      TString *name = rawtsvalue(&fs->f->k[exp->u.store.aux1]);
      if (name->tsv.reserved == CONFLICTING_GLOBAL) {
        addtolhsbuff2(sb, "_G.", sizeof("_G.")-1);
        sb->needspace = 0;
        addvarnametolhs2(sb, name);
        return luaS_newlstr(fs->H,luaZ_buffer(sb->buff),luaZ_bufflen(sb->buff));
      }
      return rawtsvalue(&fs->f->k[exp->u.store.aux1]);
    }
    else if (rootop == OP_SETUPVAL) {
      return fs->upvalues[exp->u.store.aux1];
    }
    else if (rootop == OP_SETFIELD || rootop == OP_SETTABLE) {
      int tab = exp->u.store.aux1;
      int k = exp->u.store.aux2;
      if (!istempreg(fs, tab)) {
        addvarnametolhs2(sb, getslotdesc(fs, tab)->u.locvar->varname);
        sb->needspace = 0;
        addchartolhs2(sb, '.');
      }
      else {
        ExpNode *exp = getexpinreg2(fs, tab);
        addtab2funcname(sb, exp);
      }
      sb->needspace = 0;
      if (needself) {
        sb->buff->n--;
        addchartolhs2(sb, ':');
        sb->needspace = 0;
      }
      if (ISK(k) && ttisstring(&fs->f->k[INDEXK(k)])) {
        addtstolhs2(sb, rawtsvalue(&fs->f->k[INDEXK(k)]));
      }
      return luaS_newlstr(fs->H, luaZ_buffer(sb->buff), luaZ_bufflen(sb->buff));
    }
  }
  return NULL;
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
  TString *funcname = NULL;
  struct LHSStringBuilder sb;
  struct HoldItem lhs;  /* LHS names */
  ExpNode *exp;  /* iterator for traversing the store-chain */
  ExpNode *lastsrc;  /* last source expression */
  int lastsrcreg;  /* greatest source register that is referenced in a store */
  int i = 0;
  exp = index2exp(fs, sa->laststore);
  lua_assert(exp != NULL);
  initstringbuilder2(&sb, D);
  /* check if there is a single store with a closure */
  if (index2exp(fs, exp->previndex) == NULL) {
    ExpNode *src = (exp->kind == ESTORE) ? index2exp(fs, exp->aux) : exp;
    if (src != NULL && src->kind == ECLOSURE) {
      if (D->usedebuginfo == 0 || src->u.cl.p->name != NULL) {
        int needself = (D->usedebuginfo &&
                        strchr(getstr(src->u.cl.p->name), ':') != NULL);
        funcname = buildfuncname(&sb, exp, needself);
        src->u.cl.haveself = needself;
        if (D->usedebuginfo && src->u.cl.p->name != NULL) {
          /* set the proper line number for the closure to start on  */
          src->line = src->closeparenline = exp->line;
        }
      }
      src->u.cl.name = funcname;
      lastsrcreg = (exp->kind == ESTORE) ? exp->u.store.srcreg : exp->info;
    }
  }
  if (funcname == NULL) {
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
        TString *varname;
        if (rootop == OP_SETGLOBAL) {
          varname = rawtsvalue(&fs->f->k[exp->u.store.aux1]);
          if (varname->tsv.reserved == CONFLICTING_GLOBAL) {
            addtolhsbuff2(&sb, "_G.", sizeof("_G.")-1);
            sb.needspace = 0;
          }
        }
        else
          varname = fs->upvalues[exp->u.store.aux1];
        addvarnametolhs2(&sb, varname);
      }
      else
        lua_assert(0);
      i++;
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
  }
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
    if (src != NULL && src != lastsrc) {
      /* for N remaining variables, if there are exactly N expressions
         remaining and they are all nil, do not write them, the entire
         assignment is just one `nil', which needs to be written */
      if (i != 0 && src != exp && src->kind == ENIL && src->aux == lastsrcreg)
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
      else if (lastsrc->kind == ECALL)
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
  if (funcname == NULL)
    DumpSemi(D); /* `;' */
  flushpendingexp2(fs); /* flush everything */
  sa->laststore = 0;
}


/*
** activates new local variables and emits a declaration/initialization
** statement for them
*/
static void emitlocalstat2(DFuncState *fs, int nvars, int pc)
{
  ExpNode dummy;
  ExpNode *firstexp, *lastexp;
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  int firstreg = fs->nactvar;
  int i, lastreg;
  int haveRHS = 1;
  TString *funcname = NULL;
  int seenfirstexp = 0;
  int skipsemiforlineinfo = 0;
  Mbuffer *b;
  struct HoldItem lhs;
  lua_assert(isregvalid(fs, firstreg));
  lua_assert(nvars > 0);
  lua_assert(isregvalid(fs, firstreg+nvars-1));
  b = &D->buff;
  luaZ_resetbuffer(b);
  lastreg = firstreg+nvars-1;
  firstexp = getexpinreg2(fs, firstreg);
  lastexp = NULL;
  if (nvars == 1 && firstexp && firstexp->kind == ECLOSURE) {
    TString *varname = getlocvar2(fs, fs->nactvar)->varname;
    if (fs->D->usedebuginfo && firstexp->u.cl.p->name != NULL)
      /* use the actual local variable name as the function name because p->name
         is clamped to 512 characters */
      funcname = varname;
    else if (fs->D->usedebuginfo == 0 &&
             test_ins_property(fs, firstexp->aux, INS_SELFUPVAL)) {
      funcname = varname;
    }
    firstexp->u.cl.name = funcname;
  }
  if (funcname != NULL) {
    /* add hold item for `local' before `function <funcname>' */
    addliteralholditem2(D, &lhs, "local", 1);
  }
  else {
    /* add hold item for LHSs */
    addliteral2buff(H, b, "local ");
    for (i = firstreg; i <= lastreg; i++) {
      size_t len;
      struct LocVar *var = getlocvar2(fs, fs->nactvar+(i-firstreg));
      lua_assert(var->varname != NULL);
      len = var->varname->tsv.len;
      addstr2buff(H, b, getstr(var->varname), len);
      if (i != lastreg)
        addliteral2buff(H, b, ", ");
    }
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
    if (D->matchlineinfo && firstexp->kind == ENIL && firstexp->aux == lastreg)
    {
      if (pc+1 < fs->f->sizecode-1 && getline(fs->f, pc+1) == firstexp->line &&
          D->linenumber <= firstexp->line - 1) {
        haveRHS = 0;
        skipsemiforlineinfo = 1;
        firstexp->closeparenline = --firstexp->line;
      }
    }
    /* if only assigning nil's, avoid writing the RHS altogether, unless it is
       just one variable */
    if (haveRHS &&
        (nvars > 1 && firstexp->kind == ENIL && firstexp->aux == lastreg))
      haveRHS = 0;
    if (haveRHS)
      addliteral2buff(H, b, " = ");
    addholditem2(D, &lhs, luaZ_buffer(b), luaZ_bufflen(b), 0);
    D(printf("added hold item for decl: `%.*s'\n", cast_int(luaZ_bufflen(b)),
             luaZ_buffer(b)));
  }
  D(lprintf("firstreg = %d, lastreg = %d\n", firstreg, lastreg));
  for (i = firstreg; i <= lastreg; i++) {
    ExpNode *exp = getexpinreg2(fs, i); /* the pending expression in REG */
    setreglocal2(fs, i, getlocvar2(fs, fs->nactvar++));
    if (haveRHS == 0) {
      exp->pending = 0;
      continue; /* don't write  */
    }
    else if (exp != NULL && exp != lastexp) {
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
        continue; /* vararg has already been emitted */
      }
      else if (lastexp->kind == ECALL &&
               (lastexp->info + lastexp->u.call.nret) > i) {
        continue; /* function call has already been emitted */
      }
      else if (nvars > 1 && lastexp->kind == ENIL && lastexp->aux == lastreg) {
        continue; /* only nil's remain; don't print them */
      }
      else if (lastexp->kind == ENIL) {
        if (seenfirstexp)
          DumpComma(D);
        lastexp->pending = 1;
        dumpexp2(D, fs, lastexp, 0);
      }
      else {
        DumpLiteral("UNHANDLED CASE in initlocvar2\n",D);
      }
    }
  }
  if (seenfirstexp == 0) {
    /* no expressions have been dumped, but the items in the hold need to be
       discharged and the line needs to be updated */
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
  if (funcname == NULL && skipsemiforlineinfo == 0)
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
  if (exp->kind == ECONSTRUCTOR && exp2index(fs, exp) == fs->curr_constructor)
    return exp;
  if (exp2index(fs, exp) == sa->pendingcond.e)
    return exp;
  /* a call statement does not go into a register */
  if (exp->kind == ECALL && exp->u.call.nret == 0) {
    exp->previndex = getslotdesc(fs, reg)->u.expindex;
    return exp;
  }
  if (!test_reg_property(fs, reg, REG_LOCAL)) {
    int link, lastreg;
    if (exp->kind == ENIL || exp->kind == ESELF)
      lastreg = exp->aux;
    else if (exp->kind == EVARARG && exp->aux > 1)
      lastreg = exp->info + exp->aux-2;
    else if (exp->kind == ECALL && exp->u.call.nret > 1)
      lastreg = exp->info + exp->u.call.nret - 1;
    else
      lastreg = reg;
    lua_assert(isregvalid(fs, lastreg));
    link = fs->firstfree <= lastreg;
    if (link) {
      fs->firstfree = lastreg+1;
    }
    if (exp->kind == ECONDITIONAL)
      link =0 ;
    else if (exp->kind == ECALL)
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
    else if (exp->kind != ENIL) {
      int i;
      for (i = reg+1; i <= lastreg; i++)
        pushexp2(fs, i, exp, 0);
    }
    else {  /* exp->kind == ENIL */
      /* push an expression node for each register clobbered by LOADNIL */
      int i;
      int last = exp->aux;
      int prev = exp2index(fs, exp);
      for (i = reg+1; i <= last; i++) {
        ExpNode *prevexp;
        pushexp2(fs, i-1, exp, link);
        exp = newexp(fs);
        prevexp = index2exp(fs, prev);
        *exp = *prevexp;
        exp->info = i;
        exp->auxlistprev = prev;
        prev = exp2index(fs, exp);
        prevexp->auxlistnext = prev;
      }
      reg = last;
    }
    pushexp2(fs, reg, exp, link);
    if (splitnil != NULL) *splitnil = 0;
    return exp;
  }
  else {
    lua_assert(fs->nactvar > 0);
    lua_assert(exp->info < fs->nactvar);
    /* handle LOADNIL with multiple local registers, where the single code is
       shared by multiple separate assignment statements */
    if (exp->kind == ENIL && exp->aux > exp->info) {
      /* `dischargestores2' will free all pending expressions, and EXP will be
         cloned for each store that needs to be emitted */
      ExpNode node = *exp;
      /* NEXTLINE is the line that the decompiler can generate statements up to
       */
      int nextline;
      int line = node.line;
      int i = exp->info;
      /* LASTNILREG is the last register clobbered by OP_LOADNIL */
      int lastnilreg = exp->aux;
      /* LASTSTOREREG is the limit on when to stop emitting stores, based on
         what makes sense; if OP_LOADNIL clobbers a temporary register as well,
         the limit will be set to the first temporary register that is
         clobbered; otherwise, it will be set to the last local register to
         emit a store for; it is computed such that the final node left before
         the recrusive tailcall will include either only a local register or
         only temporary registers, ensuring the recursive call does not get here
      */
      int laststorereg = exp->aux < fs->nactvar ? exp->aux : fs->nactvar;
      if (fs->D->matchlineinfo) {
        if (GET_OPCODE(fs->f->code[sa->pc+1]) != OP_CLOSURE)
          nextline = getline2(fs, sa->pc+1);
        else
          nextline = exp->line;
      }
      else
        /* compute a line number that gives each assignment its own line */
        nextline = line + exp->aux - exp->info;
      /* emit a store for each local register clobbered by OP_LOADNIL */
      do {
        /* isolate the nil expression to just the next register to store */
        exp->info = exp->aux = i++;
        /* add to the store chain */
        exp->previndex = sa->laststore;
        sa->laststore = exp2index(fs, exp);
        /* emit store */
        dischargestores2(sa, fs);
        exp = newexp(fs);
        *exp = node;
        /* check if there is room to increment the line number */
        if (line + 1 < nextline)
          exp->line = exp->closeparenline = ++line;
      } while (i < laststorereg);
      /* now there is 1 node left for the final clobbered local register */
      exp->info = laststorereg;
      exp->aux = lastnilreg;
      /* fix line for final node if needed */
      if (exp->line < fs->D->linenumber) {
        int expline;
        if (line + 1 < nextline ||
            (sa->pc+1 == fs->f->sizecode-1 && line + 1 == nextline))
          expline = line+1;
        else
          expline = fs->D->linenumber;
        exp->line = exp->closeparenline = expline;
      }
      if (sa->nextopenexpr != NULL && sa->nextopenexpr->startpc == sa->pc+1 &&
          exp->info == sa->nextopenreg)
        exp->line = exp->closeparenline = nextline;
      else if (fs->D->matchlineinfo == 0) {
        exp->line = exp->closeparenline = fs->D->nextlinenumber;
      }
      if (splitnil != NULL) *splitnil = (lastnilreg != laststorereg);
      /* add a new expression for the rest of the registers */
      return addexptoreg2(sa, fs, exp->info, exp, NULL);
    }
    exp->previndex = sa->laststore;
    sa->laststore = exp2index(fs, exp);
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
  exp->previndex = exp->auxlistprev = exp->auxlistnext = 0;
  exp->info = reg;
  exp->aux = exp->info + sa->openexprnildebt-1;
  if (GET_OPCODE(fs->f->code[sa->pc]) != OP_CLOSURE) {
    exp->line = getline2(fs, sa->pc);
    exp->closeparenline = exp->line;
  }
  else if (sa->nextopenexpr->kind == CALLPREP) {
    exp->line = getline2(fs, sa->nextopenexpr->endpc);
    exp->closeparenline = exp->line;
  }
  exp->leftside = 0;
  exp->pending = 1;
  addexptoreg2(sa, fs, exp->info, exp, NULL);
  sa->openexprnildebt = 0;
}


static void initexp2(DFuncState *fs, ExpNode *exp, int reg, int pc)
{
  lua_assert(ispcvalid(fs, pc));
  exp->info = reg;
  exp->aux = 0;
  exp->previndex = exp2index(fs, NULL);
  if (reg <= 0)
    exp->auxlistprev = exp2index(fs, NULL);
  else
    exp->auxlistprev = exp2index(fs, getexpinreg2(fs, reg-1));
  exp->auxlistnext = exp2index(fs, NULL);
  exp->line = getline2(fs, pc);
  exp->closeparenline = exp->line;
  exp->leftside = 0;
  exp->pending = 1;
  exp->goiftrue = 0;
  exp->endlabel = -1;
}


static void linkexp2(StackAnalyzer *sa, DFuncState *fs, ExpNode *exp)
{
  ExpNode *prevreg = index2exp(fs, exp->auxlistprev);
  sa->lastexpindex = exp2index(fs, exp);
  if (prevreg) {
    prevreg->auxlistnext = sa->lastexpindex;
    if (prevreg->kind == ECONSTRUCTOR && prevreg->aux == 0)
      prevreg->u.cons.firstarrayitem = sa->lastexpindex;
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
      if (test_ins_property(fs, pc, INS_BOOLLABEL))
        return c ? NULL : getexpinreg2(fs, a);
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
      exp->u.cons.arrsize = luaO_fb2int(b);
      exp->u.cons.hashsize = c > 0 ? luaO_fb2int(c-1)+1 : 0;
      exp->u.cons.narray = exp->u.cons.nhash = 0;
      exp->u.cons.firstarrayitem = 0;
      exp->u.cons.firsthashitem = 0;
      exp->u.cons.lasthashitem = 0;
      break;
    case OP_SETLIST: {
      ExpNode *tab = getexpinreg2(fs, a);
      /* should always be non-NULL, but a lone OP_SETLIST can crash the program
         if this isn't checked */
      if (tab != NULL || tab->kind != ECONSTRUCTOR) {
        int nitems = b;
        if (nitems == 0) {  /* b == 0 means set up to stack top */
          /* get number of array expressions pushed */
          nitems = fs->firstfree-(tab->info+1);
        }
        /* link together groups of stack items separated by SETLIST */
        if (tab->aux != 0 && tab->u.cons.firstarrayitem) {
          ExpNode *node = index2exp(fs, tab->u.cons.firstarrayitem);
          while (node->auxlistnext != 0)
            node = index2exp(fs, node->auxlistnext);
          node->auxlistnext = exp2index(fs, getexpinreg2(fs, tab->info+1));
        }
        tab->auxlistnext = 0;
        tab->u.cons.narray += nitems;
        fs->curr_constructor = exp2index(fs, tab);
        /* clear stack space of array items */
        clearslots2(fs, tab->info+1, nitems);
      }
      if (fs->D->matchlineinfo)
        tab->aux = getline(fs->f, pc);
      /* I use AUX being non-zero as an indication that OP_SETLIST has been
         encountered for this table, so that its first array item does not get
         updated anymore */
      if (tab->aux == 0)
        tab->aux = -1;
      sa->lastexpindex = exp2index(fs, tab);
      /* return the existing table constructor node */
      return tab;
    }
    case OP_CLOSURE:
      exp->kind = ECLOSURE;
      exp->aux = pc;
      exp->u.cl.p = f->p[bx];
      exp->u.cl.name = NULL;
      exp->u.cl.haveself = 0;
      exp->line = exp->closeparenline = 0;
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
      exp->u.indexed.isfield = isfieldname(rawtsvalue(&fs->f->k[c]));
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
    case OP_TAILCALL:
    case OP_TAILCALL_I:
    case OP_TAILCALL_I_R1:
    case OP_TAILCALL_C:
    case OP_TAILCALL_M:
      exp->kind = ECALL;
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
      if (exp->u.call.narg != 0) {
        int narg = exp->u.call.narg;
        if (narg == -1) narg = 1;
        /* clear stack space of the arguments */
        clearslots2(fs, exp->info+1, narg);
      }
      exp->aux = exp2index(fs, getexpinreg2(fs, a+exp->u.call.narg));
      break;
    case OP_CONCAT:
      exp->kind = ECONCAT;
      exp->u.concat.firstindex = exp2index(fs, getexpinreg2(fs, b));
      exp->u.concat.lastindex =exp2index(fs, getexpinreg2(fs, c));
      exp->aux = c+1-b;  /* number of expressions in concat */
      /* clear stack space of operands */
      clearslots2(fs, b, exp->aux);
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
    exp->u.unop.needinnerparen = 0;
    /* B must be a register */
    if (!test_reg_property(fs, b, REG_LOCAL)) {
      exp->u.unop.b = -1;
      exp->u.unop.bindex = getslotdesc(fs, b)->u.expindex;
    }
  }
  else if (exp->kind == EINDEXED || exp->kind == ESELF) {
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
  /* discharge stores now before pushing a new expression node */
  if (exp->kind != ESTORE && pc >= sa->pendingcond.target) {
    if (sa->laststore) {
      dischargestores2(sa,fs);
      if (fs->D->matchlineinfo == 0)
        /* use LINENUMBER, not NEXTLINENUMBER, because a statement has just
           ended and NEXTLINENUMBER has already advanced */
        exp->line = exp->closeparenline = fs->D->nextlinenumber;
    }
  }
  exp = newexp(fs);
  *exp = node;
  if (exp->kind != ESTORE)
    linkexp2(sa, fs, exp);
  else {
    exp->pending = 0;
    return updatelaststore2(sa, fs, exp);
  }
  if (exp->kind == ECALL)
    fs->lastcallexp = exp2index(fs, exp);  /* update last call node */
  return exp;
}


/*
** create a new ECONDITIONAL expression node from 2 operands, which may be any
** kind of expression; the 2 operands correspond to `a' and `b' in the binary
** operation `a [and/or] b'
*/
static ExpNode *
reducecondition2(StackAnalyzer *sa, DFuncState *fs, int e1, int e2, int reg,
                 int pc)
{
  ExpNode *exp;
  lu_byte goiftrue;
  {  /* compute the logical operator to use for this new conditional node */
    ExpNode *prev = index2exp(fs, e1);
    while (prev->kind == ECONDITIONAL)
      prev = index2exp(fs, prev->u.cond.e2);
    goiftrue = prev->goiftrue;
  }
  exp = newexp(fs);
  initexp2(fs, exp, reg, pc);
  exp->kind = ECONDITIONAL;
  exp->endlabel = sa->pendingcond.target;
  exp->u.cond.e1 = e1;
  exp->u.cond.e2 = e2;
  exp->u.cond.goiftrue = exp->goiftrue = goiftrue;
  linkexp2(sa, fs, exp);
  return exp;
}


/*
** create an expression node for a comparison operation
*/
static ExpNode *addcompare2(StackAnalyzer *sa, DFuncState *fs, int pc, int reg,
                            Instruction i, lu_byte inverted)
{
  OpCode o = GET_OPCODE(i);
  int a, b, c;
  OpCode comp = o - (o % 2);  /* remove BK bit */
  BinOpr op;
  ExpNode *exp;
  a = GETARG_A(i); b = GETARG_B(i); c = GETARG_C(i);
  exp = newexp(fs);
  initexp2(fs, exp, reg, pc);
  exp->kind = EBINOP;
  linkexp2(sa, fs, exp);
  if (inverted)
    a = !a;
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
  exp->u.binop.op = op;
  if (istempreg(fs, b)) {
    exp->u.binop.b = -1;
    exp->u.binop.bindex = getslotdesc(fs, b)->u.expindex;
  }
  else {
    exp->u.binop.b = b;
    exp->u.binop.bindex = 0;
  }
  if (istempreg(fs, c)) {
    exp->u.binop.c = -1;
    exp->u.binop.cindex = getslotdesc(fs, c)->u.expindex;
  }
  else {
    exp->u.binop.c = c;
    exp->u.binop.cindex = 0;
  }
  return exp;
}


static int isfailjump(DFuncState *fs, int pc)
{
  return (test_ins_property(fs, pc, INS_BRANCHFAIL) ||
          test_ins_property(fs, pc, INS_LOOPFAIL));
}

static int ispassjump(DFuncState *fs, int pc)
{
  return (test_ins_property(fs, pc, INS_BRANCHPASS) ||
          test_ins_property(fs, pc, INS_LOOPPASS));
}


static int iscondreducible(ExpNode *e1, ExpNode *e2, int golabel)
{
  lua_assert(e2 != NULL);
  if (e1 == NULL)
    return 0;
  if (e1->endlabel == golabel)
    return 1;
  if (/*e1->goiftrue == e2->goiftrue && */e1->endlabel == e2->endlabel)
    return 1;
  return 0;
}


/*
** update the pending conditional expression after a new jump; only call when
** encountering a jump inside a conditional expression
*/
static void addconditionalnode2(StackAnalyzer *sa, DFuncState *fs, int pc,
                               int jumptarget)
{
  int targetbool = test_ins_property(fs, jumptarget, INS_BOOLLABEL);
  /* SKIPSBOOLLABEL is true for non-VJMP expressions, which skip over 2 bool
     labels with an uncinditional jump after */
  int skipsboollabel = (!test_ins_property(fs, pc, INS_BOOLLABEL) &&
                        test_ins_property(fs, pc+1, INS_BOOLLABEL) &&
                        jumptarget == pc+3);
  /* E is the index of EXP */
  int e, reg;
  int augment_dest = 0;
  lu_byte goiftrue;
  ExpNode *exp;
  const Instruction *jc = getjumpcontrol(fs, pc);
  if (jc != NULL) {
    OpCode o = GET_OPCODE(*jc);
    if (testAMode(o) == 0) {  /* a comparison */
      goiftrue = !GETARG_A(*jc);
      if (targetbool) {
        /* see which bool label is targeted; if false, then this is a go-if-true
           node, e.g. `a == b and 1', because the boolean result of the first
           node will be used only if it is false */
        goiftrue = !GETARG_B(fs->f->code[jumptarget]);
        reg = GETARG_A(fs->f->code[jumptarget]);
      }
      else {
        reg = -1;
        if (isfailjump(fs, pc))
          goiftrue = 1;
        else if (ispassjump(fs, pc))
          goiftrue = 0;
        else
          goiftrue = 1;
        if (GET_OPCODE(fs->f->code[jumptarget-1]) == OP_JMP) {
          const Instruction *nextjc = getjumpcontrol(fs, jumptarget-1);
          if (nextjc != NULL && testAMode(GET_OPCODE(*nextjc)))
            reg = GETARG_A(*nextjc);
        }
      }
      exp = addcompare2(sa, fs, pc-1, reg, *jc, goiftrue);
    }
    else {  /* (testAMode(o) != 0) */
      int src;
      int needinvert = 0;
      goiftrue = !GETARG_C(*jc);
      if (o == OP_TESTSET) {
        src = GETARG_B(*jc);
        reg = GETARG_A(*jc);
      }
      else {
        reg = src = GETARG_A(*jc);
        /* when a jump list node does not leave a value in the destination
           register, bool labels are created for that node, and it encodes the
           destination register and the go-if-true property; also, the bool
           labels indicate that the expression is cast to a bool, which is not
           known until now since OP_NOT followed by OP_TEST will be optimized by
           inverting OP_TEST and not emitting OP_NOT; an extra unary `not' node
           will be added here if needed */
        if (targetbool)
          /* in this case OP_NOT caused argC in OP_TEST to be inverted; the
             boolean value to jump to then tells you if going on true */
          needinvert = 1;
        else if (isfailjump(fs, pc))
          needinvert = !goiftrue;
        else if (ispassjump(fs, pc))
          needinvert = goiftrue;
      }
      if (!istempreg(fs, src)) {
        /* a local variable, either create ELOCAL or EUNOP if inverting it with
           OPR_NOT */
        exp = newexp(fs);
        if (reg == src)
          reg = targetbool ? GETARG_A(fs->f->code[jumptarget]) : -1;
        if (needinvert) {
          initexp2(fs, exp, reg, pc);
          exp->kind = EUNOP;
          exp->u.unop.b = src;
          exp->u.unop.bindex = 0;
          exp->u.unop.needinnerparen = 0;
          exp->u.unop.op = OPR_NOT;
        }
        else {
          /* this jump may not leave any value in a free register if, for
             example, it is jumping from a branch test; in that case, use the
             reserved register value of (-1) */
          if (reg == src)
            reg = -1;
          initexp2(fs, exp, reg, pc);
          exp->kind = ELOCAL;
          exp->aux = src;
        }
        linkexp2(sa, fs, exp);
      }
      else {  /* src is a temporary register */
        exp = getexpinreg2(fs, src);
        if (needinvert) {
          e = exp2index(fs, exp);
          exp = newexp(fs);
          initexp2(fs, exp, reg, pc);
          exp->kind = EUNOP;
          exp->u.unop.b = -1;
          exp->u.unop.bindex = e;
          exp->u.unop.needinnerparen = 0;
          exp->u.unop.op = OPR_NOT;
        }
        /* if OP_TESTSET, move the expression to the destination register */
        if (src != reg) {
          ExpNode *prev = index2exp(fs, sa->pendingcond.e);
          popexp2(fs, src);
          exp->info = reg;
          if (prev != NULL && prev->info == src) {
            popexp2(fs, prev->info);
            prev->info = reg;
          }
        }
      }
      if (needinvert)
        goiftrue = !goiftrue;
      augment_dest = (o == OP_TESTSET && jumptarget == pc+1);
    }
  }
  else if (skipsboollabel) {
    exp = index2exp(fs, sa->lastexpindex);
    goiftrue = exp->goiftrue;
    reg = exp->info;
  }
  else {  /* jc == NULL */
    int b;
    if (targetbool) {
      b = GETARG_B(fs->f->code[jumptarget]);
      reg = GETARG_A(fs->f->code[jumptarget]);
    }
    else {
      /* this is an unconditional jump which does not skip over bool labels and
         does not target bool labels; this is a necessary jump for a go-if-true
         directive; the operand is `false' because the jump is always taken */
      b = 0;
      reg = -1;
    }
    exp = addboolexp2(sa, fs, reg, pc, b);
    goiftrue = !b;
  }
  exp->goiftrue = goiftrue;
  if (targetbool)
    exp->endlabel = jumptarget + (GETARG_C(fs->f->code[jumptarget]) + 1);
  else
    exp->endlabel = jumptarget;
  e = exp2index(fs, exp);
  sa->pendingcond.target = exp->endlabel;
  {
    ExpNode *prevnode;
    int golabel = skipsboollabel ? jumptarget : pc+1;
    addnode:
    prevnode = index2exp(fs, sa->pendingcond.e);
    if (iscondreducible(prevnode, exp, golabel)) {
      if (exp->info == -1 && prevnode->info != -1)
        reg = exp->info = prevnode->info;
      /* combine the current pending node and this new node as the 2 operands of
         a logical operation */
      exp = reducecondition2(sa, fs, sa->pendingcond.e, e, reg, pc);
      prevnode = index2exp(fs, sa->pendingcond.e);
      exp->auxlistprev = prevnode->auxlistprev;
      e = exp2index(fs, exp);  /* update the expression index */
      sa->pendingcond.e = exp->auxlistprev;
      goto addnode;
    }
    else {
      exp->auxlistprev = sa->pendingcond.e;
    }
  }
  sa->pendingcond.e = e;
  if (augment_dest) {
    /* AUGMENT_DEST is true if this condition is part of an assignment where the
       last operand in the conditional expression is the destination variable,
       for example:
          local a;
          a = b or a;
       'a' itself does generate code, but it causes 'b' to be evluated on the
       free stack, with OP_TESTSET, followed by a jump to the next instruction
    */
    exp = newexp(fs);
    initexp2(fs, exp, reg, pc);
    exp->kind = ELOCAL;
    exp->aux = reg;
    exp->endlabel = jumptarget;
    exp->goiftrue = goiftrue;
    linkexp2(sa, fs, exp);
    e = exp2index(fs, exp);
    augment_dest = 0;
    goto addnode;
  }
  if (reg != -1) {
    /* udpate the pending expression in REG if needed */
    if (istempreg(fs, reg) && exp2index(fs, getexpinreg2(fs, reg)) != e) {
      pushexp2(fs, reg, exp, 0);
      if (fs->firstfree <= reg)
        fs->firstfree = reg+1;
    }
  }
  else
    sa->tempslot.u.expindex = e;
}


/*
** close the pending conditional expression; call this once the end-label for
** the expression has been reached
*/
static ExpNode *
finalizeconditionalnode2(StackAnalyzer *sa, DFuncState *fs, int pc)
{
  int prevtarget;
  int e = sa->pendingcond.e;
  ExpNode *exp1 = index2exp(fs, e);
  ExpNode *exp2;
  lua_assert(exp1 != NULL);
  sa->pendingcond.e = exp1->auxlistprev;
  if (sa->pendingcond.e)
    prevtarget = index2exp(fs, sa->pendingcond.e)->endlabel;
  else
    prevtarget = -1;
  if (exp1->info != -1) {
    if (istempreg(fs, exp1->info))
      exp2 = getexpinreg2(fs, exp1->info);
    else {
      exp2 = index2exp(fs, sa->laststore);
      if (exp2) sa->laststore = exp2->previndex;
    }
  }
  else
    exp2 = index2exp(fs, sa->tempslot.u.expindex);
  /* do final reduction on the conditional node if needed */
  if (exp2 != NULL && exp2 != exp1) {
    exp1 = reducecondition2(sa, fs, e, exp2index(fs, exp2), exp1->info, pc);
    e = exp2index(fs, exp1);
    if (isregvalid(fs, exp1->info))
      exp1 = addexptoreg2(sa, fs, exp1->info, exp1, NULL);
    else {
      exp1->previndex = sa->tempslot.u.expindex;
      sa->tempslot.u.expindex = e;
    }
  }
  else {
    if (isregvalid(fs, exp1->info) && !istempreg(fs, exp1->info))
      exp1 = addexptoreg2(sa, fs, exp1->info, exp1, NULL);
  }
  sa->pendingcond.target = prevtarget;
  return exp1;
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
      if (!isfieldname(rawtsvalue(&fs->f->k[b])))
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
  exp->line = getline2(fs, pc);
  exp->closeparenline = exp->line;
  exp->previndex = exp->auxlistprev = exp->auxlistnext = 0;
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
      if (!isfieldname(rawtsvalue(&fs->f->k[b])))
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
  exp->line = getline2(fs, pc);
  if (!istempreg(fs, srcreg))
    exp->aux = 0;
  else {
    ExpNode *srcexp = getexpinreg2(fs, srcreg);
    if (srcexp && srcexp->kind == ECLOSURE)
      exp->line = 0;
    exp->aux = exp2index(fs, srcexp);
  }
  exp->closeparenline = exp->line;
  exp->previndex = exp->auxlistprev = exp->auxlistnext = 0;
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
    if (pc == sa->pendingcond.target) {
       finalizeconditionalnode2(sa, fs, pc);
    }
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
          ExpNode *last = index2exp(fs, tab->u.cons.lasthashitem);
          int e = exp2index(fs, exp);
          if (last) {
            lua_assert(last->kind == ESTORE);
            last->auxlistnext = e;
          }
          else {
            tab->u.cons.firsthashitem = e;
          }
          tab->u.cons.lasthashitem = e;
          {
            /* check if the key and/or the source expression are pushed to the
               stack, i.e. are temporary values; key is pushed first, then the
               source, so if key is not a temporary, than the source expression
               is the lowest free register if it is a temporary */
            int reg = exp->u.store.aux2;
            if (!istempreg(fs, reg))
              reg = exp->u.store.srcreg;
            else {
              /* use AUXLISTPREV to hold the index of the pending key expression
              */
              exp->auxlistprev = exp2index(fs, getexpinreg2(fs, b));
            }
            if (istempreg(fs, reg)) {
              setfirstfree(fs, reg);
              /* if the last used reg holds the current table constructor, then
                 reset its firstarrayitem as it now points to a free register */
              if (reg - 1 == a) {
                tab->u.cons.firstarrayitem = 0;
              }
            }
          }
          if (exp->aux) {
            ExpNode *value = index2exp(fs, exp->aux);
            int line = getexpline(value);
            if (line != exp->line && line != 0)
              value->closeparenline = exp->line;
          }
          tab->u.cons.nhash++;
          sa->lastexpindex = exp2index(fs, tab);
        }
      }
    }
    else if (o == OP_JMP) {
      int target = pc+1+sbx;
      addconditionalnode2(sa, fs, pc, target);
    }
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


static ExpNode *getforloopbasenode2(StackAnalyzer *sa, DFuncState *fs,
                                    BlockNode *node, int base, int bias)
{
  ExpNode *exp = getexpinreg2(fs, base);
  if (exp == NULL) {
    exp = addnilexp2(sa, fs, base, node->startpc - (node->startpc > 0));
    exp->aux = base+2+bias;
    addexptoreg2(sa, fs, base, exp, NULL);
    return checkexpinreg2(fs, base);
  }
  return exp;
}


static void
dumpfornumheader2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  DecompState *D = fs->D;
  struct ExpListIterator iter;
  struct HoldItem initvar, initeq;
  struct HoldItem *loopheader = sa->currheader;
  struct LocVar *var;
  /* get the base control register */
  const int base = GETARG_A(fs->f->code[node->endpc]);
  /* get the pending initial expression before creating the control variables */
  ExpNode *exp = getforloopbasenode2(sa, fs, node, base, 0);
  /* create the control variables */
  commitcontrolvars2(fs, base, 3);
  /* create the loop variable */
  pushlocalvars2(fs, base+3, 1);
  var = getlocvar2(fs, base+3);
  /* add hold items for for-loop header */
  addliteralholditem2(D, loopheader, "for", 1);
  addtsholditem2(D, &initvar, var->varname, 1);
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
  /*exp = index2exp(fs, exp->auxlistnext);*/
  dumpexp2(D, fs, exp, 0);
  DumpLiteral(" do", D);
  D->needspace = 1;
  flushpendingexp2(fs);
}


static void
dumpforlistheader2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  struct ExpListIterator iter;
  struct HoldItem *header = sa->currheader;
  Mbuffer *b = &D->buff;
  /* get the base control register */
  const int base = GETARG_A(fs->f->code[node->endpc-1]);
  /* get the number of expressions to list */
  const int nexp = (fs->firstfree - base) >= 3 ? (fs->firstfree - base) : 3;
  const int bias = nexp - 3;
  const int lastvar = base + 3 + sa->numforloopvars - 1;
  /* get the pending initial expression before creating the control variables */
  ExpNode *exp = getforloopbasenode2(sa, fs, node, base, bias);
  int i, lastreg = base + nexp - 1;
  /* create the control variables */
  commitcontrolvars2(fs, base, 3);
  /* create the loop variables */
  pushlocalvars2(fs, base+3, sa->numforloopvars);
  /* create the for-loop header string */
  luaZ_resetbuffer(b);
  addliteral2buff(H, b, "for ");
  for (i = base + 3; i <= lastvar; i++) {
    size_t len;
    struct LocVar *var = getlocvar2(fs, i);
    lua_assert(var->varname != NULL);
    len = var->varname->tsv.len;
    addstr2buff(H, b, getstr(var->varname), len);
    if (i != lastvar)
      addliteral2buff(H, b, ", ");
  }
  addliteral2buff(H, b, " in ");
  addholditem2(D, header, luaZ_buffer(b), luaZ_bufflen(b), 0);
  /* dump expression list */
  initexplistiter2(&iter, fs, base, exp);
  dumpexp2(D, fs, exp, 0);
  for (i = 1; i < nexp && !hasmultretuptoreg(exp, lastreg); i++) {
    DumpComma(D);
    exp = getnextexpinlist2(&iter, i);
    dumpexp2(D, fs, exp, 0);
  }
  DumpLiteral(" do", D);
  D->needspace = 1;
  flushpendingexp2(fs);
  sa->forloopbias = bias;
}


/*
** get an appropriate line for the given PC
*/
static int getvalidline2(DecompState *D, DFuncState *fs, int pc)
{
  int line = getline2(fs, pc);
  /* OP_CLOSURE is mapped to the final line of the nested function and is not
     appropriate to use as an estimation of how much room exists until PC */
  if (GET_OPCODE(fs->f->code[pc]) == OP_CLOSURE) {
    const Proto *f;
    int i, bx;
    /* two line numbers are computed that can possibly give a valid if not
       correct line to dump the header on; the first is the next instruction
       after OP_CLOSURE; if it is a store code and it is storing the closure
       directly to a global variable, upvalue, or table index the line-mapping
       may be the line of the header if the function is named; the second line
       is computed from the first line-mapping in the closure refernced by the
       OP_CLOSURE code; the analyzer will traverse the list of child functions
       until the first code of a function is not OP_CLOSURE */
    int line1 = 0, line2 = 0;
    int reg = GETARG_A(fs->f->code[pc]);
    /* see how this closure is being stored; if being assigned, the line-mapping
       for the store code may give the correct line for the function header, but
       only if the function is named, i.e. `function g()' intead of
       `g = function()'; if there is no store code, as is the case when the
       function is a local variable, or the closure is the RHS of a local
       definition, and then the local is stored to another variable, the
       line-mapping does not give correct line fo the header */
    if (fs->nlocvars >= fs->sizelocvars ||
        fs->locvars[fs->nlocvars].startpc > pc+1) {
      OpCode o = GET_OPCODE(fs->f->code[pc+1]);
      if ((o == OP_SETGLOBAL || o == OP_SETUPVAL || o == OP_SETUPVAL_R1 ||
           IS_OP_SETTABLE(o)) && GETARG_A(fs->f->code[pc+1]) == reg) {
        line2 = getline2(fs, pc+1);
        if (line2 >= line) line2 = 0;
      }
    }
    i = 0;
    f = fs->f;
    bx = GETARG_Bx(f->code[pc]);
    do {
      f = f->p[bx];
      bx = GETARG_Bx(f->code[0]);
      i++;
      line1 = f->lineinfo[0];
    } while (GET_OPCODE(f->code[0]) == OP_CLOSURE);
    line1 -= i;
    if ((line2 != 0) && line2 < line1)
      line1 = line2;
    if (line1 < D->linenumber)
      return D->linenumber;
    return line1;
  }
  return line;
}


static void
updateheaderline2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  DecompState *D = fs->D;
  int line = D->linenumber;
  int nextline;
  if (D->matchlineinfo) {
    if (node->kind == BL_FUNCTION && issinglelinefunc(fs, node)) {
      updateline2(fs, getline(fs->f, 0), D);
      return;
    }
    else
      nextline = getvalidline2(D, fs, node->startpc);
  }
  else
    nextline = D->nextlinenumber + sa->numblockstartatpc;
  if (nextline - sa->numblockstartatpc > line)
    line = nextline - sa->numblockstartatpc;
  updateline2(fs, line, D);
}


static void
dumpbranchheader2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  DecompState *D = fs->D;
  int iselseif;
  lua_assert(sa->currparent != NULL);
  if (sa->currparent->kind == BL_ELSE && node == sa->currparent->firstchild) {
    int endpc;
    if (haselsepart(node) && node->nextsibling->nextsibling == NULL)
      endpc = node->nextsibling->endpc;
    else if (node->nextsibling == NULL)
      endpc = node->endpc;
    else
      endpc = -1;
    iselseif = (endpc == sa->currparent->endpc && fs->seenstatinblock == 0);
  }
  else
    iselseif = 0;
  if (iselseif) {
    node->iselseif = 1;
    if (haselsepart(node))
      node->nextsibling->iselseif = 1;
    D->indentlevel--;
  }
  if (sa->pendingcond.e) {
    ExpNode *exp;
    if (iselseif) {
      DumpLiteral("if", D);  /* tag `if' onto `else' */
      D->needspace = 1;
    }
    else
      addliteralholditem2(D, sa->currheader, "if", 1);
    exp = finalizeconditionalnode2(sa, fs, sa->pc - (sa->pc > 0));
    dumpexp2(D, fs, exp, 0);
    if (exp->info == -1) {
      sa->tempslot.u.expindex = 0;
    }
  }
  else {
    if (iselseif) {
      D->needspace = 0;
    }
    else {
      updateheaderline2(sa, fs, node);
      CheckSpaceNeeded(D);
    }
    DumpLiteral("if true ", D);
  }
  CheckSpaceNeeded(D);
  DumpLiteral("then", D);
  D->needspace = 1;
  flushpendingexp2(fs);
}


static void
dumpwhilestatheader2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  DecompState *D = fs->D;
  int pclimit = getfirstindentedpc(fs, node);
  int startline;
  lua_assert(sa->pc == node->startpc);
  if (D->matchlineinfo) {
    /* in Havok Script, the final line is fixed to the start line; in regular
       Lua, you can just set this to zero and an appropriate line will be
       computed for the loop header */
    startline = getline(fs->f, node->endpc);
    if (startline > getline(fs->f, node->startpc))
      startline = 0;
  }
  else {
    startline = 0;
  }
  for (; sa->pc < pclimit; sa->pc++) {
    int pc = sa->pc;
    Instruction i; OpCode o; int a, b, c, bx;
    while (sa->nextopenexpr != NULL && sa->nextopenexpr->startpc == pc) {
      sa->currbl = node;
      openexpr2(sa, fs);
      pc = sa->pc;
    }
    i = sa->code[pc];
    o = GET_OPCODE(i);
    a = GETARG_A(i);
    b = GETARG_B(i);
    c = GETARG_C(i);
    bx = GETARG_Bx(i);
    visitinsn2(fs, node, pc, i);
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
    else if (o == OP_JMP) {
      int target = pc+1+GETARG_sBx(i);
      addconditionalnode2(sa, fs, pc, target);
    }
  }
  if (pclimit != node->startpc && sa->pendingcond.e) {
    struct HoldItem header;
    ExpNode *exp = finalizeconditionalnode2(sa, fs, sa->pc);
    addliteralholditem2(D, &header, "while", 1);
    if (startline != 0) {
      updateline2(fs, startline, D);
      dischargeholditems2(D);
    }
    dumpexp2(D, fs, exp, 0);
    if (exp->info == -1)
      sa->tempslot.u.expindex = 0;
    CheckSpaceNeeded(D);
  }
  else {
    if (startline != 0)
      updateline2(fs, startline, D);
    else
      updateheaderline2(sa, fs, node);
    CheckSpaceNeeded(D);
    DumpLiteral("while true ", D);
  }
  DumpLiteral("do", D);
  D->needspace = 1;
  flushpendingexp2(fs);
}


static void dumpheaderstring2(StackAnalyzer *sa, DFuncState *fs,
                              BlockNode *node, const char *str)
{
  DecompState *D = fs->D;
  updateheaderline2(sa, fs, node);
  CheckSpaceNeeded(D);
  DumpString(str, D);
  D->needspace = 1;
  D->nextlinenumber++;
}


static void dumpfuncheader2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  int i, haveself;
  struct HoldItem funcname;
  DecompState *D = fs->D;
  addliteralholditem2(D, sa->currheader, "function", 1);
  /* add function name if needed */
  if (D->lastcl.name != NULL) {
    addtsholditem2(D, &funcname, D->lastcl.name, 0);
  }
  if (D->lastcl.line == 0)
    updateheaderline2(sa, fs, node);
  else
    updateline2(fs, D->lastcl.line, D);
  dischargeholditems2(D);
  CheckSpaceNeeded(D);
  DumpLiteral("(", D);
  D->needspace = 0;
  haveself = D->lastcl.haveself != 0;
  for (i = haveself; i < fs->f->numparams; i++) {
    if (i != haveself) DumpComma(D);
    CheckSpaceNeeded(D);
    DumpTString(getlocvar2(fs, i)->varname, D);
    /* todo: emit type names for statically typed variables */
  }
  if (fs->f->is_vararg) {
    if (i != haveself) DumpComma(D);
    CheckSpaceNeeded(D);
    DumpLiteral("...", D);
  }
  DumpLiteral(")", D);
  D->needspace = 1;
  D->nextlinenumber++;
}


static void calcnumblockstartatpc2(StackAnalyzer *sa, BlockNode *node)
{
  int pc = sa->pc;
  int n = 1;
  while((node = node->firstchild) != NULL && node->startpc == pc)
    n++;
  sa->numblockstartatpc = n;
}


static void enterblock2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node,
                        struct HoldItem *blockheader)
{
  DecompState *D = fs->D;
  if (node->kind != BL_FUNCTION || fs->prev != NULL)
    lua_assert(D->indentlevel >= 0);
  sa->currheader = blockheader;
  if (sa->numblockstartatpc == -1) {
    calcnumblockstartatpc2(sa, node);
    if (fs->prev != NULL)
      D->nextlinenumber += sa->numblockstartatpc-1;
  }
  switch (node->kind) {
    case BL_FUNCTION: {
      int numvars, actualnumvars;
      if (fs->prev != NULL)
        dumpfuncheader2(sa, fs, node);
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
        if (getline2(fs, 0) > D->linenumber &&
            GET_OPCODE(fs->f->code[0]) != OP_CLOSURE)
          beginline2(fs, 1, D);
        emitlocalstat2(fs, numvars, 0);
        D->indentlevel--;
      }
      break;
    }
    case BL_WHILE:
      dumpwhilestatheader2(sa, fs, node);
      break;
    case BL_REPEAT:
    case BL_DO:
      dumpheaderstring2(sa, fs, node, node->kind == BL_REPEAT ? "repeat":"do");
      break;
    case BL_FORNUM:
      dumpfornumheader2(sa, fs, node);
      break;
    case BL_FORLIST:
      dumpforlistheader2(sa, fs, node);
      break;
    case BL_IF:
      dumpbranchheader2(sa, fs, node);
      break;
    default: break;
  }
  sa->numblockstartatpc--;
  if (sa->numblockstartatpc == 0)
    sa->numblockstartatpc = -1;
  sa->numforloopvars = 0;  /* discharge */
  sa->currheader = NULL;
  sa->currheaderline = D->linenumber;
}


static int calclastline2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node,
                         int inmainfunc)
{
  DecompState *D = fs->D;
  int line, nextline;
  lua_assert(ispcvalid(fs, node->endpc+1));
  if (D->linenumber == sa->currheaderline)
    return D->linenumber;
  nextline = getline2(fs, node->endpc+1);
  line = D->linenumber;
  if (line + (sa->deferleaveblock + 1) <= nextline) {
    if (inmainfunc && node->endpc+1 == fs->f->sizecode-1 &&
        sa->deferleaveblock < 1)
      line = nextline;
    else
      line = line + 1;
  }
  return line;
}


static int dumprepeatfooter2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  DecompState *D = fs->D;
  if (sa->pendingcond.e) {
    struct HoldItem until;  /* `until' */
    ExpNode *exp;
    addliteralholditem2(D, &until, "until", 1);
    exp = finalizeconditionalnode2(sa, fs, sa->pc);
    dumpexp2(D, fs, exp, 0);
    if (exp->info == -1)
      sa->tempslot.u.expindex = 0;
    DumpSemi(D);
    D->needspace = 1;
    return 1;
  }
  UNUSED(node);
  return 0;
}


static void leaveblock2(StackAnalyzer *sa, DFuncState *fs, BlockNode *node)
{
  const char *str;
  DecompState *D = fs->D;
  int inmainfunc = (fs->prev == NULL);
  if (inmainfunc && node->kind == BL_FUNCTION) {
    D->indentlevel = 0;
    /* no tokens needed to leave the main function, just add a line-feed */
    beginline2(fs, 1, D);
    return;
  }
  /* when leaving an elseif-block, do not dump anything unless this is an
     if-block which has an else part; in that case, dump `else'; otherwise,
     the parent block will dump the correct token when leaving */
  if (node->iselseif &&
      ((node->kind == BL_IF && !haselsepart(node)) || node->kind == BL_ELSE)) {
    D->indentlevel+= 1+(node->kind != BL_ELSE);
    return;
  }
  lua_assert(D->indentlevel >= 0);
  switch (node->kind) {
    case BL_REPEAT:
      if (dumprepeatfooter2(sa, fs, node))
        return;
      str = "until true;";
      goto advancetoendline;
    case BL_FUNCTION:
      str = "end";
      /* advance to the line of the final return */
      updateline2(fs, getline2(fs, node->endpc), D);
      break;
    default:
      str = (node->kind == BL_IF && haselsepart(node)) ? "else" : "end";
      advancetoendline:
      lua_assert(sa->currparent != NULL);
      if (D->matchlineinfo) {
        if (istailblock(sa->currparent, node)) {
          sa->deferleaveblock++;
          return;
        }
        else if (sa->deferleaveblock) {
          D->indentlevel += sa->deferleaveblock;
          do {
            updateline2(fs, calclastline2(sa, fs, node, inmainfunc), D);
            CheckSpaceNeeded(D);
            DumpLiteral("end", D);
            D->needspace = 1;
            D->indentlevel--;
          } while (--sa->deferleaveblock);
        }
        updateline2(fs, calclastline2(sa, fs, node, inmainfunc), D);
      }
      else {
        beginline2(fs, 1, D);
        D->nextlinenumber = D->linenumber;
      }
      break;
  }
  CheckSpaceNeeded(D);
  DumpString(str, D);
  D->needspace = 1;
}


static void addparams(DFuncState *fs, const Proto *f)
{
  fs->nlocvars = f->numparams;
  if (f->numparams)
    pushlocalvars2(fs, 0, f->numparams);
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
  /*int type = node->kind;*/
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
  fs->seenstatinblock = 0;
  D->indentlevel++;
  if (node->isempty) /* block has no instructions */
    goto loopfinished;
  /* main instruction loop */
  for (; sa->pc < sa->sizecode; sa->pc++) {
    int pc, a, b, c, bx, sbx;
    Instruction i;
    OpCode o;
    int numvars; /* number of variables which start at PC+1 */
    int actualnumvars;
    if (sa->pc == nextchildstartpc) {
      BlockNode *prevparent;
      int prevheaderline;
      /* save NEXTCHILD->ISEMPTY to use after updating NEXTCHILD */
      int waschildempty;
      processnextchild:
#ifdef HKSC_DECOMP_HAVE_PASS2
      if (nextchild->parentnilvars) {
        /* emit the local statement for the parent nil variables before entering
           the child block */
        fs->nlocvars += nextchild->parentnilvars;
        addlocalvars2(fs, nextchild->parentnilvars);
        fs->firstfree += nextchild->parentnilvars;
        if (D->matchlineinfo)
          updateline2(fs, getline(fs->f, sa->pc), D);
        emitlocalstat2(fs, nextchild->parentnilvars, 0);
      }
#endif /* HKSC_DECOMP_HAVE_PASS2 */
      prevparent = sa->currparent;
      prevheaderline = sa->currheaderline;
      sa->currparent = node;
      lua_assert(nextchild != NULL);
      waschildempty = nextchild->isempty;
      blnode2(sa, fs, nextchild);
      sa->currparent = prevparent;
      sa->currheaderline = prevheaderline;
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
      if (pc == sa->pendingcond.target) {
         finalizeconditionalnode2(sa, fs, pc);
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
          if (numvars > 0)
            emitlocalstat2(fs, numvars, pc);
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
    if (pc == sa->pendingcond.target) {
       finalizeconditionalnode2(sa, fs, pc);
    }
    if (testAMode(o)) { /* A is a register */
      ExpNode *exp;
      lua_assert(!test_ins_property(fs, pc, INS_BREAKSTAT));
      if (sa->pc == node->startpc && node->parentnilvars)
        a += node->parentnilvars;
      exp = addexp2(sa, fs, pc, o, a, b, c, bx);
      if (exp != NULL) {
        int splitnil;
        D(lprintf("created new expression node\n"));
        D(lprintf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(lprintf("---------------------------\n"));
        lua_assert(isregvalid(fs, exp->info));
        if (exp->kind != ESTORE)
          exp = addexptoreg2(sa, fs, a, exp, &splitnil);
        if (exp->kind == ECALL && exp->u.call.nret == 0)
          emitcallstat2(fs, exp);
        /* if the final return the next code and it is mapped to a different
           line than this expression, wrap this expression in parens and put
           the closing paren on the line that the return is mapped to; this
           preserves line info when recompiling */
        if (fs->prev == NULL && node->kind == BL_FUNCTION &&
            D->matchlineinfo && pc+1 == fs->f->sizecode-1) {
          int retline = getline2(fs, pc+1);
          int expline = getexpline(exp);
          if (retline != expline && expline != 0) {
            if (splitnil) {
              lua_assert(exp->kind == ENIL);
              exp->line = exp->closeparenline = retline;
            }
            else
              exp->closeparenline = retline;
          }
        }
        if (pc+1 == sa->pendingcond.target) {
           finalizeconditionalnode2(sa, fs, pc);
        }
        if (numvars > 0) {
          checkdischargestores2(sa, fs);
          D(lprintf("NEW LOCAL VARIABLE\n"));
          (void)getfirstexp;
          emitlocalstat2(fs, numvars, pc);
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
        if (numvars > 0)
          emitlocalstat2(fs, numvars, pc);
      }
    }
    else {
      if (o == OP_JMP) {
        int target = pc+1+sbx;
        if (test_ins_property(fs, pc, INS_BREAKSTAT)) {
          checkdischargestores2(sa, fs);
          emitbreakstat2(fs, pc);
        }
        else if (pc == endpc && node->kind != BL_REPEAT) {
          /* do nothing */
        }
        else if (sa->numforloopvars > 0) {
          /* do nothing */
          lua_assert(nextchild != NULL && isforloop(nextchild) &&
                     nextchildstartpc == pc+1);
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
#ifdef HKSC_DECOMP_HAVE_PASS2
  checkdischargestores2(sa, fs);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  D->indentlevel--;
#ifdef HKSC_DECOMP_HAVE_PASS2
  leaveblock2(sa, fs, node);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  debugleaveblock2(sa, fs, node);
  fs->nactvar = nactvar;
#ifdef HKSC_DECOMP_HAVE_PASS2
  flushpendingexp2(fs);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
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
  BlockNode *functionblock = fs->root;
  StackAnalyzer sa;
  sa.currparent = NULL;
  sa.deferleaveblock = 0;
  sa.inheadercondition = 0;
  sa.pc = 0;
  sa.code = f->code;
  sa.sizecode = f->sizecode;
  sa.maxstacksize = f->maxstacksize;
  sa.intailemptyblock = 0;
  sa.inheadercondition = 0;
  sa.lastexpindex = 0;
  sa.laststore = 0;
  sa.pendingcond.e = 0;
  sa.pendingcond.target = -1;
  sa.openexprkind = -1;
  sa.openexprnildebt = 0;
  sa.numforloopvars = 0;
  sa.whilestatbodystart = 0;
  sa.numblockstartatpc = -1;
  sa.currheaderline = 0;
  memset(&sa.tempslot, 0, sizeof(SlotDesc));
  fs->nactvar = 0;
  fs->nlocvars = 0;
  lua_assert(fs->firstfree == 0);
  lua_assert(functionblock != NULL);
  lua_assert(functionblock->kind == BL_FUNCTION);
  initpass2(fs);
#ifdef HKSC_DECOMP_HAVE_PASS2
  addparams(fs, f);
  updatenextopenexpr2(&sa, fs);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  blnode2(&sa, fs, functionblock);
  fs->D->nextlinenumber = fs->D->linenumber;
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
  pass1(&new_fs);
  debugpass1summary(&new_fs);
  pass2(f,&new_fs);
  close_func(D);
}


static void MarkGlobals(const Proto *f)
{
  int i;
  for (i = 0; i < f->sizecode; i++) {
    Instruction insn = f->code[i];
    OpCode o = GET_OPCODE(insn);
    if (o == OP_GETGLOBAL || o == OP_GETGLOBAL_MEM || o == OP_SETGLOBAL) {
      int bx = GETARG_Bx(insn);
      TString *name = rawtsvalue(&f->k[bx]);
      name->tsv.reserved = MARKED_GLOBAL;
    }
  }
}


static void prescan_enterfunc(DecompState *D)
{
  D->funcidx++;
  if (D->funcidx > D->maxtreedepth)
    D->maxtreedepth = D->funcidx;
}

static void prescan_leavefunc(DecompState *D)
{
  D->funcidx--;
}


/*
** when using debug info, just calculcate the maximum function tree depth
*/
static void prescan_withdebug(DecompState *D, const Proto *f)
{
  int i;
  prescan_enterfunc(D);
  for (i = 0; i < f->sizep; i++)
    prescan_withdebug(D, f->p[i]);
  prescan_leavefunc(D);
}


/*
** when not using debug info, mark global variable names referenced in the code
** and calculate the maximum function tree depth
*/
static void prescan_nodebug(DecompState *D, const Proto *f)
{
  int i;
  prescan_enterfunc(D);
  MarkGlobals(f);
  for (i = 0; i < f->sizep; i++)
    prescan_nodebug(D, f->p[i]);
  prescan_leavefunc(D);
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
  DecompState *D = sd->D;
  const Proto *f = sd->f;
  if (D->usedebuginfo)
    prescan_withdebug(D, f);
  else
    prescan_nodebug(D, f);
  lua_assert(D->funcidx == 0);
  DecompileFunction(D, f);
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
  D.rescan=0;
  D.funcidx=0;
  D.indentlevel=-1;
  D.linenumber=1;
  D.nextlinenumber=1;
  D.needspace=0;
  D.maxtreedepth=1;  /* at least a main function */
  VEC_INIT(D.loopstk);
  VEC_INIT(D.blockstk);
  VEC_INIT(D.freeblocknodes);
  VEC_INIT(D.varnotes);
  D.kmap = NULL;
  D.sizekmap = 0;
  D.holdfirst = NULL;
  D.holdlast = NULL;
  D.lastcl.name = NULL;
  D.lastcl.pc = -1;
  D.lastcl.line = D.lastcl.haveself = 0;
  D.usedebuginfo = (!Settings(H).ignore_debug && f->sizelineinfo > 0);
  D.matchlineinfo = (Settings(H).match_line_info && D.usedebuginfo);
  luaZ_initbuffer(H, &D.buff);
  luaZ_resetbuffer(&D.buff);
  sd.D=&D;
  sd.f=f;
  status = luaD_pcall(H, f_decompiler, &sd);
  luaZ_freebuffer(H, &D.buff);
  VEC_FREE(H, D.loopstk, LoopState);
  VEC_FREE(H, D.blockstk, BlockState);
  VEC_FREE(H, D.freeblocknodes, BlockNode *);
  VEC_FREE(H, D.varnotes, lu_byte);
  freekmap(&D);
  if (status) D.status = status;
  return D.status;
}

#endif /* HKSC_DECOMPILER */
