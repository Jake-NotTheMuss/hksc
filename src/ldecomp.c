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

#define FuncState CompilerFuncState

#include "hksclua.h"

#include "lbitmap.h"
#include "lcode.h"
#include "ldecomp.h"
#include "ldebug.h"
#include "ldo.h"
#include "llex.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#ifdef HKSC_VERSION
#include "lstruct.h"
#endif /* HKSC_VERSION */
#include "lundump.h"
#include "lvec.h"
#include "lzio.h"

/* end of includes */
#undef FuncState


#ifdef HKSC_DECOMPILER

#ifdef HKSC_DECOMP_DEBUG_PASS1
#undef HKSC_DECOMP_HAVE_PASS2
#else /* !HKSC_DECOMP_DEBUG_PASS1 */
#define HKSC_DECOMP_HAVE_PASS2
#endif /* HKSC_DECOMP_DEBUG_PASS1 */

#ifndef HKSC_VERSION
#define hksc_State lua_State
void luaO_printstring (const TString *ts,
                       void (*pfn) (const char *s, size_t l, void *ud),
                       void *ud, int quote);
void luaO_printk (const TValue *o,
                  void (*pfn) (const char *s, size_t l, void *ud),
                  void *ud, int quote);
#endif /* HKSC_VERSION */

/*
** additional reserved values for TStrings to resolve conflicts between global
** names and generated local names (i.e. f0_local0)
*/
#define MARKED_GLOBAL cast_byte(NUM_RESERVED+1)
#define CONFLICTING_GLOBAL cast_byte(NUM_RESERVED+2)


enum DecompPhase {
  DECOMP_PHASE_INITIAL,
  DECOMP_PHASE_DETECT_LOOPS,
  DECOMP_PHASE_CHECK_LOAD_OPTIMIZATION,
  DECOMP_PHASE_PARSE_CONSTRUCTORS,
  DECOMP_PHASE_PARSE_ASSIGNMENTS,
  DECOMP_PHASE_PARSE_EXPRESSIONS,
  DECOMP_PHASE_SCAN_LEXICAL,
  /* number of decompiler phases */
  DECOMP_PHASE_COUNT,
  /* first bytecode scan phase */
  DECOMP_PHASE_FIRST_BYTECODE_SCAN = DECOMP_PHASE_DETECT_LOOPS
};

#define changephase(D,p) ((D)->phase = (p))
#define assertphase(D,p) lua_assert((D)->phase == (p));


/* for formatting decimal integers in generated variable names */
#define INT_CHAR_MAX_DEC (3 * sizeof(int) * (CHAR_BIT/8))

struct FuncState;  /* decompiler function state */

struct HoldItem;  /* used in pass2 to hold onto strings before dumping them */

/* `pass1' structs */
struct LoopState;
struct BlockState;

/*
** while OponExpr describes a section of code that must push values to the free
** stack space, StackExpr describes code which looks like an open expression,
** but may be clobbering local variables or free stack space
*/
typedef struct StackExpr {
  /* first and last pc of the expression */
  int startpc, endpc;
  /* if this expression has a jump, the offset (I use the offset to save some
     bits, as you only need SIZE_Bx-1 to represent an unsigned offset, and it
     is unsigned because conditional expressions only have forward jumps with
     non-zero offsets; a value of zero indicates no jump) */
  unsigned int jump : (SIZE_Bx-1);
  /* first register pushed to */
  unsigned int firstreg : SIZE_A;
  /* last register with a value yet to be used at the time of ENDPC */
  unsigned int lastreg : SIZE_A;
} StackExpr;


#ifndef HKSC_VERSION
#define HKSC_STRUCTURE_EXTENSION_ON 0
#endif /* HKSC_VERSION */


struct ConsControl {
#if HKSC_STRUCTURE_EXTENSION_ON
  const struct StructProto *proto;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  int startpc, endpc;
  int setlist;  /* pc of last SETLIST */
  int nhash;
};


typedef struct linemap {
  int pc, line;
} linemap;


/*
** DecompState - state of decompilation across all functions
*/
typedef struct {
  hksc_State *H;
  struct FuncState *fs;  /* current function state */
  struct FuncState *fs_stack;
  lua_Writer writer;
  void *data;
  const char *name;  /* input name */
  const Proto *mainfunc;
  enum DecompPhase phase;
  int status;
  int rescan;  /* true if rescanning code in the first pass */
  int usedebuginfo;  /* true if using debug info */
  int matchlineinfo;  /* true if matching statements to line info */
  int funcidx;  /* n for the nth function that is being decompiled */
  int indentlevel;  /* indentation level counter */
  int noindent;  /* used when dumping a long string, to prevent indenting
                    inside the string */
  int linenumber;  /* output line counter */
  int lastline;
  int nextlinenumber;  /* used when not using line info */
  int delaysemi;  /* used to delay the final semicolon to match line info with
                     the final return opcode when it is mapped to a different
                     line (0 = not delayed, -1 = already delayed, otherwise the
                     value is the line to dump the semicolon) */
  int needspace;  /* for adding space between tokens */
  int maxtreedepth;  /* maximum number of function states that will be active
                        at once during the decompilation */
  /* Pass 1 dynamic vectors - instances of these arrays to do not need to exist
     per-function, because the first pass is not recursive, i.e. each closure
     in the program is analyzed consecutively */
#define VEC_LIST \
  DEF_VEC(struct LoopState, loopstk) \
  DEF_VEC(int, blockstk) \
  DEF_VEC(int, varnotes) \
  DEF_VEC(StackExpr, stackexpr) \
  DEF_VEC(struct ConsControl, cons) \
  DEF_VEC(BlockNode, loopnodes) \
  DEF_VEC(int, cons_pc) \
  DEF_VEC(struct ConsControl, cons_state) \
  DEF_VEC(linemap, fixedstartlines)
#define DEF_VEC(T,n) VEC_DECL(T,n);
  VEC_LIST
#undef DEF_VEC
  /* vector for fixed start-lines for pc which have `fixed lines', that is,
     they correspond to lines in source code that are earlier than the line
     that they are mapped to in debug info */
  /*VEC_DECL(linemap, fixedstartlines);*/
  /* constants/upval bitmaps - each bit represents whether the corresponding
     constant or upval has been referenced in the current function - used in
     the first pass when generating variable info */
  Bitmap kmap, upvalmap;
  List blocknodes;
  struct ExpressionParser *parser;
  /* this structure is for per-function data that only needs to be instanced
     once at any time, because it is only needed in the initial passes where
     functions are processsed consecutively, not recursively */
  struct {
    int pendingbreak;  /* used in detectloops */
    struct BlockState *bl;  /* current block scope */
    /* prevnode is the lexical scope that was just left, nextnode is the next
       child scope to enter while scanning */
    BlockNode *prevnode, *nextnode;
    /* the current building StackExpr which will be moved to the stack once it
       has ended */
    BlockNode *currscope;
    StackExpr currexpr;
    /* the endpc of the OpenExpr within the current StackExpr if the OpenExpr
       starts at the same pc, or -1 */
    int currexpr_open;
    /* some bits to encode new upvalue and constant references in the current
       instruction while scanning, updated on each instruction */
    lu_int32 newref;
    struct { int pc, reg, savedstartpc; } lastdischarged;
    int minexprstartpc;  /* used in initial pass for openexpr1 */
    int currvarlimit;
    int nclose;
    int laststat;
    int testsetendlabel;  /* current TESTSET end label or -1 */
    /* if the current testset expression is assigning to an existing local,
       then this holds the slot of that local, otherwise it is -1 */
    int testsetlocvar;
    /* if the current testset expression is initializing a new local, then this
       holds the slot of that local, otherwise it is -1 */
    int testsetnewlocvar;
    /* the pc of the first OP_TESTSET in a testset expression */
    int testsetstart;
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
    lu_byte currexpr_expectedreg;
    /* these 3 bits indicate whether the constants `nil', `true', and `false'
       appear in the function constants table within the range MAXINDEXRK, and
       therefore will be encoded in a RK operand as a constant when used as an
       operand in arithmetic or table access */
    unsigned int isnilrk : 1, istruerk : 1, isfalserk : 1;
    unsigned int inassignment : 1;
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


/* a decompiled function */
typedef struct FuncState {
  struct FuncState *prev;  /* enclosing function */
  DecompState *D;  /* decompiler state */
  const Proto *f;  /* current function header */
  hksc_State *H;  /* copy of the Lua state */
  InstructionFlags *insproperties;  /* instruction flags */
  SlotDesc *regproperties;  /* register properties */
  unsigned short *actvar;
  VEC_DECL(ExpNode, expstack);
  int idx;  /* the nth function, used for generating local variable names */
  struct LocVar *locvars;  /* information about local variables */
  TString **upvalues;
  BlockNode *root;  /* the root lexical block of the function */
  short nlocvars;  /* number of local variables created so far */
  lu_byte nactvar;
  lu_byte seenstatinblock;  /* true if a simple statement has been encountered
                               in the current block node */
  int startlinemapbase;  /* local base of FIXEDSTARTLINES for this funcstate */
  int pc;
  int freereg;
  int inopenexpr;
  int sizelocvars;
  int sizeupvalues;
  int firstclob;  /* first pc that clobbers register A */
  int firstclobnonparam;  /* first pc that clobbers non-parameter register A */
  int firstfree;
  int lastcallexp;  /* exp index of last function call node */
  int curr_constructor;  /* exp index of current table constructor */
  unsigned int used : 1;  /* arrays need to be freed */
} FuncState;


#ifdef LUA_DEBUG
#define D(x) x

#undef printf
#define printf luaprintf

static hksc_State *luaprintf_state;

static int luaprintf (const char *fmt, ...) {
  va_list ap;
  int ret;
  void *logfile;
  lua_assert(luaprintf_state != NULL);
  logfile = G(luaprintf_state)->logfile;
  if (logfile == NULL)
    logfile = stdout;
  va_start(ap, fmt);
  ret = vfprintf(cast(FILE *, logfile), fmt, ap);
  va_end(ap);
  return ret;
}

/*****************************************************************************/
/* Debug print functions */
/*****************************************************************************/

static const char *blocktypename (int i) {
#define DEFBLTYPE(e)  #e,
  static const char *const blocktypenames [] = { BLTYPE_TABLE NULL };
#undef DEFBLTYPE
  return blocktypenames[i];
}


static void printinsflags (const FuncState *fs, int pc) {
#define DEFINSFLAG(e)  "INS_" #e,
  static const char *const insflagnames [] = { INSFLAG_TABLE NULL };
#undef DEFINSFLAG
  int i;
  printf("pc (%d):", pc+1);
  for (i = 0; i < MAX_INSFLAG; i++) {
    if (fs->insproperties[pc] & (1 << i))
      printf("  %s", insflagnames[i]);
  }
  printf("\n");
}

#else /* !LUA_DEBUG */
#define D(x) ((void)0)
#endif /* LUA_DEBUG */


#define CHECK(fs,c,msg) if (!(c)) fatalerror(fs,msg)

static void fatalerror (FuncState *fs, const char *msg) {
  const char *name = fs->D->name;
  luaD_setferror(fs->H, "%s: bad code in precompiled chunk: %s", name, msg);
  luaD_throw(fs->H, LUA_ERRSYNTAX);
}


static int ispcvalid (const FuncState *fs, int pc) {
  return (pc >= 0 && pc < fs->f->sizecode);
}


static int isregvalid (const FuncState *fs, int reg) {
  return (reg >= 0 && reg < fs->f->maxstacksize);
}


static int test_ins_property (const FuncState *fs, int pc, int prop) {
  lua_assert(ispcvalid(fs, pc));
  return ((fs->insproperties[pc] & (cast(lu_int32, 1) << prop)) != 0);
}


static void set_ins_property (FuncState *fs, int pc, int prop) {
  lua_assert(ispcvalid(fs, pc));
  fs->insproperties[pc] |= (cast(lu_int32, 1) << prop);
}


static void unset_ins_property (FuncState *fs, int pc, int prop) {
  lua_assert(ispcvalid(fs, pc));
  fs->insproperties[pc] &= ~(cast(lu_int32, 1) << prop);
}


#ifdef HKSC_DECOMP_HAVE_PASS2

static void set_reg_property (FuncState *fs, int reg, int prop) {
  lua_assert(isregvalid(fs, reg));
  fs->regproperties[reg].flags |= (1 << prop);
}


static void unset_reg_property (FuncState *fs, int reg, int prop) {
  lua_assert(isregvalid(fs, reg));
  fs->regproperties[reg].flags &= ~(1 << prop);
}

static int test_reg_property(const FuncState *fs, int reg, int prop) {
  lua_assert(isregvalid(fs, reg));
  return ((fs->regproperties[reg].flags & (1 << prop)) != 0);
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*****************************************************************************/
/* BlockNode functions */
/*****************************************************************************/


/*
** the startpc of a node or -1 if node is NULL
*/
#define NODE_STARTPC(node) ((node) ? (node)->startpc : -1)

/* empty blocks start 1 pc after they end; the startpc to use for comparisons
   is the emptiness subtracted from the startpc (consider a tail-empty child
   block which has an actual startpc that is greater than its parent block's
   endpc, or similarly, a previous empty sibling block that ends immediately
   before its next sibling block, which would have a startpc that is equal to
   its sibling block's startpc) */
#define blstartpc(bl)  \
  check_exp(bl, cast_int((bl)->startpc - nodegetflag(node, EMPTY)))

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
  node->parentnilvars = 0;
  nodesetflagval(node, EMPTY, endpc < startpc);
}


static int isloopnode(const BlockNode *node)
{
  int k = node->kind;
  return (k == BL_WHILE || k == BL_REPEAT || k == BL_FORNUM ||k == BL_FORLIST);
}


static void recalcemptiness(BlockNode *node) {
  nodesetflagval(node, EMPTY, node->endpc < node->startpc);
}


/*
** true if the if-block represented by NODE has en else-part
*/
static int haselsepart (const BlockNode *node) {
  lua_assert(node != NULL && node->kind == BL_IF);
  return (node->nextsibling != NULL && node->nextsibling->kind == BL_ELSE);
}


/*
** returns the pc where a natural OP_CLOSE would occur within NODE, natural
** meaning that the node alone generates the OP_CLOSE code, not an inner
** do-block
*/
static int getnaturalclosepc (const BlockNode *node) {
  switch (node->kind) {
    case BL_FORLIST: return node->endpc-2;
    case BL_IF: return node->endpc - haselsepart(node);
    case BL_ELSE: return node->endpc;
    default: return node->endpc-1;
  }
}


/*
** returns the natural endpc for local variables declared inside NODE
*/
static int getnaturalvarendpc (const BlockNode *node) {
  int pc;
  switch (node->kind) {
    /* repeat-loops without upvalues have variables end 1 after the endpc,
       whereas repeat-loops with upvalues have variables end on the OP_CLOSE,
       which is 1 before the endpc */
    case BL_REPEAT: return node->endpc+1 - 2*nodegetflag(node, UPVAL);
    case BL_FORLIST: pc = node->endpc-1; break;
    case BL_IF: pc = node->endpc+!haselsepart(node); break;
    /* variables in do-blocks that have upvalues end on the OP_CLOSE code,
       while variables in do-blocks that don't have upvalues end 1 after the
       last pc in the block */
    case BL_DO: pc = node->endpc+1; break;
    default: /* FUNCTION, WHILE, FORNUM, ELSE */ pc = node->endpc; break;
  }
  return pc - nodegetflag(node, UPVAL);
}


/*
** returns the first pc that would be in the indented part of a block in the
** source code
*/
static int getfirstindentedpc (const FuncState *fs, const BlockNode *node) {
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
    case BL_REPEAT: return nodegetflag(node, REPUNTILTRUE) ? node->endpc : -1;
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
  return pc - nodegetflag(node, UPVAL);
}


#ifdef HKSC_DECOMP_HAVE_PASS2
/*
** true if CHILD is a tail-block of PARENT, i.e. CHILD is the last statement in
** PARENT
*/
static int istailblock (const BlockNode *parent, const BlockNode *child) {
  int hasendcode;
  if (child->nextsibling != NULL)
    return 0;
  switch (parent->kind) {
    case BL_REPEAT: return 0;
    case BL_DO: case BL_ELSE: hasendcode = 0; break;
    case BL_IF: hasendcode = haselsepart(parent); break;
    case BL_FORLIST: hasendcode = 2; break;
    default: hasendcode = 1; break;
  }
  return (child->endpc + hasendcode == parent->endpc);
}

static int issinglelinefunc (const FuncState *fs, const BlockNode *func) {
  const Proto *f = fs->f;
  lua_assert(func->kind == BL_FUNCTION);
  if (fs->D->usedebuginfo == 0)
    return 0;
  return (getline(f, func->startpc) == getline(f, func->endpc));
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */

/* endpc of list loops is the final JMP, meaning subtract 1 to get the end-for
   code */
#define getendfor(node) ((node)->endpc - ((node)->kind != BL_FORNUM))

static int getforloopbase (const Instruction *code, const BlockNode *node) {
  int endfor = check_exp(isforloop(node), getendfor(node));
  return GETARG_A(code[endfor]);
}


static int getnumforloopvars (const Instruction *code, const BlockNode *node) {
  lua_assert(isforloop(node));
  return node->kind == BL_FORNUM ? 1 : GETARG_C(code[getendfor(node)]);
}


static SlotDesc *getslotdesc (const FuncState *fs, int reg) {
  SlotDesc *slot = check_exp(isregvalid(fs, reg), &fs->regproperties[reg]);
  return slot;
}


#ifdef HKSC_DECOMP_HAVE_PASS2


/*****************************************************************************/
/* ExpNode functions */
/*****************************************************************************/

static ExpNode *newexp (FuncState *fs) {
  hksc_State *H = fs->H;
  lua_assert(fs->expstack.used >= 0 && fs->expstack.used <=fs->expstack.alloc);
  return VEC_NEWELT(H, fs->expstack);
}


#define prevexp(fs,exp) index2exp(fs, exp->previndex)

static int exp2index (const FuncState *fs, const ExpNode *exp) {
  if (exp == NULL)
    return 0;
  else {
    lua_assert(exp >= fs->expstack.s &&
               exp < fs->expstack.s + fs->expstack.used);
    return exp-fs->expstack.s+1;
  }
}


static ExpNode *index2exp (const FuncState *fs, int index) {
  if (index == 0)
    return NULL;
  else
    return fs->expstack.s+(index-1);
}


#define checkfirstexp(fs) check_exp(getfirstexp(fs) != NULL, getfirstexp(fs))
#define checktopexp(fs) check_exp(gettopexp(fs) != NULL, gettopexp(fs))

static ExpNode *getfirstexp (const FuncState *fs) {
  int used = fs->expstack.used;
  if (used == 0)
    return NULL;
  else {
    lua_assert(used > 0);
    return &fs->expstack.s[0];
  }
}


static ExpNode *gettopexp (const FuncState *fs) {
  int used = fs->expstack.used;
  if (used == 0)
    return NULL;
  else {
    lua_assert(used > 0);
    return &fs->expstack.s[used-1];
  }
}


static int hasmultret (const ExpNode *exp) {
  return (exp->kind == ECALL || exp->kind == EVARARG);
}


static int ishashtable (const ExpNode *exp) {
  lua_assert(exp->kind == ECONSTRUCTOR);
  return (exp->u.cons.narray == 0 &&  exp->u.cons.nhash != 0);
}


static int getexpline (const ExpNode *exp) {
  int line = exp->line;
  if (exp->kind == ECONSTRUCTOR && exp->aux > 0)
    line = exp->aux;  /* use line-mapping of OP_SETLIST or final hash item */
  return line;
}


static int gethighestexpreg (const ExpNode *exp) {
  int numextraregs = 0;
  if (hasmultret(exp))
    numextraregs = exp->kind == ECALL ? exp->u.call.nret-1 : exp->aux-2;
  return exp->info + numextraregs;
}


/*
** check if an expression returns multiple values REG is the last one clobbered
*/
static int hasmultretuptoreg (const ExpNode *exp, int reg) {
  if (hasmultret(exp))
    return gethighestexpreg(exp) == reg;
  return 0;
}


/*
** returns true if EXP is a multret expression but only uses a single value
*/
static int hasmultretsinglereg (const ExpNode *exp) {
  lua_assert(exp != NULL);
  if (hasmultret(exp)) {
    if (exp->kind == ECALL)
      return exp->u.call.nret == 1;
    else if (exp->kind == EVARARG)
      return exp->aux == 2;
  }
  return 0;
}


#ifdef LUA_DEBUG
static const char *getunoprstring (UnOpr op);
static const char *getbinoprstring (BinOpr op);

static void pfn_printf (const char *s, size_t l, void *ud) {
  printf("%.*s", cast_int(l), s);
  UNUSED(ud);
}

static void debugexp (const FuncState *fs, const ExpNode *exp, int indent) {
  if (exp == NULL) {
    printf("(ExpNode *)0\n");
    return;
  }
  printf("exp: %d", exp->info);
  if ((exp->kind == ENIL || exp->kind == EVARARG) && exp->aux != exp->info)
    printf("-%d", exp->aux);
  printf(" <- ");
  switch (exp->kind) {
    case EVOID:    printf("(void)"); break;
    case ENIL:     printf("'nil'"); break;
    case ETRUE:    printf("'true'"); break;
    case EFALSE:   printf("'false'"); break;
    case EVARARG:  printf("'...'"); break;
    case ELITERAL:
      luaO_printk(exp->u.k, pfn_printf, NULL, '"');
      break;
    case ECALL:
      printf("[CALL %d,  %d ret, %d arg]", exp->info, exp->u.call.nret,
              exp->u.call.narg);
      break;
    case ECONCAT:
      /*printf("[CONCAT %d..%d]",index2exp(fs, exp->u.concat.firstindex)->info,
              index2exp(fs, exp->u.concat.lastindex)->info);*/
      break;
    case EGLOBAL: printf("_G.%s", getstr(exp->u.name)); break;
    case EUPVAL: printf("upval=%s", getstr(exp->u.name)); break;
    case EBINOP:
      printf("[BINOP %s  %d, %d]", getbinoprstring(exp->u.binop.op),
              exp->u.binop.b, exp->u.binop.c);
      break;
    case EUNOP:
      printf("[UNOP %s  %d]", getunoprstring(exp->u.unop.op),
              exp->u.unop.b);
      break;
    case ECONSTRUCTOR:
      printf("'{}' %d, %d", exp->u.cons.arrsize, exp->u.cons.hashsize);
      break;
    case ESTORE:
      printf("STORE (from %d)", exp->u.store.srcreg);
      break;
    case ECONDITIONAL: {
      int i;
      printf("[CONDITIONAL '%s']\n", getbinoprstring(exp->u.cond.goiftrue ?
                                                      OPR_AND : OPR_OR));
      for (i=0;i<=indent;i++)
        printf("  ");
      printf("- ");
      debugexp(fs, index2exp(fs, exp->u.cond.e1), indent+1);
      for (i=0;i<=indent;i++)
        printf("  ");
      printf("- ");
      debugexp(fs, index2exp(fs, exp->u.cond.e2), indent+1);
      return;
    }
    default: break;
  }
  printf("\n");
  if (prevexp(fs,exp) == NULL)
    return;
  indent++;
  {
    int i;
    for (i = 0; i < indent; i++)
      printf("  ");
  }
  printf("- ");
  debugexp(fs, prevexp(fs,exp), indent);
}
#else /* LUA_DEBUG */
#define debugexp(fs,exp,indent) ((void)0)
#endif /* LUA_DEBUG */


/*
** get the pc of the last encountered closure in the function FS
*/
static int getlastclosurepc (const DecompState *D, const FuncState *fs) {
  int lastclpc = D->lastcl.pc;
  lua_assert(ispcvalid(fs, lastclpc));
  lua_assert(GET_OPCODE(fs->f->code[lastclpc]) == OP_CLOSURE);
  return lastclpc;
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */

enum {
  UPVAL_FROM_STACK = 1,  /* from the current stack frame */
  UPVAL_FROM_UPVAL = 2  /* from the upvalue array */
};

#ifdef HKSC_DECOMP_HAVE_PASS2

/*
** stores the upval index in UPVAL, which is to be interpreted based on whether
** TYPE is set to UPVAL_FROM_STACK or UPVAL_FROM_UPVAL
*/
static int isloadupval (const FuncState *fs, int pc, int *upval, int *type) {
  Instruction i = fs->f->code[pc];
  OpCode o = GET_OPCODE(i);
#ifdef HKSC_VERSION
  if (o == OP_DATA) {
    *upval = GETARG_Bx(i);
    *type = (GETARG_A(i) < 2 ? UPVAL_FROM_STACK : UPVAL_FROM_UPVAL);
    return 1;
  }
#else  /* regular Lua */
  if (o == OP_MOVE) {
    *upval = GETARG_B(i), *type = UPVAL_FROM_STACK;
    return 1;
  }
  if (o == OP_GETUPVAL) {
    *upval = GETARG_Bx(i), *type = UPVAL_FROM_UPVAL;
    return 1;
  }
#endif /* HKSC_VERSION */
  return 0;
}


/*
** populates the `upvalues' array with the names of the upvalues from PARENT
** that FS uses (only call when not using debug info)
*/
static void initupvalues (FuncState *fs, const FuncState *parent) {
  DecompState *D = fs->D;
  int upval, type, nupn = fs->sizeupvalues;
  const int pc = getlastclosurepc(D, parent)+1;
  lua_assert(D->usedebuginfo == 0);
  while (--nupn >= 0 && isloadupval(parent, pc+nupn, &upval, &type)) {
    /* check which kind of upvalue is encoded */
    if (type == UPVAL_FROM_STACK) {
      /* argBx is a register */
      lua_assert(isregvalid(parent, upval));
      lua_assert(test_reg_property(parent, upval, REG_LOCAL));
      fs->upvalues[nupn] = getslotdesc(parent, upval)->u.locvar->varname;
    }
    else {
      /* argBx is an upvalue index in PARENT */
      lua_assert(upval < parent->sizeupvalues);
      fs->upvalues[nupn] = parent->upvalues[upval];
    }
  }
  CHECK(fs, nupn < 0, "not enough OP_DATA codes after OP_CLOSURE");
}


#define IS_RESERVED(ts) \
  ((ts)->tsv.reserved > 0 && (ts)->tsv.reserved < MARKED_GLOBAL)


/*
** returns true if NAME can be written as a field
*/
static int isfieldname (const TString *name) {
  const char *str = check_exp(name != NULL, getstr(name));
  size_t i;
  if (IS_RESERVED(name))
    return 0;
  if (!isalpha(*str) && *str != '_')
    return 0;
  for (i = 1; i < name->tsv.len; i++)
    if (!isalnum(str[i]) && str[i] != '_') return 0;
  return 1;
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


static int varisaugmented (const struct LocVar *var) {
  TString *name = var->varname;
  lua_assert(name != NULL);
  return (*getstr(name) == '(');
}


/*
** add a new entry for a fixed start-line LINE at PC
*/
static void addfixedstartline (FuncState *fs, int pc, int line) {
  DecompState *D = fs->D;
  linemap *map;
  if (test_ins_property(fs, pc, INS_FIXEDSTARTLINE))
    return;  /* already have an entry for PC */
  set_ins_property(fs, pc, INS_FIXEDSTARTLINE);
  map = VEC_NEWELT(D->H, D->fixedstartlines);
  map->pc = pc;
  map->line = line;
}


#ifdef HKSC_DECOMP_HAVE_PASS2
/*
** call this version to get the mapped line from debug info; okay to call if
** not using debug info or matching line info
*/
static int getline2 (const FuncState *fs, int pc) {
  lua_assert(ispcvalid(fs, pc));
  if (fs->D->matchlineinfo)
    return fs->f->lineinfo[pc];
  else
    return fs->D->nextlinenumber;
}


/*
** get the fixed start-line entry at PC; entry must exist (before calling,
** check if test_ins_property(fs, pc, INS_FIXEDSTARTLINE) is true)
*/
static int getfixedstartline (const FuncState *fs, int pc) {
  DecompState *D = fs->D;
  int n = D->fixedstartlines.used - fs->startlinemapbase;
  linemap *map = D->fixedstartlines.s + fs->startlinemapbase;
  lua_assert(n >= 0);
  lua_assert(test_ins_property(fs, pc, INS_FIXEDSTARTLINE));
  for (; n > 0; map++, n--) {
    if (map->pc == pc)
      return map->line;
  }
  lua_assert(0);
  return 0;
}


/*
** call this if you need the actual source code line that begins the expression
** at pc; this accounts for OP_CLOSURE line mapping to the end of the function
** rather than the beginning; only call if matching line info
*/
static int getstartline (const FuncState *fs, int pc) {
  lua_assert(fs->D->matchlineinfo);
  /* if PC is the start of a while-loop/for-loop, it has a fixed start-line */
  if (test_ins_property(fs, pc, INS_FIXEDSTARTLINE))
    return getfixedstartline(fs, pc);
  /* if the next code is CLOSURE, the mapped line will be the end of the
     function, but you want the start line, so return LINEDEFINED */
  if (GET_OPCODE(fs->f->code[pc]) == OP_CLOSURE) {
    int bx = GETARG_Bx(fs->f->code[pc]);
    const Proto *f = fs->f->p[bx];
    return f->linedefined;
  }
  return fs->f->lineinfo[pc];
}


/*
** return the fixed start-line of a for-loop given its NODE, or zero if its
** start line is not fixed (it should always be fixed in both Lua and Havok
** Script)
*/
static int getforloopstartline (const FuncState *fs, BlockNode *node) {
  int pc, startline;
  lua_assert(isforloop(node));
  lua_assert(fs->D->matchlineinfo);
  pc = node->endpc - (node->kind == BL_FORLIST);
  startline = getline(fs->f, pc);
  return nodegetflag(node, FIXEDSTARTLINE) ? startline : 0;
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


static void initregproperties (FuncState *fs) {
  int i;
  for (i = 0; i < fs->f->maxstacksize; i++) {
    SlotDesc *slot = fs->regproperties + i;
    slot->flags = 0;
#if HKSC_STRUCTURE_EXTENSION_ON
    slot->type = LUA_TNIL;
    slot->proto = NULL;
#endif
    slot->u.s.firstactive = 0;
    slot->u.s.aux = 0;
  }
}


static FuncState *open_func (DecompState *D, const Proto *f) {
  int i;
  hksc_State *H = D->H;
  FuncState *fs = D->fs_stack + D->funcidx;
  D->loopstk.used = D->blockstk.used = 0;
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
  fs->freereg = 0;
  /* initialize arrays before setting used to 1 */
  fs->insproperties = NULL;
  fs->regproperties = NULL;
  fs->locvars = NULL;
  fs->sizelocvars = 0;
  fs->upvalues = NULL;
  fs->sizeupvalues = 0;
  VEC_INIT(fs->expstack);
  fs->used = 1;
  fs->startlinemapbase = D->fixedstartlines.used;
  if (f->name)
    D(printf("-- Decompiling function (%d) named '%s'\n", D->funcidx,
             getstr(f->name)));
  else
    D(printf("-- Decompiling anonymous function (%d)\n", D->funcidx));
  if (D->usedebuginfo) { /* have debug info */
    D(printf("using debug info for function '%s'\n", f->name ? getstr(f->name):
             "(anonymous)"));
    fs->sizelocvars = f->sizelocvars;
    fs->locvars = f->locvars;
    fs->sizeupvalues = f->sizeupvalues;
    fs->upvalues = f->upvalues;
  }
  else {
    fs->upvalues = luaM_newvector(H, f->nups, TString *);
    fs->sizeupvalues = f->nups;
    for (i = 0; i < f->nups; i++)
      fs->upvalues[i] = NULL;
  }
  fs->actvar = luaM_newvector(H, f->maxstacksize, unsigned short);
  for (i = 0; i < f->maxstacksize; i++)
    fs->actvar[i] = 0;
  fs->firstclob = -1;
  fs->firstclobnonparam = -1;
  fs->firstfree = 0;
  fs->lastcallexp = 0;
  fs->curr_constructor = 0;
  /* allocate vectors for instruction and register properties */
  fs->insproperties = luaM_newvector(H, f->sizecode, InstructionFlags);
  for (i = 0; i < f->sizecode; i++)
    fs->insproperties[i] = 0;
  fs->regproperties = luaM_newvector(H, f->maxstacksize, SlotDesc);
  for (i = 0; i < f->maxstacksize; i++)
    fs->regproperties[i].flags = 0;
#ifdef HKSC_DECOMP_HAVE_PASS2
  if (D->usedebuginfo == 0 && fs->prev != NULL)
    initupvalues(fs, fs->prev);
#endif /* HKSC_DECOMP_HAVE_PASS2 */
  return fs;
}


static void free_funcstate (DecompState *D, FuncState *fs) {
  hksc_State *H = D->H;
  luaM_freearray(H, fs->insproperties, fs->f->sizecode, InstructionFlags);
  luaM_freearray(H, fs->regproperties, fs->f->maxstacksize, SlotDesc);
  luaM_freearray(H, fs->actvar, fs->f->maxstacksize, unsigned short);
  if (D->usedebuginfo == 0) {
    luaM_freearray(H, fs->locvars, fs->sizelocvars, struct LocVar);
    luaM_freearray(H, fs->upvalues, fs->sizeupvalues, TString *);
  }
  VEC_FREE(H, fs->expstack);
  fs->used = 0;
}

static void close_func (DecompState *D) {
  FuncState *fs = D->fs;
  lua_assert(fs->used);
  D->funcidx--;
  D->fs = fs->prev;
  D->fixedstartlines.used = fs->startlinemapbase;
  free_funcstate(D, fs);
}


static BlockNode *addblnode (FuncState *fs, int startpc, int endpc, int kind) {
  BlockNode *new_node = luaO_newnode(fs->H, &fs->D->blocknodes);
  initblnode(new_node, startpc, endpc, kind);
  return new_node;
}


static void freeblnode (FuncState *fs, BlockNode *node) {
  luaO_delnode(&fs->D->blocknodes, node);
}


/*****************************************************************************/
/* Dump functions used by the final pass */
/*****************************************************************************/

#define DumpLiteral(s,D) DumpBlock("" s, sizeof(s)-1, D)
#define DumpString(s,D) DumpBlock(s, strlen(s), D)

static void DumpBlock (const void *b, size_t size, DecompState *D) {
#if defined LUA_DEBUG && defined HKSC_DECOMP_HAVE_PASS2
  /* newline is acceptable when only dumping the newline and no other
     characters */
  if (*cast(char *, b) != '\n')
    lua_assert(memchr(b, '\n', size) == NULL);
  else {
    size_t i;
    for (i = 0; i < size; i ++)
      lua_assert(cast(char *, b)[i] == '\n');
  }
#endif /* LUA_DEBUG */
  if (D->status==0)
  {
    lua_unlock(D->H);
    D->status=(*D->writer)(D->H,b,size,D->data);
    D->lastline = D->linenumber;
    lua_lock(D->H);
  }
}


/*
** declarations for dump functions used by both passes
*/
static void DumpIndentation (DecompState *D);


#ifdef HKSC_DECOMP_HAVE_PASS2

/*
** dumps a TString data to output
*/
static void DumpTString (const TString *ts, DecompState *D) {
  lua_assert(ts != NULL);
  DumpBlock(getstr(ts), ts->tsv.len, D);
}


static void pfn_dumpobj (const char *s, size_t l, void *ud) {
  DumpBlock(s, l, ud);
}


/*
** prints a Lua object as it would appear in source code to output
*/
static void DumpTValue (const TValue *o, DecompState *D) {
  luaO_printk(o, pfn_dumpobj, D, '"');
}


/*
** dumps a constant indexed by INDEX from the constant table
*/
static void DumpConstant (FuncState *fs, int index, DecompState *D) {
  const Proto *f = fs->f;
  const TValue *o = &f->k[index];
  DumpTValue(o,D);
}

static void updateline2 (FuncState *fs, int line, DecompState *D);

/*
** dumps a semicolon to output
*/
static void DumpSemi (DecompState *D) {
  if (D->delaysemi > 0) {
    FuncState *fs = D->fs;
    if (fs->prev == NULL) {
      updateline2(fs, D->delaysemi, D);
      D->delaysemi = -1;
    }
  }
  DumpLiteral(";",D);
  D->needspace = 1;
}


/*
** dumps a comma to output
*/
static void DumpComma (DecompState *D) {
  DumpLiteral(",",D);
  D->needspace = 1;
}

/*
** dumps a space to output
*/
static void DumpSpace (DecompState *D) {
  DumpLiteral(" ",D);
  D->needspace = 0;
}


/*
** checks if a pending space is needed and discharges it
*/
static void CheckSpaceNeeded (DecompState *D) {
  if (D->needspace)
    DumpSpace(D);
}


/*
** dumps N linefeeds to output and updates the current line counter
*/
static void beginline2 (FuncState *fs, int n, DecompState *D) {
  static const char lf[] = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
  const int buffsize = cast_int(sizeof(lf)-1);
  /* save the lastline as you don't want to update it in this case */
  const int lastline = D->linenumber;
  D->linenumber += n;
  D(printf("adding %d line%s, updating D->linenumber to (%d)\n",
            n, n == 1 ? "" : "s", D->linenumber));
  lua_assert(n > 0);
  while (n > buffsize) {
    DumpBlock(lf, buffsize, D);
    n -= buffsize;
  }
  lua_assert(n >= 0 && n <= buffsize);
  if (n != 0)
    DumpBlock(lf, n, D);
  if (D->noindent == 0)
    DumpIndentation(D);
  D->lastline = lastline;  /* restore last line */
  D->needspace = 0;
  UNUSED(fs);
}


/*
** updates the line counter, dumping new lines to output if needed
*/
static void updateline2 (FuncState *fs, int line, DecompState *D) {
  if (line > D->linenumber) {
    int lines_needed = line - D->linenumber;
    beginline2(fs, lines_needed, D);
    lua_assert(D->linenumber == line);
  }
}


#if !defined(LUA_COMPAT_LSTR) || LUA_COMPAT_LSTR != 2
static size_t getlstrlevel (const TString *ts) {
  int inbracket = 0;
  size_t count = 0;
  size_t level = 0;
  const char *str = check_exp(ts, getstr(ts));
  size_t len = ts->tsv.len;
  for (; len; str++, len--) {
    if (inbracket) {
      if (*str == '=')
        count++;
      else {
        if (*str == ']') {
          /* end of end bracket */
          if (count+1 > level)
            level = count+1;
        }
        inbracket = 0;
        count = 0;
      }
    }
    else if (*str == ']')
      inbracket = 1;
  }
  return level;
}


static void dumplstrquote (DecompState *D, size_t level, const char *s) {
  lua_assert(*s == '[' || *s == ']');
  DumpBlock(s, 1, D);
  while (level--)
    DumpLiteral("=",D);
  DumpBlock(s, 1, D);
}
#else /* LUA_COMPAT_LSTR */
#define getlstrlevel(ts) 0  /* level 0 is always ok */
#define dumplstrquote(D,l,s) DumpLiteral(s, D)
#endif /* LUA_COMPAT_LSTR */



/*
** dumps a string constant as a long string `[[...]]'
*/
static void emitlongstring2 (ExpNode *exp, DecompState *D) {
  int endline = exp->line;  /* last line of the string */
  const TString *ts = rawtsvalue(exp->u.k);
  const size_t requiredlevel = getlstrlevel(ts);
  const char *str;
  size_t len;
  int numlinefeeds = 0;  /* number LF characters in the string */
  int numleadinglines;  /* number of lines the string starts with in source
                           (skipped by lexer) */
  (void)requiredlevel;  /* avoid warnings when `requiredlevel' is not used */
  lua_assert(D->matchlineinfo);
  lua_assert(ts != NULL);
  /* count number of new lines in the string */
  for (str = getstr(ts), len = ts->tsv.len; ; numlinefeeds++) {
    const char *s = memchr(str, '\n', len);
    if (s == NULL) break;
    len -= cast(size_t, s+1-str);
    str = s+1;
  }
  /* calculate number of leading lines that were skipped by lexer */
  lua_assert(endline >= D->linenumber);
  numleadinglines = (endline - D->linenumber) - numlinefeeds;
  /* NUMLEADINGLINES will only be negative if you have bad input */
  if (numleadinglines < 0)
    numleadinglines = 0;
  /* start emitting the string */
  dumplstrquote(D,requiredlevel,"[[");
  D->noindent = 1;
  if (numleadinglines)
    beginline2(D->fs, numleadinglines, D);
  str = getstr(ts);
  len = ts->tsv.len;
  for (;;) {
    /* use memchr instead of strchr in case there are embedded null bytes */
    const char *s = memchr(str, '\n', len);
    if (s != NULL) {
      int numlines;
      size_t blocksize = cast(size_t, s-str);
      DumpBlock(str, blocksize, D);
      str = s;
      len -= blocksize;
      /* advance over new lines */
      while (*++s == '\n')
        ;
      numlines = s-str;
      beginline2(D->fs, numlines, D);
      len -= cast(size_t, numlines);
      str = s;
    }
    else {
      DumpBlock(str, len, D);
      break;
    }
  }
  dumplstrquote(D,requiredlevel,"]]");
  D->noindent = 0;
}

#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*
** dumps indentation of to the current indentation level using tabs
*/
static void DumpIndentation (DecompState *D) {
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

static void debugblnode1 (BlockNode *node, int indent) {
  BlockNode *child, *nextsibling;
  int i;
  lua_assert(node != NULL);
  child = node->firstchild;
  nextsibling = node->nextsibling;
  for (i = 0; i < indent; i++)
    printf("  ");
  if (indent) printf("- ");
  printf("(%d-%d) %s ",node->startpc+1,node->endpc+1,
         blocktypename(node->kind));
  if (nextsibling != NULL)
    printf("(sibling (%d-%d) %s)\n",nextsibling->startpc+1,
           nextsibling->endpc+1, blocktypename(nextsibling->kind));
  else
    printf("(NO sibling)\n");
  lua_assert(nodegetflag(node, VISITED) == 0);
  nodesetflag(node, VISITED);
  while (child != NULL) {
    debugblnode1(child, indent+1);
    child = child->nextsibling;
  }
}


static void unvisittree (BlockNode *node) {
  BlockNode *child;
  nodeclearflag(node, VISITED);
  child = node->firstchild;
  while (child != NULL) {
    unvisittree(child);
    child = child->nextsibling;
  }
}


static void debugblnode (BlockNode *node) {
  debugblnode1(node, 0);
  unvisittree(node);
}


static void debugblocksummary (const FuncState *fs) {
  BlockNode *node = fs->root;
  printf("BLOCK SUMMARY\n"
         "-------------------\n");
  lua_assert(node != NULL);
  lua_assert(node->kind == BL_FUNCTION);
  lua_assert(node->nextsibling == NULL);
  debugblnode(node);
  printf("-------------------\n");
}


static void debugstackexpr (const StackExpr *e) {
  printf("%d-%d, reg %d-%d\n", e->startpc+1,e->endpc+1,e->firstreg,e->lastreg);
}


static void debugstackexprsummary (const FuncState *fs) {
  DecompState *D = fs->D;
  int i;
  printf("STACK EXPRESSION SUMMARY\n"
          "-------------------\n");
  for (i = 0; i < D->stackexpr.used; i++)
    debugstackexpr(&D->stackexpr.s[i]);
  printf("-------------------\n");
}


static void debugpass1summary (const FuncState *fs) {
  debugblocksummary(fs); printf("\n");
  debugstackexprsummary(fs); printf("\n");
  {
    int pc;
    for (pc = 0; pc < fs->f->sizecode; pc++)
      printinsflags(fs, pc);
  }
}


static void checktreevisited (const BlockNode *node) {
  const BlockNode *child;
  lua_assert(node != NULL);
  child = node->firstchild;
  lua_assert(nodegetflag(node, VISITED));
  while (child != NULL) {
    checktreevisited(child);
    child = child->nextsibling;
  }
}


#else /* !LUA_DEBUG */

#define debugpass1summary(fs)  ((void)(fs))
#define checktreevisited(node)  ((void)(node))

#endif /* LUA_DEBUG */

/*****************************************************************************/
/* Common functions */
/*****************************************************************************/


/* fetch the current instruction */
static void updateinsn (DecompState *D, FuncState *fs) {
  Instruction i = check_exp(ispcvalid(fs, fs->pc), fs->f->code[fs->pc]);
  D->a.insn.o = GET_OPCODE(i);
  D->a.insn.a = GETARG_A(i);
  D->a.insn.b = GETARG_B(i);
  D->a.insn.c = GETARG_C(i);
  D->a.insn.bx = GETARG_Bx(i);
  D->a.insn.sbx = GETARG_sBx(i);
}


/* get the next pc that is not OP_DATA */
static int getnextpc (const FuncState *fs, int pc) {
#ifdef HKSC_VERSION
  /* in Havok Script, skip OP_DATA */
  while (++pc < fs->f->sizecode && GET_OPCODE(fs->f->code[pc]) == OP_DATA)
    ;
#else
  /* in regular Lua, skip argC data after OP_NEWTABLE */
  if (GET_OPCODE(fs->f->code[pc]) == OP_NEWTABLE && !GETARG_C(fs->f->code[pc]))
    pc++;
  pc++;
#endif /* HKSC_VERSION */
  return pc;
}

/* get the previous pc that is not OP_DATA */
static int getprevpc (const FuncState *fs, int pc) {
#ifdef HKSC_VERSION
  /* in Havok Script, skip OP_DATA */
  while (--pc > 0 && GET_OPCODE(fs->f->code[pc]) == OP_DATA)
    ;
#else
  --pc;
  /* in regular Lua, skip argC data after OP_NEWTABLE */
  if (pc > 0 && GET_OPCODE(fs->f->code[pc-1]) == OP_NEWTABLE &&
      !GETARG_C(fs->f->code[pc-1]))
    --pc;
#endif /* HKSC_VERSION */
  return pc;
}


#ifdef HKSC_VERSION
static int getinsndata (FuncState *fs, int pc) {
  Instruction data = check_exp(ispcvalid(fs, pc), fs->f->code[pc]);
  CHECK(fs, GET_OPCODE(data) == OP_DATA, "expected OP_DATA");
  return GETARG_Bx(data);
}
#endif /* HKSC_VERSION */


/*
** Check whether an op loads a constant into register A. Helps determine if a
** tested expression starts evaluating at an instruction. Ops that load
** constants warrant their own statements, as otherwise the constant would be
** encoded as an RK operand directly in the instruction that uses it, unless
** there are more than MAXINDEXRK constants before this one
*/
#define opLoadsK(o) ((o) == OP_LOADK || (o) == OP_LOADBOOL ||(o) == OP_LOADNIL)

/*
** check if an OpArgMode value can encode constants
*/
#define iskmode(o)  ((o) == OpArgRK || (o) == OpArgK)


static int isunarycode (OpCode o) {
  return (o == OP_UNM || o == OP_NOT || o == OP_NOT_R1 || o == OP_LEN);
}


static int isconstructorcode (OpCode o) {
  return o == OP_NEWTABLE
#if HKSC_STRUCTURE_EXTENSION_ON
  || o == OP_NEWSTRUCT
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  ;
}


static int ischecktypecode (OpCode o) {
  return
#if HKSC_STRUCTURE_EXTENSION_ON
  o == OP_CHECKTYPE || o == OP_CHECKTYPES || o == OP_CHECKTYPE_D ||
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  0; (void)o;
}


static int isstorecode (OpCode o) {
  switch (o) {
    case CASE_OP_SETTABLE_LABEL: case OP_SETGLOBAL: case OP_SETUPVAL:
#ifdef HKSC_VERSION
    case OP_SETUPVAL_R1: case OP_SETSLOTN: case OP_SETSLOTI: case OP_SETSLOT:
    case OP_SETSLOTS: case OP_SETSLOTMT:
#endif /* HKSC_VERSION */
      return 1;
    default: return 0;
  }
}


/*
** return the source operand in a store instruction
*/
static int getstoresource (OpCode o, int a, int b, int c) {
  switch (o) {
    case CASE_OP_SETTABLE_LABEL:
#ifdef HKSC_VERSION
    case OP_SETSLOTI: case OP_SETSLOT: case OP_SETSLOTS: case OP_SETSLOTMT:
#endif /* HKSC_VERSION */
      return c;  /* a[b] := c */
#ifdef HKSC_VERSION
    case OP_SETSLOTN:
      return -1;  /* source is hardcoded as nil in the opcode */
#endif /* HKSC_VERSION */
    case OP_MOVE:
      return b;  /* a := b */
    default:
      return a;  /* Gbl/Upval := a */
  }
}


/*
** get the table and index registers of a table or struct assignment code,
** return 1 if only the table is a register, 2 if index is also a register
*/
static int getstoreindexregs (Instruction i, int *table, int *key) {
  OpCode o = GET_OPCODE(i);
  int a = GETARG_A(i), b = GETARG_B(i);
  if (IS_OP_SETTABLE(o)) {
    *table = a;
    if ((getBMode(o) == OpArgR || getBMode(o) == OpArgRK) && !ISK(b)) {
      *key = b;
      return 2;
    }
    return 1;
  }
  else if (IS_OP_SETSLOT(o)) {
    *table = a;
    return 1;
  }
  return 0;
}


/* check if an opcode is a load operation */
static int isload (OpCode o) {
  switch (o) {
    case OP_GETGLOBAL: case OP_LOADBOOL: case OP_LOADK: case OP_LOADNIL:
    case OP_GETUPVAL: case OP_NEWTABLE: case OP_CLOSURE: case OP_VARARG:
    case OP_MOVE: case OP_UNM: case OP_NOT: case OP_LEN: case OP_TESTSET:
    case OP_SELF: case OP_GETTABLE: case OP_CONCAT: case OP_ADD: case OP_SUB:
    case OP_MUL: case OP_DIV: case OP_MOD: case OP_POW:
#ifdef HKSC_VERSION
    /* havok script codes */
    case OP_GETGLOBAL_MEM: case OP_NEWSTRUCT: case OP_GETFIELD:
    case OP_GETFIELD_R1: case OP_NOT_R1: case OP_GETSLOT: case OP_GETSLOTMT:
    case OP_SELFSLOT: case OP_SELFSLOTMT: case OP_GETFIELD_MM:
    case OP_GETTABLE_S: case OP_GETTABLE_N: case OP_ADD_BK: case OP_SUB_BK:
    case OP_MUL_BK: case OP_DIV_BK: case OP_MOD_BK: case OP_POW_BK:
#endif /* HKSC_VERSION */
#ifdef LUA_CODT7
    case OP_LEFT_SHIFT: case OP_LEFT_SHIFT_BK:
    case OP_RIGHT_SHIFT: case OP_RIGHT_SHIFT_BK:
    case OP_BIT_AND: case OP_BIT_AND_BK:
    case OP_BIT_OR: case OP_BIT_OR_BK:
#endif /* LUA_CODT7 */
      return 1;
    default: return 0;
  }
}


/*
** get the operand constants for an instruction, returns the number of operands
*/
static int getkoperands (OpCode o, int b, int c, int bx, int operands[2]) {
  int n = 0;
  enum OpMode mode = getOpMode(o);
  if (mode == iABx) b = bx;
  else if (mode != iABC) return 0;
  /* see if B is a K index */
  switch (getBMode(o)) {
    case OpArgRK: if (!ISK(b)) break;
    /* fallthrough */
    case OpArgK: operands[n++] = INDEXK(b); break;
    default:;
  }
  /* see if C is a K index */
  switch (getCMode(o)) {
    case OpArgRK: if (!ISK(c)) break;
    /* fallthrough */
    case OpArgK: operands[n++] = INDEXK(c); break;
    default:;
  }
  return n;
}

typedef struct OperandDesc {
  int r;  /* register */
  enum OpArgMask mode;
} OperandDesc;


/*
** get the operand registers for an instruction, returns the number of operands
*/
static int getregoperands (OpCode o, int a, int b, int c, OperandDesc slots[3])
{
  int n = 0;
  enum OpArgMask bmode, cmode;
  if (isstorecode(o))
    slots[0].r = a, slots[0].mode = OpArgR, n = 1;
  switch (o) {
#ifdef HKSC_VERSION
#ifdef LUA_CODIW6
    case OP_DELETE: case OP_DELETE_BK:
      switch (c) {
        case DELETE_UPVAL: case DELETE_GLOBAL: return 0;
        case DELETE_LOCAL: slots[0].r = a; slots[0].mode = OpArgR; return 1;
        case DELETE_INDEXED:
          slots[0].r = a;
          slots[0].mode = OpArgR;
          if (!ISK(b)) {
            slots[n].r = b;
            slots[n++].mode = OpArgR;
          }
      }
      return n;
#endif /* LUA_CODIW6 */
    case OP_CHECKTYPE: case OP_CHECKTYPES: case OP_CHECKTYPE_D:
    case OP_TEST_R1:
#endif /* HKSC_VERSION */
    case OP_TEST:
      slots[0].r = a;
      slots[0].mode = OpArgR;
      return 1;
    case OP_RETURN:
      if (b == 1)
        return 0;
      else if (b == 2) {
        slots[0].r = a;
        slots[0].mode = OpArgR;
        return 1;
      }
      b--; /* fallthrough */
    case CASE_OP_CALL_LABEL:
      slots[0].r = a;
      slots[1].r = b > 0 ? a + b - 1 : -1;
      slots[0].mode = slots[1].mode = OpArgU;
      return -1;
    case OP_CONCAT:
      slots[0].r = b;
      slots[1].r = c;
      slots[0].mode = slots[1].mode = OpArgU;
      return -1;
    case OP_SETLIST:
      slots[0].r = a;
      slots[1].r = b > 0 ? a + b : -1;
      slots[0].mode = slots[1].mode = OpArgU;
      return -1;
    case OP_VARARG:
    case OP_LOADNIL:
      return 0;
    default: break;
  }
  switch (bmode = getBMode(o)) {
    case OpArgRK:
      if (ISK(b)) break;
      /* fallthrough */
    case OpArgR:
      slots[n].r = b;
      slots[n++].mode = bmode;
      break;
    default: break;
  }
  switch (cmode = getCMode(o)) {
    case OpArgRK:
      if (ISK(c)) break;
      /* fallthrough */
    case OpArgR:
      slots[n].r = c;
      slots[n++].mode = cmode;
      break;
    default: break;
  }
  return n;
}


static int getjump (const FuncState *fs, int pc) {
  int offset = GETARG_sBx(fs->f->code[pc]);
  return (pc+1)+offset;  /* turn offset into absolute position */
}


static const Instruction *getjumpcontrol (const FuncState *fs, int pc) {
  Instruction *pi = &fs->f->code[pc];
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
    return pi-1;
  else
    return NULL;
}


/*****************************************************************************/
/* First bytecode scan functions */
/*****************************************************************************/

static void firstscan_handleop (DecompState *D, FuncState *fs) {
  OpCode o = D->a.insn.o;
  int pc = fs->pc, a = D->a.insn.a, b = D->a.insn.b, c = D->a.insn.c;
  assertphase(D, DECOMP_PHASE_FIRST_BYTECODE_SCAN);
  switch (o) {
    case OP_JMP: {
      int offs = D->a.insn.sbx, label = pc + 1 + offs;
      const Instruction *jc = getjumpcontrol(fs, pc);
      /* check if this jump skips over a pair of bool labels */
      if (jc == NULL && offs >= 2 && test_ins_property(fs,pc+1,INS_BOOLLABEL))
        set_ins_property(fs, pc, INS_SKIPBOOLLABEL);
      /* if the jump is tested, the jump instruction is a leader */
      if (jc != NULL)
        set_ins_property(fs, pc, INS_LEADER);
      set_ins_property(fs, pc+1, INS_LEADER);
      set_ins_property(fs, label, INS_LEADER);
      break;
    }
    case OP_LOADBOOL:
      if (c) {
        set_ins_property(fs, pc, INS_BOOLLABEL);
        set_ins_property(fs, pc, INS_LEADER);
        set_ins_property(fs, pc+1, INS_BOOLLABEL);
        set_ins_property(fs, pc+1, INS_LEADER);
      }
      break;
    case OP_LOADNIL:
      if (GET_OPCODE(fs->f->code[pc+1]) == OP_LOADNIL) {
        int nextA = GETARG_A(fs->f->code[pc+1]);
        /* if these 2 nil codes could have been combined, the next pc must have
           been a possible leader when the code was generated, which indicates
           to the decompiler that there should be a basic block boundary
           emitted in the source that will be optimized away when compiled,
           for example, an if-true block:
              local a = nil;
              if true than -- falls through
                  local b = nil;
              end
            */
        if (nextA >= a && nextA <= b+1)
          set_ins_property(fs, pc+1, INS_NILLABEL);
      }
      break;
    case OP_CLOSURE: {
      int nupn, nup = fs->f->p[D->a.insn.bx]->nups;
      /* check if any of the upvalues are the closure itself */
      for (nupn = nup; nupn > 0; nupn--) {
        Instruction i = fs->f->code[pc+nupn];
        if (GETARG_A(i) == UPVAL_FROM_STACK && GETARG_Bx(i) == a)
          /* using its own register as an upvalue */
          set_ins_property(fs, pc, INS_SELFUPVAL);
      }
      break;
    }
    case OP_NEWTABLE:
#ifdef HKSC_VERSION
    case OP_NEWSTRUCT:
#endif
      if (c) {
        VEC_GROW(D->H, D->cons_pc);
        D->cons_pc.s[D->cons_pc.used++] = pc;
      }
      break;
    case OP_CLOSE:
      set_ins_property(fs, pc, INS_ENDSCOPE);
      break;
    default:;
  }
}

/*****************************************************************************/
/* Loop Detection phase - marks basic instruction flags and loop boundaries */
/*****************************************************************************/


/*
** return whether the line at PC was fixed to the start-line of a loop starting
** at TARGET
*/
static int isjumplinefixed (FuncState *fs, int pc, int target) {
  int pc_line = check_exp(ispcvalid(fs, pc), getline(fs->f, pc));
  int target_line = check_exp(ispcvalid(fs, target), getline(fs->f, target));
  if (pc_line != target_line)
    /* if the final jump maps to an earlier line than the start of the loop,
       the line was fixed */
    return pc_line < target_line;
  /* if PC_LINE and TARGET_LINE are the same, the line may be fixed; this can
     be confirmed if there is another line mapping inside the loop which
     maps to a line greater than PC_LINE */
  while (--pc > target)
    if (getline(fs->f, pc) > pc_line) return 1;
  return 0;  /* no evidence that the line was fixed */
}


typedef struct LoopState {
  int startlabel, endlabel;  /* start and end labels of the loop */
  /* breaklabel and exitlabel may vary between each other.
     This happens when there is a conditional expression after the loop whose
     first operand is a literal boolean value. For example:
        while a do
            break;
        end
        local a = false and 1;
     The break will target one of the bool labels generated by the expression,
     while the loop conditional will exit to the instruction after the bool
     labels. This is bug in Lua 5.1.4/Havok Script that was fixed in 5.1.5
     when Lua removed optimization for testing bool literals. */
  int breaklabel, exitlabel;
  unsigned int kind : 4;  /* 4 kinds of loops + BL_FUNCTION */
  unsigned int unsure : 1;
  unsigned int hasbreak : 2;  /* value of 1 means maybe, 2 means yes */
} LoopState;


static const LoopState dummyloop = {-1, -1, -1, -1, BL_FUNCTION, 0, 0};


static void setlooplabels (FuncState *fs, LoopState *loop) {
  int endlabel = check_exp(ispcvalid(fs, loop->endlabel), loop->endlabel);
  loop->breaklabel = loop->exitlabel = endlabel;
  if (GET_OPCODE(fs->f->code[endlabel]) == OP_JMP) {
    /* breaks will jump to the label targeted by the jump at ENDLABEL */
    int breaklabel = getjump(fs, endlabel);
    loop->breaklabel = loop->exitlabel = breaklabel;
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
** call this when encountering another break jump for a given loop
*/
static void setloophasbreak (FuncState *fs, const LoopState *loop, int pc) {
  LoopState *loop1 = cast(LoopState *, loop);
  if (loop == &dummyloop)
    return;
  if (loop1->unsure == 0)
    loop1->hasbreak = 2;
  else if (loop1->hasbreak == 0)
    loop1->hasbreak = 1, fs->D->a.pendingbreak = pc;
  if (loop1->hasbreak == 2)
    set_ins_property(fs, pc, INS_BREAKSTAT);
}


static LoopState *pushloopstate (FuncState *fs, int kind, int start, int end) {
  LoopState *loop = VEC_NEWELT(fs->H, fs->D->loopstk);
  loop->kind = kind;
  loop->startlabel = start;
  loop->endlabel = end;
  loop->unsure = 0;
  loop->hasbreak = 0;
  setlooplabels(fs, loop);
  fs->D->a.pendingbreak = -1;
  return loop;
}

#define poploopstate(fs)  popnloopstate(fs, 1)
static LoopState *popnloopstate (FuncState *fs, int n) {
  DecompState *D = fs->D;
  lua_assert(D->loopstk.used >= n);
  return &D->loopstk.s[(D->loopstk.used -= n)];
}


#define getcurrloop(fs)  getloopstate(fs, 1)
static const LoopState *getloopstate (FuncState *fs, int n) {
  DecompState *D = fs->D;
  lua_assert(n > 0);
  if (D->loopstk.used < n)
    return &dummyloop;
  return &D->loopstk.s[D->loopstk.used-n];
}


static void finalizeloopstate (FuncState *fs, BlockNode **pnextnode) {
  const LoopState *loop = getcurrloop(fs);
  int kind = loop->kind;
  int startpc = loop->startlabel;
  int endpc = loop->endlabel-1;
  int skip = (loop->unsure && loop->hasbreak < 2);
  BlockNode *new_node, *nextnode;
  poploopstate(fs);
  fs->D->a.pendingbreak = -1;
  if (skip)
    return;
  new_node = addblnode(fs, startpc, endpc, kind);
  /* find the next sibling for NEW_NODE from NEXTNODE, which may be a child
     block */
  nextnode = *pnextnode;
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
  set_ins_property(fs, endpc, INS_ENDSCOPE);
  set_ins_property(fs, startpc, INS_SEQPT);
  set_ins_property(fs, endpc+1, INS_SEQPT);
  if (kind == BL_FORLIST)
    set_ins_property(fs, endpc-1, INS_ENDSCOPE);
  if (kind == BL_REPEAT) {
    set_ins_property(fs, endpc, INS_FAILJUMP);
    set_ins_property(fs, endpc, INS_LOOPFAIL);
  }
  *pnextnode = new_node;
}


/*
** returns non-NULL if TARGET is any of the exit-labels of LOOP, returning the
** pointer to the first label that matches TARGET
*/
static const int *isloopexit (const LoopState *loop, int target) {
  if (target == loop->exitlabel)
    return &loop->exitlabel;
  else if (target == loop->breaklabel)
    return &loop->breaklabel;
  else if (target == loop->endlabel)
    return &loop->endlabel;
  return NULL;
}


static int isloopbreak (const LoopState *loop, int target) {
  const int *label = isloopexit(loop, target);
  return (label && *label == loop->breaklabel);
}


static void
calcloopunsure (FuncState *fs, LoopState *loop, const LoopState *outerloop) {
  int target = getjump(fs, loop->endlabel-1);
  /* check the target of the final jump in the loop; if it jumps to the start
     of the enclosing loop, the current loop may not exist, and may instead be
     be a jump that targets the final jump of the outer loop */
  if (target == outerloop->startlabel) {
    /* the following example highlights the necessity to mark `unsure' loops,
       for when the outer loop is either a while-loop or a repeat-loop with an
       unconditional jump (i.e. `until false')

          repeat
              local a = 1;
              if false then -- jumps to start
                  return;
              end
          until false;

       looks like

          repeat
              while true
                local a = 1;
              end -- jumps to start
              return;
          until false;
    */
    if (outerloop->kind == BL_WHILE) {
      if (fs->D->usedebuginfo)
        /* in Havok Script, while-loops have their final jump line mapping
           fixed to the start of the loop; this would indicate the presence of
           a loop; in regular Lua, isjumplinefixed() will return false as
           required */
        loop->unsure = !isjumplinefixed(fs, loop->endlabel-1, target);
      else
        loop->unsure = 1;
    }
    else if (outerloop->kind == BL_REPEAT &&
             getjumpcontrol(fs, outerloop->endlabel-1) == NULL)
      loop->unsure = 1;
  }
}


static int isbooljump (FuncState *fs, int pc, int label) {
  return (test_ins_property(fs, label, INS_BOOLLABEL) ||
          test_ins_property(fs, pc, INS_SKIPBOOLLABEL));
}


static void detectloops_onjump (DecompState *D, FuncState *fs) {
  int pc = fs->pc;
  int offs = D->a.insn.sbx;
  int target = pc+1+offs;
  const LoopState *currloop = getcurrloop(fs);
  int loopkind = -1;
  const Instruction *jc = getjumpcontrol(fs, pc);
  /* check for jumps that cannot be used to detect loops */
  /* if this jump interacts with bool labels */
  if (isbooljump(fs, pc, target))
    return;
  /* if this jump is controlled by TESTSET */
  if (jc != NULL && GET_OPCODE(*jc) == OP_TESTSET)
    return;
  /* if this jump is the first jump into a list forloop */
  if (offs >= 0 && GET_OPCODE(fs->f->code[target]) == OP_TFORLOOP)
    return;
  if (offs < 0 && jc != NULL) {
    if (GET_OPCODE(*jc) == OP_TFORLOOP)
      loopkind = BL_FORLIST;
    else {  /* possibly a repeat-loop */
      const int *label = isloopexit(currloop, target);
      if (label == NULL || *label != currloop->exitlabel)
        loopkind = BL_REPEAT;
    }
  }
  else {
    const LoopState *loop;
    int i;
    /* check if it is a break statement */
    if (jc == NULL && isloopbreak(currloop, target)) {
      setloophasbreak(fs, currloop, pc);
      return;
    }
    if (offs < 0) {
      /* the final jump in a while-loop will have its line fixed to the start
         line of the loop; if the line for this jump is greater than the start
         line, this cannot be a while-loop */
      if (D->usedebuginfo && getline(fs->f, pc) > getline(fs->f, target))
        /* repeat-loops do not have fixed lines */
        loopkind = BL_REPEAT;
      else
        loopkind = BL_WHILE;
    }
    /* check if this jump breaks from an outer loop, not the current one, which
       would imply that the current loop does not exist */
    else if (currloop->unsure && !isloopexit(currloop, target)) {
      for (i = 2; (loop = getloopstate(fs, i))->kind != BL_FUNCTION; i++) {
        if (isloopexit(loop, target)) {
          combineloops: popnloopstate(fs, i-1);
          /* try again */
          detectloops_onjump(D, fs);
          return;
        }
      }
    }
    /* combine the conditions of different repeat-loop states into 1 loop when
       necessary */
    else if (jc == NULL &&
             currloop->kind == BL_REPEAT && !isloopbreak(currloop, target)) {
      for (i = 2; (loop = getloopstate(fs, i))->kind == BL_REPEAT; i++) {
        if (loop->startlabel != currloop->startlabel)
          break;
        if (isloopbreak(loop, target))
          goto combineloops;
      }
    }
  }
  if (loopkind != -1) {
    pushloopstate(fs, loopkind, target, pc+1);
    calcloopunsure(fs, cast(LoopState *, getcurrloop(fs)),getloopstate(fs, 2));
  }
}


/*
** detect loops in the bytecode
*/
static void detectloops (DecompState *D, FuncState *fs) {
  BlockNode *nextnode = NULL;
  D->a.pendingbreak = -1;
  assertphase(D, DECOMP_PHASE_DETECT_LOOPS);
  /* traverse code backwards */
  fs->pc = fs->f->sizecode-1;
  for (; fs->pc >= 0; fs->pc = getprevpc(fs, fs->pc)) {
    /* fetch */
    updateinsn(D, fs);
    /* dispatch */
    firstscan_handleop(D, fs);
    switch (D->a.insn.o) {
      case OP_JMP:
        detectloops_onjump(D, fs);
        break;
      case OP_FORLOOP:
      case OP_FORPREP: {
        int target = fs->pc+1+D->a.insn.sbx;
        /* if entering the forloop push a loop state */
        if (D->a.insn.o == OP_FORLOOP)
          pushloopstate(fs, BL_FORNUM, target, fs->pc+1);
        /* mark leader instructions */
        set_ins_property(fs, fs->pc+1, INS_LEADER);
        set_ins_property(fs, target, INS_LEADER);
        break;
      }
      default:;
    }
    /* pump `hasbreak' every instruction; I use 2 bits for its value, a value
       of 1 means there was a jump detected that may or may not be a break */
    if (getcurrloop(fs)->hasbreak == 1 && D->a.pendingbreak != -1) {
      if (D->a.insn.o != OP_JMP && !testTMode(D->a.insn.o)) {
        cast(LoopState *, getcurrloop(fs))->hasbreak = 2;
        set_ins_property(fs, D->a.pendingbreak, INS_BREAKSTAT);
        D->a.pendingbreak = -1;
      }
    }
    while (fs->pc == getcurrloop(fs)->startlabel)
      finalizeloopstate(fs, &nextnode);
  }
  set_ins_property(fs, 0, INS_LEADER);  /* first instruction is a leader */
  fs->root = addblnode(fs, 0, fs->f->sizecode-1, BL_FUNCTION);
  fs->root->firstchild = nextnode;
  D->loopstk.used = 0;
}

/*****************************************************************************/
/* Check optiimzation phase - checks the function constants to get information
   about code optimization, and then marks sequence points where bytecode could
   have been optimized but is not */
/*****************************************************************************/


/*
** update the fields `isnilrk', `istruerk' and `isfalserk' accordingly
*/
static void updateisrk (DecompState *D, FuncState *fs) {
  int nk, k[2];
  assertphase(D, DECOMP_PHASE_CHECK_LOAD_OPTIMIZATION);
  nk = getkoperands(D->a.insn.o, D->a.insn.b, D->a.insn.c, D->a.insn.bx, k);
  /* check if true, false, or nil exist within MAXINDEXRK; for each of these
     values, if it does not exist within MAXINDEXRK, than the corresponding
     non-label opcode (OP_LOADNIL or OP_LOADBOOL) that loads the value could
     not have been optimized as an RK operand in the next instruction; if it
     could, the lack of optimization indicates a statement boundary */
  while (nk--) {
    const TValue *o = &fs->f->k[k[nk]];
    int isrk = (k[nk] <= MAXINDEXRK);
    if (ttisnil(o))
      D->a.isnilrk = isrk;
    else if (ttisboolean(o)) {
      if (bvalue(o)) D->a.istruerk = isrk;
      else D->a.isfalserk = isrk;
    }
  }
}


static int isloadkfullexpr (DecompState *D, FuncState *fs) {
  OpCode o = D->a.insn.o;
  if (o == OP_LOADK)
    /* if the index is small enough to encode as RK, it cannot be a
       subexpression */
    return D->a.insn.bx <= MAXINDEXRK;
  if (o == OP_LOADBOOL && !test_ins_property(fs, fs->pc, INS_BOOLLABEL))
    return (D->a.insn.b && D->a.istruerk) || (!D->a.insn.b && D->a.isfalserk);
  if (o == OP_LOADNIL)
    return D->a.isnilrk;
  return 0;
}


static int isunaryfullexpr (DecompState *D, FuncState *fs) {
  OpCode prevOp, o = D->a.insn.o;
  int prevpc = getprevpc(fs, fs->pc);
  Instruction prev;
  if (prevpc < 0)
    return 1;
  prev = fs->f->code[prevpc];
  prevOp = GET_OPCODE(prev);
  if (!isload(prevOp) || GETARG_A(prev) != D->a.insn.b)
    return 0;
  if (o == OP_UNM)
    /* if previous is LOADK and the constant is a number, it cannot be a
       subexpression */
    return (prevOp == OP_LOADK && ttisnumber(&fs->f->k[GETARG_Bx(prev)]));
  else if (o == OP_NOT || o == OP_NOT_R1)
    /* if the previous is a constant, is cannot be a subexpression */
    return opLoadsK(prevOp) && !test_ins_property(fs, prevpc, INS_BOOLLABEL);
  return 0;
}


/*
** called by pass1 for marking non-obvious sequence points
*/
static void checkloadoptimization (DecompState *D, FuncState *fs) {
  assertphase(D, DECOMP_PHASE_CHECK_LOAD_OPTIMIZATION);
  /* initially, use values that make `isloadfullexpr' correct, until the nil
     and boolean constants are encountered, where the actual value will be set
     */
  D->a.isnilrk = D->a.istruerk = D->a.isfalserk = 1;
  for (fs->pc = 0; fs->pc < fs->f->sizecode; fs->pc = getnextpc(fs, fs->pc)) {
    updateinsn(D, fs);
    updateisrk(D, fs);
    if (opLoadsK(D->a.insn.o) && isloadkfullexpr(D, fs))
      set_ins_property(fs, fs->pc, INS_KLOCVAR);
    else if (isunarycode(D->a.insn.o) && isunaryfullexpr(D, fs))
      set_ins_property(fs, fs->pc, INS_SEQPT);
  }
}


/*****************************************************************************/
/* Expression parser - scans basic blocks for expression list sequences */
/*****************************************************************************/


static void initbitmaps (DecompState *D, FuncState *fs) {
  luaO_bitmapalloc(D->H, &D->kmap, fs->f->sizek);
  luaO_bitmapalloc(D->H, &D->upvalmap, fs->f->nups);
}

static void updatebitmaps (DecompState *D, FuncState *fs) {
  int ak[2];
  OpCode o = D->a.insn.o;
  /* mark any constants referenced by the current instruction */
  int nk = getkoperands(o, D->a.insn.b, D->a.insn.c, D->a.insn.bx, ak);
  D->a.newref = 0;
  unset_ins_property(fs, fs->pc, INS_SKIPPEDREF);
  while (nk--) {
    int k = ak[nk];
    if (luaO_bitmapsetq(&D->kmap, k)) {
      if (!D->a.newref && !luaO_isbitconsecutive(&D->kmap, k))
        set_ins_property(fs, fs->pc, INS_SKIPPEDREF);
      D->a.newref |= (cast(lu_int32, k) << (2 + (D->a.newref ? SIZE_A : 0)));
      D->a.newref++;
      lua_assert((D->a.newref & 3) < 3);
    }
  }
  /* update the highest upvalue referenced */
  if (o == OP_GETUPVAL || o == OP_SETUPVAL || o == OP_SETUPVAL_R1) {
    int up = D->a.insn.b;
    /* same as with constant references, upvalue references are used to check
       if an assignment list is necessary */
    if (luaO_bitmapsetq(&D->upvalmap, up)) {
      D->a.newref = (cast(lu_int32, up) << 2) | 3;
      if (!luaO_isbitconsecutive(&D->upvalmap, up))
        set_ins_property(fs, fs->pc, INS_SKIPPEDREF);
    }
  }
}


static int getslotactive (const FuncState *fs, int r) {
  return getslotdesc(fs, r)->u.s.firstactive;
}


/* slot becomes active after PC, meaning it completely evluates the value it
   will hold, and may be discharged at a later instruction */
static void setslotactive (FuncState *fs, int r, int pc) {
  getslotdesc(fs, r)->u.s.firstactive = getnextpc(fs, pc);
}

/* get the first instruction which evaluated the current value in slot R */
static int getslotinit (const FuncState *fs, int r) {
  int pc = getslotactive(fs, r);
  while (--pc >= 0)
    if (test_ins_property(fs, pc, INS_LOCVAREXPR)) return pc;
  return pc;
}


enum SLOTFLAGS {
  SLOT_CONSTRUCTOR_PENDING = 1,  /* holds an active constructor */
  SLOT_DISCHARGED_RECORD  /* discharged as an operand in a constructor */
};


#if HKSC_STRUCTURE_EXTENSION_ON
static void setslottype (FuncState *fs, int r, int t) {
  SlotDesc *slot = getslotdesc(fs, r);
  lua_assert(t >= 0 && t != LUA_TSTRUCT);
  slot->type = t;
  slot->proto = NULL;
}

static void setslotproto (FuncState *fs, int r, const struct StructProto *p) {
  SlotDesc *slot = getslotdesc(fs, r);
  slot->type = LUA_TSTRUCT;
  slot->proto = p;
}


static int getslottype (const FuncState *fs, int r) {
  return getslotdesc(fs, r)->type;
}


static void clearslottype (FuncState *fs, int r) {
  SlotDesc *slot = getslotdesc(fs, r);
  slot->type = LUA_TNIL;
  slot->proto = NULL;
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void clearslotflags (FuncState *fs, int r) {
  getslotdesc(fs, r)->flags = 0;
}


/* PC is the first evaluation point in the expression list R was in at the time
   of discharge */
static void setslotdischargerecord (FuncState *fs, int r, int pc) {
  SlotDesc *slot = getslotdesc(fs, r);
  slot->flags = SLOT_DISCHARGED_RECORD;
  slot->u.s.aux = pc;
}


static int isslotdischaredrecord (const FuncState *fs, int r) {
  return getslotdesc(fs, r)->flags == SLOT_DISCHARGED_RECORD;
}

static int getslotdischarged (const FuncState *fs, int r) {
  lua_assert(isslotdischaredrecord(fs, r));
  return getslotdesc(fs, r)->u.s.aux;
}


static void setslotconstructor (FuncState *fs, int r, int index) {
  SlotDesc *slot = getslotdesc(fs, r);
  slot->flags = SLOT_CONSTRUCTOR_PENDING;
  slot->u.s.aux = index;
}


static int isslotconstructor (const FuncState *fs, int r) {
  return getslotdesc(fs, r)->flags == SLOT_CONSTRUCTOR_PENDING;
}


static void clearslotconstructor (FuncState *fs, int r) {
  if (isslotconstructor(fs, r))
    clearslotflags(fs, r);
}


static struct ConsControl *getslotconstructor (const FuncState *fs, int r) {
  SlotDesc *slot = getslotdesc(fs, r);
  int index = slot->u.s.aux;
  lua_assert(isslotconstructor(fs, r));
  return fs->D->cons_state.s + index;
}


enum ParserStatus {
  PARSER_STATUS_INITIAL,
  PARSER_STATUS_ACTIVE,
  PARSER_STATUS_ENDEXPR,
  PARSER_STATUS_COUNT
};

enum ParserMode {
  PARSER_MODE_DEFAULT,
  PARSER_MODE_CONSTRUCTOR,  /* determine constructor lifetimes in code */
  PARSER_MODE_ASSIGNMENTS,  /* keep track of constant/upval references */
  PARSER_MODE_COUNT
};

enum ParserToken {
  DEFAULT_TOKEN,
  TOKEN_STORE = 1,  /* a store instruction */
  TOKEN_RETCODE,  /* a return statement */
  TOKEN_CALLSTAT,  /* a call statement */
  TOKEN_BREAK,
  TOKEN_JUMP_BACK,
  TOKEN_FORPREP,  /* a for-loop preparation instruction */
  TOKEN_ENDSCOPE,  /* a scope-ending instruction (JMP or CLOSE) */
  TOKEN_ENDOFCODE,  /* end of code */
  TOKEN_COUNT
};


typedef struct ExpressionParser {
  StackExpr *expr;  /* the current basic block expression */
  /* the state of the parser determines how to behave on each instruction */
  enum ParserStatus status;
  /* the mode of the parser determines what actions it takes for certain
     expressions */
  enum ParserMode mode;
  /* the token indicates to the caller what kind of instruction the parser
     stopped on */
  enum ParserToken token;
  /* startpc is the pc at the time the parser was initialized */
  /* prevpc is the last pc dispatched by the parser */
  int startpc, prevpc;
  int lastopen;
  /* these fields are stack positions
     - base is the base of the current expression list being parsed
     - top is the top of the pending stack space
     - actualtop is the top of written stack space (pending or discharged)
     - prevtop is what the top was before dispatch */
  lu_byte base, top, actualtop, prevtop;
} ExpressionParser;


static void initstackexpr (StackExpr *e) {
  e->startpc = e->endpc = -1;
  e->firstreg = e->lastreg = 0;
  e->jump = 0;
}


static void updatestackexpr (DecompState *D, int pc, int reg) {
  StackExpr *e = D->parser->expr;
  if (e->startpc == -1) {
    e->startpc = pc;
    e->firstreg = reg;
  }
  e->endpc = pc;
  e->lastreg = reg;
}


static int parser_getendlabel (const DecompState *D) {
  const StackExpr *e = D->parser->expr;
  return (e->jump && e->startpc != -1) ? e->startpc + e->jump : -1;
}


static StackExpr *parser_getexpr (const DecompState *D, int n) {
  int count = D->stackexpr.used;
  lua_assert(n < 0);
  if (count + n <= 0)
    return NULL;
  return D->parser->expr+n;
}


static int stackexprisunreachable (const FuncState *fs, const StackExpr *e) {
  int jumppc = e->startpc-1;
  const Instruction *code = fs->f->code;
  /* unreachable block must be undonditionally jumped over */
  if (jumppc >= 0 && GET_OPCODE(code[jumppc]) == OP_JMP &&
      getjumpcontrol(fs, jumppc) == NULL) {
    /* then check if the jump skips the whole expression */
    int nextlabel = getjump(fs, jumppc);
    if (GET_OPCODE(code[nextlabel-1]) == OP_JMP)
      nextlabel -= 1 + (getjumpcontrol(fs, nextlabel-1) != NULL);
    return (getprevpc(fs, nextlabel) == e->endpc);
  }
  return 0;
}


static void initparser (DecompState *D, FuncState *fs, int base, int mode) {
  initregproperties(fs);
  D->parser->status =(base==NO_REG)?PARSER_STATUS_INITIAL:PARSER_STATUS_ACTIVE;
  D->parser->mode = mode;
  D->parser->token = DEFAULT_TOKEN;
  D->parser->base = D->parser->top = D->parser->actualtop = base;
  D->parser->startpc = check_exp(ispcvalid(fs, fs->pc), fs->pc);
  D->parser->prevpc = -1;
  D->parser->lastopen = -1;
  D->stackexpr.used = 0;
  D->parser->expr = VEC_NEWELT(D->H, D->stackexpr);
  initstackexpr(D->parser->expr);
  initbitmaps(D, fs);
}

static void parser_reset (DecompState *D) {
  D->parser->token = DEFAULT_TOKEN;
  D->parser->base = D->parser->top = D->parser->actualtop = NO_REG;
  D->parser->status = PARSER_STATUS_INITIAL;
}


static void parser_advance (DecompState *D, FuncState *fs) {
  fs->pc = getnextpc(fs, fs->pc);
  parser_reset(D);
}


static void parser_visit (DecompState *D, FuncState *fs) {
  updatebitmaps(D, fs);
}


static void endexpr (DecompState *D, int token) {
  D->parser->status = PARSER_STATUS_ENDEXPR;
  D->parser->token = token;
}


static StackExpr *parser_pushexpr (DecompState *D) {
  StackExpr *prevexpr;
  if (D->parser->expr->startpc == -1)
    return NULL;
  D->parser->expr = VEC_NEWELT(D->H, D->stackexpr);
  prevexpr = D->parser->expr - 1;
  initstackexpr(D->parser->expr);
  return prevexpr;
}


static int parser_mergeexpr (DecompState *D, FuncState *fs) {
  if (D->stackexpr.used > 1) {
    StackExpr *e1 = parser_getexpr(D, -1);
    StackExpr *e2 = D->parser->expr;
    int pc;
    if (e1->jump || stackexprisunreachable(fs, e1)) {
      lua_assert(GET_OPCODE(fs->f->code[e1->startpc-1]) == OP_JMP);
      if (getjump(fs, e1->startpc-1) == e2->startpc) {
        if (e1->jump)
          /* merge with the previous basic block subexpression */
          e1--;
        else
          /* make the expression start on the jump */
          e1->startpc--;
      }
    }
    if (e1->firstreg == NO_REG) {
      e1->firstreg = e2->firstreg;
      e1->lastreg = e2->lastreg;
    }
    e1->endpc = e2->endpc;
    e1->lastreg = e2->lastreg;
    /* unset extra LOCVAREXPR for the basic block expressions, the actual start
       is in the previous basic block */
    for (pc = e1->startpc+1; pc <= e1->endpc; pc++)
      unset_ins_property(fs, pc, INS_LOCVAREXPR);
    set_ins_property(fs, e1->startpc, INS_LOCVAREXPR);
    D->parser->expr = e1;
    D->stackexpr.used = (e1 - D->stackexpr.s) + 1;
    return 1;
  }
  return 0;
}


static void restorelastreg (const FuncState *fs, StackExpr *e) {
  Instruction jc = fs->f->code[getnextpc(fs, e->endpc)];
  if (testTMode(GET_OPCODE(jc)) && !testAMode(GET_OPCODE(jc))) {
    int b = GETARG_B(jc), c = GETARG_C(jc);
    if (!ISK(c) && c > e->lastreg)
      e->lastreg = c;
    else if (!ISK(b) && b > e->lastreg)
      e->lastreg = b;
  }
}


static void parser_checklabel (DecompState *D, FuncState *fs) {
  while (fs->pc == parser_getendlabel(D)) {
    StackExpr *prevbb = parser_getexpr(D, -1);
    StackExpr *currbb = D->parser->expr;
    int prevpc = getprevpc(fs, fs->pc);
    lua_assert(prevbb != NULL);
    if (currbb->endpc == prevpc &&
        (prevbb->lastreg == NO_REG || prevbb->lastreg+1 == D->parser->top)) {
      if (!parser_mergeexpr(D, fs))
        break;
    }
    else {
      if (prevbb->lastreg != NO_REG)
        restorelastreg(fs, prevbb);
      endexpr(D, DEFAULT_TOKEN);
      break;
    }
  }
}

/* L and R are equal if it is a unary operation */
static int isloadsubexpr (const DecompState *D, const FuncState *fs,
                          const OperandDesc *l, const OperandDesc *r) {
  int reg = D->a.insn.a, lastonstack = D->parser->top-1;
  int l_init, r_init, l_active, r_active;
  if (D->parser->status != PARSER_STATUS_ACTIVE)
    return 0;
  if (test_ins_property(fs, fs->pc, INS_SEQPT))
    return 0;
  /* must be like ADD 0 0 1 where top == 1 or UNM 0 0 where top == 0 */
  if (reg != l->r || lastonstack != r->r)
    return 0;
  l_init = getslotinit(fs, l->r);
  r_init = getslotinit(fs, r->r);
  /* make sure the operands were just recently evaluated */
  l_active = getslotactive(fs, l->r);
  r_active = getslotactive(fs, r->r);
  if (r_active != fs->pc || (l_active != r_active && l_active != r_init))
    return 0;
  if (test_ins_property(fs, l_init, INS_MULTILOAD) ||
      test_ins_property(fs, r_init, INS_MULTILOAD))
    return 0;
  if (iskmode(l->mode) && test_ins_property(fs, l_init, INS_KLOCVAR))
    return 0;
  if (iskmode(r->mode) && test_ins_property(fs, r_init, INS_KLOCVAR))
    return 0;
  return 1;
}


static void markdischargedexp (FuncState *fs, int from, int to) {
  int pc;
  for (pc = from; pc < to; pc++) {
    if (test_ins_property(fs, pc, INS_LOCVAREXPR)) {
      unset_ins_property(fs, pc, INS_LOCVAREXPR);
      set_ins_property(fs, pc, INS_DISCHARGEDLOCVAREXPR);
    }
  }
}


static void parser_dischargeload (DecompState *D, FuncState *fs,
                                  const OperandDesc *l, const OperandDesc *r) {
  D->parser->top = l->r+1;
  if (l != r)
    markdischargedexp(fs, getslotinit(fs, l->r)+1, fs->pc);
  setslotactive(fs, l->r, fs->pc);
  updatestackexpr(D, fs->pc, l->r);
}


static void parser_dischargeread (DecompState *D, FuncState *fs, OperandDesc o)
{
  while (D->parser->top > o.r)
    clearslotconstructor(fs, --D->parser->top);
  (void)fs;
}


static void parser_fullexpr (DecompState *D, FuncState *fs,
                             const OperandDesc operands[2], int noperands) {
  int reg = D->a.insn.a, lastreg = reg;
  lua_assert(noperands >= 0 && noperands <= 2);
  if (reg != D->parser->top) {
    endexpr(D, DEFAULT_TOKEN);
    return;
  }
  while (noperands--) {
    int r = operands[noperands].r;
    if (r >= D->parser->base) {
      endexpr(D, DEFAULT_TOKEN);
      return;
    }
  }
  if (D->a.insn.o == OP_LOADNIL)
    lastreg = D->a.insn.b;
  else if (D->a.insn.o == OP_VARARG && D->a.insn.b > 2)
    lastreg = reg + D->a.insn.b - 2;
  set_ins_property(fs, fs->pc, INS_LOCVAREXPR);
  for (; reg <= lastreg; reg++)
    setslotactive(fs, reg, fs->pc);
  D->parser->top = D->parser->actualtop = reg;
  /* first set the first reg, than update the lastreg */
  updatestackexpr(D, fs->pc, D->a.insn.a);
  updatestackexpr(D, fs->pc, reg-1);
  if (D->a.insn.a != lastreg)
    set_ins_property(fs, fs->pc, INS_MULTILOAD);
}


static void parser_onload (DecompState *D, FuncState *fs) {
  OperandDesc operands[3];
  int oldtop, i, noperands;
  if (D->parser->status == PARSER_STATUS_INITIAL) {
    D->parser->base = D->parser->top = D->parser->actualtop = D->a.insn.a;
    D->parser->status = PARSER_STATUS_ACTIVE;
  }
  if (test_ins_property(fs, fs->pc, INS_BOOLLABEL) && !D->a.insn.c) {
    /* only need to handle 1 of the bool labels */
    D->parser->expr->endpc = fs->pc;
    return;
  }
  noperands = getregoperands(D->a.insn.o, D->a.insn.a, D->a.insn.b,
                             D->a.insn.c, operands);
  lua_assert(noperands >= 0 && noperands <= 2);
  oldtop = D->parser->top;
  /* end any pending constructors that are operands */
  for (i = noperands-1; i >= 0; i--)
    clearslotconstructor(fs, operands[i].r);
  if (noperands && isloadsubexpr(D, fs, &operands[0], &operands[noperands-1]))
    parser_dischargeload(D, fs, &operands[0], &operands[noperands-1]);
  else
    parser_fullexpr(D, fs, operands, noperands);
  /* reset flags for clobbered slots */
  for (i = D->parser->top-1; i >= oldtop; i--) {
    clearslotflags(fs, i);
#if HKSC_STRUCTURE_EXTENSION_ON
    clearslottype(fs, i);
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  }
}


static void parser_onforprep (DecompState *D, FuncState *fs, int firstreg);


static void parser_onjump (DecompState *D, FuncState *fs) {
  int label = fs->pc+1+D->a.insn.sbx;
  const Instruction *jc = getjumpcontrol(fs, fs->pc);
  StackExpr *lastbb;
  if (D->parser->expr->startpc == -1 && jc && !testAMode(GET_OPCODE(*jc)))
    goto pushfakeexpr;
  if (D->a.insn.sbx <= 0 || test_ins_property(fs, fs->pc, INS_BREAKSTAT)) {
    int token = test_ins_property(fs, fs->pc, INS_BREAKSTAT) ?
    TOKEN_BREAK : TOKEN_JUMP_BACK;
    endexpr(D, token);
    return;
  }
  else if (GET_OPCODE(fs->f->code[label]) == OP_TFORLOOP) {
    parser_onforprep(D, fs, GETARG_A(fs->f->code[label]));
    return;
  }
  lastbb = parser_pushexpr(D);
  /* adjust TOP as needed when values are not dicharged */
  if (isbooljump(fs, fs->pc, label)) {
    int bpc = test_ins_property(fs, label, INS_BOOLLABEL) ? label : fs->pc+1;
    D->parser->top = GETARG_A(fs->f->code[bpc]);
  }
  else if (jc && GET_OPCODE(*jc) == OP_TESTSET)
    D->parser->top--;
  /* if a comparison uses pushed values, adjust LASTREG so it will match the
     expected LASTREG of the next basic block; if the basic blocks are not of
     the same expression, LASTREG will be reverted back */
  if (lastbb != NULL && jc != NULL && !testAMode(GET_OPCODE(*jc))) {
    int b = GETARG_B(*jc), c = GETARG_C(*jc);
    if (D->parser->actualtop == D->parser->top+2 && !ISK(b) && !ISK(c))
      lastbb->lastreg = D->parser->top;
  }
  D->parser->actualtop = D->parser->top;
  if (lastbb == NULL) {
    /* the last basic block does not yield a stack value, so create a fake
       StackExpr entry to assist in merging basic blocks in case that is needed
       once the label is reached; use the special value NO_REG to indiciate the
       StackExpr is fake */
    pushfakeexpr:
    D->parser->expr = VEC_NEWELT(D->H, D->stackexpr);
    *D->parser->expr = *(D->parser->expr-1);
    lastbb = D->parser->expr-1;
    initstackexpr(lastbb);
    lastbb->startpc = fs->pc - (jc != NULL);
    lastbb->endpc = fs->pc;
    lastbb->firstreg = lastbb->lastreg = NO_REG;
  }
  /* jump is always positive */
  D->parser->expr->jump = cast(unsigned int, D->a.insn.sbx);
}


static int parser_readoperand (DecompState *D, FuncState *fs, OperandDesc o) {
  int r = o.r, r_init;
  int checkloadk = cast_int(o.mode) == -1;
  if (D->parser->status == PARSER_STATUS_INITIAL)
    return 0;
  if (r < D->parser->top-1 || r >= D->parser->top)
    return 0;
  r_init = getslotinit(fs, r);
  if (checkloadk)
    o.mode = OpArgRK;
  if (iskmode(o.mode) && test_ins_property(fs, r_init, INS_KLOCVAR)) {
    /* OP_LOADK followed by a `goiffalse' jump is not optimized by Lua,
       therefore this check is added to account for expressions like `1 or a',
       where `1' is tested with OP_TEST instead of being optimized, because of
       the use of `or' instead of `and'. See the difference between
       `luaK_goiftrue' and `luaK_goiffalse' in `lcode.c' */
    if (!checkloadk || GET_OPCODE(fs->f->code[r_init]) != OP_LOADK)
      return 0;
  }
  parser_dischargeread(D, fs, o);
  return 1;
}


static int parser_readreg (DecompState *D, FuncState *fs, int r, int tested) {
  OperandDesc o;
  o.r = r;
  /* if the variable is referenced by a test instruction, pass in mode RK to
     detect lack of optimization of testing a constant value, in the case of
     a variable initialized with LOADK and than tested:
        local a = 12;
        if a then ... end
     the code must not be optimized to match, so ensure the variable is not
     marked as DISCHARGED to avoid pruning */
  o.mode = tested == 1 ? OpArgRK : (tested > 1 ? -1 : OpArgR);
  return parser_readoperand(D, fs, o);
}


static void parser_ontest (DecompState *D, FuncState *fs) {
  OpCode o = D->a.insn.o;
  int a = D->a.insn.a, b = D->a.insn.b, c = D->a.insn.c;
  int r1, r2, discharged = 1;
  if (testAMode(o))
    r1 = a, r2 = -1;
  else
    r1 = ISK(b) ? -1 : b, r2 = ISK(c) ? -1 : c;
  /* sort r1 and r2, r1 must be less */
  if (r1 > r2 && r2 != -1) {
    int temp = r1; r1 = r2; r2 = temp;
  }
  if ((r2 == -1 || (discharged = parser_readreg(D, fs, r2, 1))) && r1 != -1)
    discharged = parser_readreg(D, fs, r1, 1 + testAMode(o));
  if (!discharged) {
    if (D->parser->status != PARSER_STATUS_INITIAL)
      endexpr(D, DEFAULT_TOKEN);
  }
}


/*
** only call to lower the top
*/
static void parser_trimtop (DecompState *D, FuncState *fs, int newtop) {
  int r;
  lua_assert(newtop <= D->parser->top);
  for (r = newtop+1; r < D->parser->actualtop; r++)
    clearslotconstructor(fs, r);
  lua_assert(D->cons_state.used >= 0);
  D->parser->actualtop = D->parser->top = newtop;
}


static void parser_onconstructor (DecompState *D, FuncState *fs) {
  struct ConsControl *cc;
  int index, reg = D->a.insn.a;
  if (D->parser->mode == PARSER_MODE_CONSTRUCTOR &&
      fs->pc > D->parser->startpc && reg <= D->parser->base) {
    D->parser->actualtop = D->parser->top = reg;
    endexpr(D, DEFAULT_TOKEN);
    return;
  }
  parser_onload(D, fs);
  if (D->a.insn.b == 0 && D->a.insn.c == 0)
    return;
  VEC_GROW(D->H, D->cons_state);
  cc = &D->cons_state.s[index = D->cons_state.used++];
  cc->startpc = cc->endpc = fs->pc;
  cc->setlist = -1;
  cc->nhash = 0;
#if HKSC_STRUCTURE_EXTENSION_ON
  cc->proto = NULL;
  if (D->a.insn.o == OP_NEWSTRUCT)
    cc->proto = luaR_getstructbyid(D->H, getinsndata(fs, fs->pc+1));
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  setslotconstructor(fs, reg, index);
}

#if HKSC_STRUCTURE_EXTENSION_ON
/* return codes for resolvestructindex */
enum {
  /* the point is to determine the necessity of allcating hash-space for a
     SETTABLE operation within a struct constructor by classifying it as one of
     these codes */
  INDEX_MAY_RESOLVE,  /* this code represents cases of ambiguity; the index may
                         or may not resolve to a slot name, and therefore may
                         or may not have hash-space allocated for it */
  INDEX_CANNOT_RESOLVE,  /* known-type, non-string keys cannot resolve to a
                            slot name, and therefore must have hash-space
                            allocated inside a constructor */
  INDEX_IS_SLOT_NAME  /* string constants which are slot names would never
                         appear as keys in a SETTABLE code inside a struct
                         constructor; it would be SETSLOT instead */
};

static int parser_resolvestructindex (const DecompState *D,
                                  const FuncState *fs, const StructProto *p) {
  const TValue *o;
  lua_assert(IS_OP_SETTABLE(D->a.insn.o));
  /* if B is a register, see if it was type-checked, if so you can determine if
     it can resolve to a slot */
  if (getBMode(D->a.insn.o) != OpArgK && !ISK(D->a.insn.b)) {
    int type = getslottype(fs, D->a.insn.b);
    if (type != LUA_TNIL && type != LUA_TSTRING)
      return INDEX_CANNOT_RESOLVE;
    else return INDEX_MAY_RESOLVE;
  }
  /* if B is a constant, see if it is a string and if the string is a slot
     name, in which case this assignment must be outside the constructor
     (otherwise SETSLOT would have been used) */
  o = &fs->f->k[INDEXK(D->a.insn.b)];
  if (ttisstring(o) && luaR_findslot(p, rawtsvalue(o)) != NULL)
    return INDEX_IS_SLOT_NAME;
  return ttisstring(o) ? INDEX_MAY_RESOLVE : INDEX_CANNOT_RESOLVE;
}
#endif /* HKSC_STRUCTURE_EXTENSION_ON */


static void parser_onstore (DecompState *D, FuncState *fs);

static void parser_updateconstructor (DecompState *D, FuncState *fs) {
  int maxhashsize, reg = D->a.insn.a;
  int ishash = !IS_OP_SETSLOT(D->a.insn.o);
  int endconstructor = 0;
  struct ConsControl *cc = getslotconstructor(fs, reg);
  maxhashsize = luaO_fb2int(GETARG_C(fs->f->code[cc->startpc]));
#if HKSC_STRUCTURE_EXTENSION_ON
  /* SETSLOTMT cannot exist in a constructor */
  if (D->a.insn.o == OP_SETSLOTMT)
    endconstructor = 1;
  else if (cc->proto && ishash) {
    if (!cc->proto->hasproxy)
      /* structs cannot have a constructor hash item without a backing table */
      endconstructor = 1;
    else {
      /* You cannot always be certain if the compiler would have allocated hash
         table space for this index; because CHECKTYPE codes are ommitted if
         the location to type-check will always have the matching type, you
         cannot know if any register is statically typed, which you need to
         know to determine if hash table space was allocated for it.
         In these cases of ambiguity, I assume the compiler did not allocate
         hashtable space, allowing for longest possible matches (this may
         generate unmatching bytecode, but still equivalent to the source code)
         Consider the following code:
            local a:number = b; -- CHECKTYPE
            local b = a; -- no type-check
         compared to:
            local a:number = b;
            local b:number = a; -- no type-check because a is always a number
         These 2 examples generate the same code, therefore it is impossible to
         know at this point if register 'b' is statically typed, and if it is
         used as in index in the constructor, it will allocate hash table space
         in the second example but not the first */
      int result = parser_resolvestructindex(D, fs, cc->proto);
      /* if the index is a slot name, it cannot be inside the constructor;
         otherwise SETSLOT would be used instead of SETTABLE */
      endconstructor = (result == INDEX_IS_SLOT_NAME);
      /* if the index cannot resolve to a slot at runtime, the compiler would
         have allocated hashtable space for it */
      ishash = (result == INDEX_CANNOT_RESOLVE);
    }
  }
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  if (endconstructor || (ishash && cc->nhash >= maxhashsize)) {
    clearslotconstructor(fs, reg);
    parser_onstore(D, fs);
    return;
  }
  /* if temporary registers were used, record them as discharged */
  if (D->parser->top != D->parser->prevtop) {
    const int pc = getslotinit(fs, D->parser->top);
    markdischargedexp(fs, pc, fs->pc);
    lua_assert(D->parser->prevtop != NO_REG);
    for (reg = D->parser->top; reg < D->parser->prevtop; reg++)
      setslotdischargerecord(fs, reg, pc);
  }
  /* update constructor state */
  D->parser->expr->endpc = cc->endpc = fs->pc;
  cc->nhash += ishash;
  set_ins_property(fs, fs->pc, INS_CONSTRUCTOR_SETTABLE);
  D->parser->lastopen = fs->pc;
}

static int parser_checkopenexpr (DecompState *D, FuncState *fs, int base) {
  if (D->parser->mode == PARSER_MODE_CONSTRUCTOR && base < D->parser->base) {
    D->parser->actualtop = D->parser->top = base;
    endexpr(D, DEFAULT_TOKEN);
    return 0;
  }
  (void)fs;
  return 1;
}


static void parser_onsetlist (DecompState *D, FuncState *fs) {
  struct ConsControl *cc;
  int pc, reg = D->a.insn.a;
  if (!parser_checkopenexpr(D, fs, reg))
    return;
  /* something bad is happening if REG is not a constructor anymore */
  if (!isslotconstructor(fs, reg)) {
    lua_assert(0);
    return;
  }
  cc = getslotconstructor(fs, reg);
  for (pc = cc->endpc+1; pc < fs->pc; pc++)
    unset_ins_property(fs, pc, INS_LOCVAREXPR);
  cc->endpc = cc->setlist = fs->pc;
  setslotactive(fs, reg, fs->pc);
  parser_trimtop(D, fs, reg+1);
  updatestackexpr(D, fs->pc, reg);
  D->parser->lastopen = fs->pc;
}


static int parser_onopenexpr (DecompState *D, FuncState *fs, int firstreg) {
  const int startpc = check_exp(firstreg >= D->parser->base,
                                getslotinit(fs, firstreg));
  int pc = startpc;
  Instruction insn = fs->f->code[pc];
  /* if the open expression shared a LOADNIL code with a different statement,
     do not unmark INS_LOCVAREXPR for the start pc */
  if (GET_OPCODE(insn) == OP_LOADNIL && GETARG_A(insn) < firstreg)
    pc++;
  for (; pc < fs->pc; pc++)
    unset_ins_property(fs, pc, INS_LOCVAREXPR);
  parser_trimtop(D, fs, firstreg);
  return startpc;
}


static void parser_onforprep (DecompState *D, FuncState *fs, int firstreg) {
  parser_trimtop(D, fs, firstreg);
  endexpr(D, TOKEN_FORPREP);
  D->parser->lastopen = fs->pc;
}


static void parser_onreturn (DecompState *D, FuncState *fs) {
  int nret = D->a.insn.b-1;
  int firstreg = D->a.insn.a;
  if (nret == 1)
    parser_readreg(D, fs, firstreg, 0);
  else if (nret != 0) {
    parser_trimtop(D, fs, firstreg);
    D->parser->lastopen = fs->pc;
  }
  endexpr(D, TOKEN_RETCODE);
}


static void parser_oncall (DecompState *D, FuncState *fs) {
  int startpc, firstreg = D->a.insn.a;
  int nret = D->a.insn.c-1;
  if (!parser_checkopenexpr(D, fs, firstreg))
    return;
  startpc = parser_onopenexpr(D, fs, firstreg);
  if (nret == 0)
    endexpr(D, TOKEN_CALLSTAT);
  else {
    /* if the call returns up to stack top, pretend it returns 1 value */
    nret = nret > 1 ? nret : 1;
    set_ins_property(fs, startpc, INS_LOCVAREXPR);
    if (nret != 1)
      set_ins_property(fs, startpc, INS_MULTILOAD);
    do {
      setslotactive(fs, D->parser->top, fs->pc);
      D->parser->top++, D->parser->actualtop++;
    } while (--nret);
    D->parser->expr->endpc = fs->pc;
    D->parser->expr->lastreg = D->parser->top-1;
  }
  D->parser->lastopen = fs->pc;
}


static void parser_onconcat (DecompState *D, FuncState *fs) {
  int firstreg = D->a.insn.b;
  if (!parser_checkopenexpr(D, fs, firstreg))
    return;
  /*int startpc =*/ parser_onopenexpr(D, fs, firstreg);
  D->parser->expr->lastreg = D->parser->top-1;
  parser_onload(D, fs);
  D->parser->lastopen = fs->pc;
}


static void parser_onstore (DecompState *D, FuncState *fs) {
  OpCode o = D->a.insn.o;
  int a = D->a.insn.a;
  OperandDesc operands[3];
  int discharged = 1;/*(noperands < 3 || operands[2].r == operands[1].r+1);*/
  int istable = (IS_OP_SETTABLE(o) || IS_OP_SETSLOT(o));
  int noperands = getregoperands(o, a, D->a.insn.b, D->a.insn.c, operands);
  lua_assert(noperands > 0 && noperands <= 3);
  while (discharged && noperands-- > istable)
    discharged = parser_readoperand(D, fs, operands[noperands]);
  if (D->parser->mode != PARSER_MODE_CONSTRUCTOR) {
    if (test_ins_property(fs, fs->pc, INS_CONSTRUCTOR))
      goto updateconstructor;
    else
      goto updatestore; 
  }
  if (istable && isslotconstructor(fs, a))
    updateconstructor: parser_updateconstructor(D, fs);
  else {
    updatestore:
    if (istable && discharged)
      discharged = parser_readoperand(D, fs, operands[0]);
    endexpr(D, TOKEN_STORE);
  }
}


static void parser_onchecktype (DecompState *D, FuncState *fs) {
#if HKSC_STRUCTURE_EXTENSION_ON
  int reg = D->a.insn.a;
  if (D->a.insn.o == OP_CHECKTYPE)
    setslottype(fs, reg, D->a.insn.bx);
  else
    setslotproto(fs, reg, luaR_getstructbyid(D->H, D->a.insn.bx));
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  (void)D; (void)fs;
}


static void parser_dispatch (DecompState *D, FuncState *fs) {
  OpCode o = D->a.insn.o;
  unset_ins_property(fs, fs->pc, INS_LOCVAREXPR);
  D->parser->prevtop = D->parser->top;
  if (D->parser->startpc != fs->pc && test_ins_property(fs, fs->pc, INS_SEQPT))
  {
    endexpr(D, DEFAULT_TOKEN);
    return;
  }
  if (test_ins_property(fs, fs->pc, INS_ENDSCOPE))
    endexpr(D, TOKEN_ENDSCOPE);
  else if (o == OP_MOVE && D->a.insn.b > D->a.insn.a)
    endexpr(D, TOKEN_STORE);
  else if (isstorecode(o))
    parser_onstore(D, fs);
  else {
    if (ischecktypecode(o))
      parser_onchecktype(D, fs);
    else if (IS_OP_CALL(o))
      parser_oncall(D, fs);
    else if (o == OP_CONCAT)
      parser_onconcat(D, fs);
    else if (isconstructorcode(o))
      parser_onconstructor(D, fs);
    else if (o == OP_SETLIST)
      parser_onsetlist(D, fs);
    else if (isload(o))
      parser_onload(D, fs);
    else if (o == OP_JMP)
      parser_onjump(D, fs);
    else if (testTMode(o))
      parser_ontest(D, fs);
    else if (o == OP_FORPREP)
      parser_onforprep(D, fs, D->a.insn.a);
    else if (o == OP_RETURN)
      parser_onreturn(D, fs);
    else
      endexpr(D, DEFAULT_TOKEN);
  }
}


static int parseexpression (DecompState *D, FuncState *fs) {
  lua_assert(fs->pc >= 0);
  initstackexpr(D->parser->expr);
  D->parser->lastopen = -1;
  D->parser->startpc = fs->pc;
  for (; fs->pc < fs->f->sizecode; fs->pc = getnextpc(fs, fs->pc)) {
    updateinsn(D, fs);  /* fetch */
    parser_visit(D, fs);
    parser_checklabel(D, fs);
    parser_dispatch(D, fs);
    D->parser->prevpc = fs->pc;
    if (D->parser->status == PARSER_STATUS_ENDEXPR) {
      const enum ParserToken token = D->parser->token;
      D->parser->expr = D->stackexpr.s;
      D->stackexpr.used = 1;
      if (D->parser->expr->endpc != -1)
        fs->pc = getnextpc(fs, D->parser->expr->endpc);
      if (fs->pc == D->parser->startpc && token == DEFAULT_TOKEN)
        fs->pc = getnextpc(fs, fs->pc);
      /* if the first expression is a fake one pushed to assist merging basic
         blocks, reset it */
      if (D->parser->expr->firstreg == NO_REG && token == DEFAULT_TOKEN)
        initstackexpr(D->parser->expr);
      return D->parser->token;
    }
  }
  return TOKEN_ENDOFCODE;
}


static void scanpostconstructor (DecompState *D, FuncState *fs, int startpc,
                                 int ndischarged, int constructorbase) {
  struct ConsControl *cc = &D->cons_state.s[D->cons_state.used-1];
  OperandDesc operands[3];
  int noperands, lowestdischargepc = fs->f->sizecode;
  int prevtop = D->parser->actualtop;
  for (fs->pc = getnextpc(fs, cc->endpc);
       fs->pc < fs->f->sizecode;
       fs->pc = getnextpc(fs, fs->pc), prevtop = D->parser->actualtop) {
    int i, r;
    OpCode o;
    if (D->parser->actualtop <= constructorbase)
      break;
    /* if top was lowered, unmark discharged registers above the new top */
    for (r = D->parser->actualtop; r < prevtop; r++)
      if (isslotdischaredrecord(fs, r)) ndischarged--, clearslotflags(fs, r);
    if (ndischarged <= 0)
      break;
    /* if this instruction is a settable operation from a lower active
       constructor, ignore it because its already been verified */
    if (test_ins_property(fs, fs->pc, INS_CONSTRUCTOR_SETTABLE))
      continue;
    /* fetch */
    updateinsn(D, fs); r = D->a.insn.a; o = D->a.insn.o;
    /* dispatch */
    /* check if the lexical scope of this constructor has ended */
    if (test_ins_property(fs, fs->pc, INS_ENDSCOPE)) {
      if (o == OP_CLOSE) {
        D->parser->actualtop = r;
        continue;
      }
      else if (o == OP_JMP && getjump(fs, fs->pc) <= startpc)
        break;
    }
    /* check for for-list loop jump (update top accordingly) */
    if (o == OP_JMP) {
      Instruction insn = fs->f->code[getjump(fs, fs->pc)];
      if (GET_OPCODE(insn) == OP_TFORLOOP)
        D->parser->actualtop = GETARG_A(insn);
      continue;
    }
    if (o == OP_FORPREP || o == OP_FORLOOP) {
      D->parser->actualtop = r;
      continue;
    }
    /* get operand registers */
    noperands = getregoperands(o, r, D->a.insn.b, D->a.insn.c, operands);
    /* if noperands is -1, the operands are a continuous stack range, and it is
       an open expression, therefore update top accordingly */
    if (noperands < 0) {
      D->parser->actualtop = operands[0].r;
      continue;
    }
    /* for each register read, see if it was discharged within the constructor
    */
    for (i = 0; i < noperands; i++) {
      int r = operands[i].r;
      if (isslotdischaredrecord(fs, r)) {
        const int pc = getslotdischarged(fs, r);
        lua_assert(ispcvalid(fs, pc));
        if (pc < lowestdischargepc) {
          /* the discharged register may itself be a constructor, in which case
             update the variable CC and fix the startpc of each constructor
             state above it */
          for (; cc->startpc >= pc && cc > D->cons_state.s; cc--)
            cc->endpc = cc->startpc;
          lowestdischargepc = pc;
        }
        clearslotflags(fs, r);
        ndischarged--;
      }
    }
    /* undischarge slots which are written to */
    if (isload(o)) {
      int lastreg = r;
      if (o == OP_LOADNIL)
        lastreg = D->a.insn.b;
      else if (o == OP_VARARG && D->a.insn.b > 1)
        lastreg = r + D->a.insn.b - 2;
      for (; r <= lastreg; r++)
        if (isslotdischaredrecord(fs, r)) ndischarged--, clearslotflags(fs, r);
    }
  }
  /* check if any discharged registers were referenced after the constructor
     without being clobbered */
  if (lowestdischargepc <= cc->endpc) {
    /* save original endpc of base constructor */
    int pc = D->cons_state.s->endpc;
    /* constructor must end before the clobber */
    cc->endpc = getprevpc(fs, lowestdischargepc);
    /* and update all constructors below it */
    while (cc-- > D->cons_state.s)
      cc->endpc = getprevpc(fs, (cc+1)->endpc);
    /* unmark constructor discharges that are now outside the constructor */
    for (; pc > D->cons_state.s->endpc; pc--)
      unset_ins_property(fs, pc, INS_CONSTRUCTOR_SETTABLE);
  }
}



static void parseconstructor (DecompState *D, FuncState *fs, int startpc) {
  int i, ndischarged = 0, reg, base = GETARG_A(fs->f->code[startpc]);
  fs->pc = startpc;
  initparser(D, fs, base, PARSER_MODE_CONSTRUCTOR);
  parseexpression(D, fs);
  for (i = 0; i < D->cons_state.used; i++) {
    struct ConsControl *cc = &D->cons_state.s[i];
    if (cc->setlist != -1) {
      int pc = cc->endpc;
      /* if it has SETLIST, it must end on SETLIST */
      cc->endpc = cc->setlist;
      while (pc > cc->endpc)
        unset_ins_property(fs, pc--, INS_CONSTRUCTOR_SETTABLE);
    }
  }
  /* if top is at or below base, no discharged registers need to be checked */
  if (D->parser->actualtop <= base+1)
    return;
  /* count how many registers between base and top are discharged */
  for (reg = D->parser->actualtop-1; reg >= D->parser->top; reg--)
    if (isslotdischaredrecord(fs, reg)) ndischarged++;
  if (ndischarged == 0)
    return;
  /* ensure the endpc is correct by checking for later references to discharged
     registers used by the constructor */
  scanpostconstructor(D, fs, startpc, ndischarged, base+1);
}


static void finalizeconstructor (DecompState *D, FuncState *fs) {
  const int count = D->cons_state.used;
  const struct ConsControl *cc = check_exp(count > 0, D->cons_state.s);
  int pc = check_exp(cc != NULL, cc->startpc);
  do {
    lua_assert(cc->startpc <= cc->endpc);
    if (cc->startpc >= pc) {
      const int limitpc = getnextpc(fs, cc->endpc);
      for (pc = cc->startpc; pc < limitpc; pc++) {
        set_ins_property(fs, pc, INS_CONSTRUCTOR);
        unset_ins_property(fs, pc, INS_LOCVAREXPR);
        unset_ins_property(fs, pc, INS_DISCHARGEDLOCVAREXPR);
      }
      set_ins_property(fs, cc->startpc, INS_LOCVAREXPR);
      /* fix current pc for next parse session */
      fs->pc = limitpc;
    }
  } while (++cc < D->cons_state.s+count);
  D->cons_state.used = 0;  /* reset constructor stack */
}


static void parseconstructors (DecompState *D, FuncState *fs) {
  ExpressionParser parser = {0};
  assertphase(D, DECOMP_PHASE_PARSE_CONSTRUCTORS);
  fs->pc = 0;
  D->parser = &parser;
  while (D->cons_pc.used > 0) {
    int startpc = D->cons_pc.s[--D->cons_pc.used];
    if (startpc < fs->pc)
      continue;
    parseconstructor(D, fs, startpc);
    finalizeconstructor(D, fs);
  }
  VEC_FREE(D->H, D->cons_pc);
  VEC_INIT(D->cons_pc);
  D->parser = NULL;
}


/*****************************************************************************/
/* Assignment parser - scan blocks of store codes to mark assignment lists */
/*****************************************************************************/


typedef struct ExprBuffer {
  StackExpr b[2];
  lu_byte open[2];
  lu_byte n;
} ExprBuffer;

typedef struct StoreList {
  lu_int32 newref;
  int startpc, endpc;
  int skippedref, noskippedref;
  lu_byte top;
  lu_byte n;
  lu_byte firstsourcek;
  lu_byte clobber;
  /* for the next 2 fields, interesting values are 0, 1, or greater than 1 */
  lu_byte newconstants;
  lu_byte newupvals;
} StoreList;


static int parseassignment (DecompState *D, FuncState *fs, ExprBuffer *buffer){
  StoreList store = {0};
  const StackExpr *RHS = NULL;
  int src = getstoresource(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c);
  const int lastsrc = src;
  const OpCode lastop = D->a.insn.o;
  if (buffer->n == 0)
    goto nostorelist;
  if (src == -1 || ISK(src))
    store.firstsourcek = 1;
  /* check for a relocated RHS expression, assigned directly to a local
     register rather than moved with OP_MOVE; this cannot be the case if the
     source expression is an open expression */
  else if (buffer->n > 1 && !buffer->open[buffer->n-1]) {
    const StackExpr *e2 = &buffer->b[1];  /* the final relocated expression */
    const StackExpr *e1 = &buffer->b[0];  /* the rest of the RHS */
    /* if relocated, the expression must be a single value and located in a
       lower register than the space used to evluate the RHS */
    if (e2->firstreg == e2->lastreg && e2->firstreg < src &&
        getnextpc(fs, e1->endpc) == e2->startpc) {
      store.clobber = 1;
      RHS = e1;
    }
  }
  if (RHS == NULL)
    RHS = &buffer->b[buffer->n-1];
  store.top = src;
  store.startpc = fs->pc;
  store.skippedref = store.noskippedref = -1;
  if (store.firstsourcek) {
    lu_int32 newref = D->a.newref;
    store.n = 1;
    /* lose the constant C, as it counts for the RHS, not LHS */
    if ((newref & 3) <= 1)
      store.newref = 0;
    else
      /* 2 constants; keep argB, lose argC */
      /* mask out the argC constant because it will be included as the argB
         constant as if it were Bx */
      store.newref = (newref & ((1 << (2 + SIZE_A)) - 1)) - 1;
    goto advance;
  }
  for (;;) {
    int expectedsrc;
    if (store.firstsourcek == 1) {
      store.top = src;
      store.firstsourcek = 2;
    }
    expectedsrc = store.top - store.n;
    if (src != expectedsrc || src < RHS->firstreg || src > RHS->lastreg)
      break;
    else {
      store.n++;
      store.endpc = fs->pc;
      if (test_ins_property(fs, fs->pc, INS_SKIPPEDREF))
        store.skippedref = fs->pc;
      else if (D->a.newref) {
        if (store.noskippedref == -1)
          store.noskippedref = fs->pc;
      }
      /* check if NEWREF is a new upvalue reference (indicated by 2 set bits)*/
      if ((store.newref & 3) == 3) {
        if (store.newupvals < 2)
          store.newupvals++;
      }
      else if (store.newref && store.newconstants < 2)
        store.newconstants++;
      store.newref = D->a.newref;
    }
    advance: parser_advance(D, fs);
    updateinsn(D, fs);
    /* end of the store list is marked by a non-store */
    if ((D->a.insn.o != OP_MOVE || D->a.insn.b < D->a.insn.a) &&
        !isstorecode(D->a.insn.o))
      break;
    /* a leader cannot be in the middle or end of a store list */
    if (test_ins_property(fs, fs->pc, INS_LEADER))
      break;
    src = getstoresource(D->a.insn.o, D->a.insn.a, D->a.insn.b, D->a.insn.c);
    if (src == -1 || ISK(src))
      break;
    parser_visit(D, fs);
  }
  if (store.n == 0)
    goto nostorelist;
  lua_assert(store.startpc <= store.endpc);
  lua_assert(RHS->lastreg >= store.top);
  /* check if there are extra RHS values not assigned to anything */
  if (RHS->lastreg > store.top) {
    /* these flags can only apply to a store list that does not have extra
       values */
    if (store.firstsourcek || store.clobber)
      goto nostorelist;
  }
  /* if there are no extra values and no relocated source, check if the last
     assignment in the list should have used a relocatable source, in which
     case the store list cannot be verified;
     example:
      local a, b, c;
      local d, e, f = 1, 2, 3;
      c = f;
      b = e;
      a = d;
      if it was an assignment list, `3' would be loaded direcly into `c':
      local a, b, c;
      a, b, c = 1, 2, 3; */
  else if (!store.firstsourcek && !store.clobber) {
    int initpc = check_exp(isregvalid(fs, lastsrc), getslotinit(fs, lastsrc));
    /* if the last assignment is a local (OP_MOVE) and the corresponing RHS
       expression is not VNONRELOC (i.e. not an open expression), than it is
       not a list */
    if (lastop == OP_MOVE && D->parser->lastopen < initpc)
      goto nostorelist;
  }
  /* any new constant/upvalue references must allow for an assignment list */
  if (store.skippedref == -1 && store.noskippedref != -1)
    goto nostorelist;
  /* if part of th assignment list has regularly ordered references, than
     exclude that part from the list */
  if (store.noskippedref != -1) {
    int pc;
    for (pc = store.noskippedref; pc <= store.endpc; pc = getnextpc(fs, pc))
      store.n--;
    store.endpc = store.noskippedref-1;
  }
  {
    int firstreg = store.top - (store.n - 1);
    /* find the startpc of the assignment */
    int pc, assignstart;
    /* find the lowest temporary register used in the store */
    for (pc = store.startpc; pc <= store.endpc; pc = getnextpc(fs, pc)) {
      int table, key;
      int n = getstoreindexregs(fs->f->code[pc], &table, &key);
      if (n) {
        if (n == 2 && (n--, key == firstreg-1))
          firstreg--;
        if (n == 1 && table == firstreg-1)
          firstreg--;
      }
    }
    if (firstreg < RHS->firstreg)
      firstreg = RHS->firstreg;
    assignstart = getslotinit(fs, firstreg);
    set_ins_property(fs, assignstart, INS_ASSIGNSTART);
    set_ins_property(fs, store.endpc, INS_ASSIGNEND);
  }
  return store.endpc;
  nostorelist:
  parser_advance(D, fs);
  return -1;
}


static void parseassignments (DecompState *D, FuncState *fs) {
  ExpressionParser parser = {0};
  ExprBuffer buffer;
  assertphase(D, DECOMP_PHASE_PARSE_ASSIGNMENTS);
  fs->pc = 0;
  D->parser = &parser;
  initparser(D, fs, NO_REG, PARSER_MODE_ASSIGNMENTS);
  for (;;) {
    buffer.n = 0;
    /* parse until encountering an assignment */
    for (;;) {
      enum ParserToken token = parseexpression(D, fs);
      if (token == TOKEN_ENDOFCODE)
        goto done;
      if ((token == TOKEN_STORE || token == DEFAULT_TOKEN) &&
          D->parser->expr->startpc != -1) {
        if (buffer.n >= 2) {
          buffer.n = 1;
          buffer.b[0] = buffer.b[1];
          buffer.open[0] = buffer.open[1];
        }
        buffer.open[buffer.n] = D->parser->lastopen == D->parser->expr->endpc;
        buffer.b[buffer.n++] = *D->parser->expr;
      }
      else
        buffer.n = 0;
      if (token == TOKEN_STORE)
        break;
      if (token != DEFAULT_TOKEN)
        parser_advance(D, fs);
      else
        parser_reset(D);
    }
    parseassignment(D, fs, &buffer);
  } done:
  D->parser = NULL;
}


/*
** pass1 is the set of all passes before the final pass, grouped together,
** after which the complete lexical block tree should exist, and complete local
** variable information should exist
*/
static void pass1 (DecompState *D, FuncState *fs) {
  BlockNode *func;
  const int nodebug = (D->usedebuginfo == 0);
  /* the primary step of analysis is to detect loops because their detection
     does not rely on other information, whether using debug info or not */
  changephase(D, DECOMP_PHASE_DETECT_LOOPS);
  detectloops(D, fs);
  /* without debug info, the decompiler needs to detect lack of optimization,
     which depends on what is in the constants array; nil, true, and false may
     be encoded as RK values in instructions if they exist in the constants
     array within the range of MAXINDEXRK */
  if (nodebug) {
    changephase(D, DECOMP_PHASE_CHECK_LOAD_OPTIMIZATION);
    checkloadoptimization(D, fs);
  }
  /* first, constructors are parsed to determine their lifetimes in the code;
     then, the constructors are treated atomically in subsequent parse sessions
     */
  if (nodebug) {
    changephase(D, DECOMP_PHASE_PARSE_CONSTRUCTORS);
    parseconstructors(D, fs);
  }
  /* second, assignment lists are parsed to determine which blocks of store
     instructions may be grouped as a list in the generated source code */
  if (nodebug) {
    changephase(D, DECOMP_PHASE_PARSE_ASSIGNMENTS);
    parseassignments(D, fs);
  }
  /* lexical analysis will provide information about how registers are used
     within each lexical scope, which is only needed if not using debug info */
  if (nodebug) {
    changephase(D, DECOMP_PHASE_SCAN_LEXICAL);
    /*scanlexical(D, fs);*/
  }
#if 1
 /* goto end;*/
  /* resize any vectors that will not grow past this point */
  /*finalizeopenexprvector(fs);*/
  /* mark fail- and pass-jumps */
  /*analyzejumps(fs);
  if (D->usedebuginfo == 0) {
    markbbexpr1(fs);
    analyzestack(fs);
  }*/
#endif
  goto end;
  /*analyzeblocks(fs);*/
  goto end;
  /*simblock1(fs);*/
  func = fs->root;
  lua_assert(func != NULL);
  lua_assert(func->nextsibling == NULL);
  lua_assert(func->kind == BL_FUNCTION);
#if 0
  /* add post-processing functions here */
  finalizelexicalblocks1(fs, func);
  fs->nlocvars = 0;
  fixblockendings1(fs, func);
  fs->nlocvars = 0;
  checkparentnilvars1(fs, func);
  markfollowblock1(fs, func);
  if (D->matchlineinfo)
    recordfixedstartlines(fs);
#endif
  end:
  initregproperties(fs);
}


/*****************************************************************************/
/* Source code generation phase */
/*****************************************************************************/


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
  SlotDesc tempslot;  /* a place to put conditional expressions that do not
                         live in a register, such as comparison operations in
                         branch and loop conditions, or testing local registers
                         in branch and loop conditions */
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
  int openexprnildebt;  /* number of registers that contain nil values, but
                           have no actual expression nodes for them because
                           they share a previous OP_LOADNIL code that is before
                           the start of the open expression; if non-zero, the
                           first register to load nils into is NEXTOPENREG */
  int openexprkind;
  OpenExpr *nextopenexpr;  /* next open expression */
  int nextopenreg;  /* first register of next open expression */
  int lastexpindex;
  lu_byte intailemptyblock;
  lu_byte inheadercondition;  /* evaluating the condition of a while-loop or
                                  an if-statement */
} StackAnalyzer;


static void DecompileFunction (DecompState *D, const Proto *f);


#ifdef HKSC_DECOMP_HAVE_PASS2


/*
** returns true if RK indexes a non-local slot
*/
static int istempreg (const FuncState *fs, int rk) {
  return !ISK(rk) && !test_reg_property(fs, rk, REG_LOCAL);
}


/*
** returns the LocVar currently corresponding to REG
*/
static LocVar *getlocvar2 (const FuncState *fs, int reg) {
  return &fs->locvars[fs->actvar[reg]];
}


/*
** map the N newest local variables from their corresponding registers to their
** positions in the LocVar vector
*/
static void addlocalvars2 (FuncState *fs, int n) {
  int i;
  lua_assert(fs->nactvar >= 0 && fs->nactvar <= fs->sizelocvars);
  lua_assert(fs->nlocvars >= 0 && fs->nlocvars <= fs->sizelocvars);
  lua_assert(fs->nlocvars >= n);
  lua_assert(fs->nactvar+n-1 <= fs->f->maxstacksize);
  for (i = 0; i < n; i++)
    fs->actvar[fs->nactvar+i] = fs->nlocvars-n+i;
}


/*
** `varstartsatpc2' returns how many variables start at the given PC (if debug
** info is not being used, the return value is always -1)
*/
static int varstartsatpc2 (FuncState *fs, int pc) {
  struct LocVar *var;
  int i = fs->nlocvars;
  int n = 0;
  lua_assert(ispcvalid(fs, pc));
  while (i < fs->sizelocvars && (var = &fs->locvars[i])->startpc == pc) {
    i = ++fs->nlocvars;
    n++;
    D(printf("variable '%s' begins at (%d)\n", getstr(var->varname), pc+1));
  }
  return n;
}


static void updatenextopenexpr2 (StackAnalyzer *sa, FuncState *fs) {
  sa->nextopenexpr = NULL;
  sa->nextopenreg = -1;
  (void)fs;
}


#else
#define initfirstfree2(fs,f) ((fs)->firstfree = 0)
#define updatenextopenexpr2(sa,fs)  \
  ((sa)->nextopenexpr = NULL, (sa)->nextopenreg = -1)
#endif /* HKSC_DECOMP_HAVE_PASS2 */


#ifdef LUA_DEBUG
static void
debugenterblock2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
#ifdef HKSC_DECOMP_DEBUG_PASS1
  if (node->kind == BL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  DumpIndentation(fs->D);
  DumpString(blocktypename(node->kind),fs->D);
  DumpLiteral("\n",fs->D);
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
  UNUSED(fs); UNUSED(sa); UNUSED(node);
}

static void
debugleaveblock2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
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
  UNUSED(fs); UNUSED(sa); UNUSED(node);
}
#else /* !LUA_DEBUG */
#define debugenterblock2(sa,fs,node) ((void)0)
#define debugleaveblock2(sa,fs,node) ((void)0)
#endif /* LUA_DEBUG */


#ifdef LUA_DEBUG
static void assertblvalid (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
  int startpc = node->startpc;
  int endpc = node->endpc;
  int type = node->kind;
  BlockNode *nextsibling = node->nextsibling;
  lua_assert((startpc > endpc) == nodegetflag(node, EMPTY));
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
  else if (type == BL_DO) {
    lua_assert(test_ins_property(fs, endpc, INS_BLOCKEND));
  }
  else if (type == BL_IF && nextsibling != NULL &&
           nextsibling->kind == BL_ELSE) {
    lua_assert(node->endpc+1 == nextsibling->startpc);
  }
}
#else /* !LUA_DEBUG */
#define assertblvalid(sa,fs,node) ((void)(sa),(void)(fs),(void)(node))
#endif /* LUA_DEBUG */


static void visitinsn2 (FuncState *fs, BlockNode *node, int pc, Instruction i){
#ifdef LUA_DEBUG
  /* make sure this instruction hasn't already been visited */
  lua_assert(!test_ins_property(fs, pc, INS_VISITED));
  set_ins_property(fs, pc, INS_VISITED);
#endif /* LUA_DEBUG */
  UNUSED(fs); UNUSED(node); UNUSED(pc); UNUSED(i);
}


/*
** update pc variables based on CHILD; if it is not NULL, update
** NEXTCHILDSTARTPC to its startpc and NEXTPCLIMIT to 1 before its startpc,
** otherwise, set NEXTCHILDSTARTPC to (-1) and NEXTPCLIMIT to 1 before the
** endpc of PARENT
*/
static void initnextchild2 (StackAnalyzer *sa, BlockNode *parent,
                            BlockNode *child, int *childstartpc) {
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


static BlockNode *updatenextchild2 (StackAnalyzer *sa, BlockNode *parent,
                                    BlockNode *child, int *childstartpc) {
  BlockNode *nextchild;
  lua_assert(child != NULL);
  nextchild = child->nextsibling;
  initnextchild2(sa, parent, nextchild, childstartpc);
  return nextchild;
}

#ifdef HKSC_DECOMP_HAVE_PASS2

static void dischargestores2(StackAnalyzer *sa, FuncState *fs);

/*
** adds a new store node to the pending chain
*/
static ExpNode *
updatelaststore2 (StackAnalyzer *sa,FuncState *fs,ExpNode *exp) {
  int prevlaststore = sa->laststore;
  int chainempty = prevlaststore == 0;
  lua_assert(exp != NULL);
  lua_assert(exp->kind == ESTORE);
  exp->previndex = sa->laststore;
  sa->laststore = exp2index(fs, exp);
  /* when preserving line info, if the last expression has a different line
     than this store, add extra parens around it to make it end on the line
     that this store is mapped to */
  if (fs->D->matchlineinfo && chainempty) {
    ExpNode *src = index2exp(fs, sa->lastexpindex);
    if (src != NULL) {
      int line = getexpline(src);
      /* compare the last line of a closure, not the first */
      if (src->kind == ECLOSURE)
        line = src->u.cl.p->lastlinedefined;
      if (line != exp->line && line != 0) {
        if (src->kind == ECONSTRUCTOR && src->aux <= 0)
          src->aux = exp->line;
        else if (src->kind == ECALL && src->aux == 0)
          src->aux = exp->line;
        else
          src->closeparenline = exp->line;
      }
    }
  }
  if (chainempty) {
    int issingle;
    int lowesttempreg = exp->u.store.srcreg;
    if (IS_OP_SETTABLE(exp->u.store.rootop) &&istempreg(fs, exp->u.store.aux2))
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
static int issimplelistfield (ExpNode *exp) {
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


static const char *getunoprstring (UnOpr op) {
  static const char *const unoprstrings[] = {
    "-", "not", "#"
  };
  lua_assert(op != OPR_NOUNOPR);
  return unoprstrings[op];
}


static const char *getbinoprstring (BinOpr op) {
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


static void DumpBinOpr (BinOpr op, DecompState *D) {
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


static void checklineneeded2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  int line = exp->line;
  lua_assert(line >= D->linenumber);
  updateline2(fs, line, D);
}

/*****************************************************************************/
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
static void addholditem2 (DecompState *D, struct HoldItem *item,
                          const char *str, size_t len,lu_byte addtrailingspace)
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
static void dischargeholditems2 (DecompState *D) {
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
static void predumpexp2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  /* add new lines if needed */
  checklineneeded2(D,fs,exp);
  /* dump all strings that were held until the line was updated */
  dischargeholditems2(D);
  CheckSpaceNeeded(D);
}


/*
** `postdumpexp2' - all expression dump functions call this after dumping
*/
static void postdumpexp2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  D->needspace = 1;
  UNUSED(fs); UNUSED(exp);
}


static void dumpexpbool2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  predumpexp2(D,fs,exp);
  if (exp->kind == ETRUE)
    DumpLiteral("true",D);
  else
    DumpLiteral("false",D);
  postdumpexp2(D,fs,exp);
}


/* dump a constant (literal) expression */
static void dumpexpk2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  predumpexp2(D,fs,exp);
  DumpTValue(exp->u.k,D);
  postdumpexp2(D,fs,exp);
}


/* dump a global or upvalue name as an R-value */
static void dumpexpvar2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  TString *name = exp->u.name;
  predumpexp2(D,fs,exp);
  if (exp->kind == EGLOBAL && name->tsv.reserved == CONFLICTING_GLOBAL)
    DumpLiteral("_G.",D);
  DumpTString(exp->u.name, D);
  postdumpexp2(D,fs,exp);
}


static void dumplocvar2 (DecompState *D, FuncState *fs, int reg);


/* dump a local variable name as an R-value */
static void dumpexplocvar2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  predumpexp2(D,fs,exp);
  dumplocvar2(D,fs,exp->aux); /* aux is the source register in OP_MOVE */
  postdumpexp2(D,fs,exp);
}


/* dump `nil' */
static void dumpexpnil2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  predumpexp2(D,fs,exp);
  DumpLiteral("nil",D);
  postdumpexp2(D,fs,exp);
}


/* dump `...' */
static void dumpexpva2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  predumpexp2(D,fs,exp);
  DumpLiteral("...",D);
  postdumpexp2(D,fs,exp);
}


/* dump a child function */
static void dumpexpfunc2 (DecompState *D, FuncState *fs, ExpNode *exp) {
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
    for (i = 1; i <= fs->expstack.used; i++) {
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
static ExpNode *getexpinreg2 (FuncState *fs, int reg) {
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
static void pushexp2 (FuncState *fs, int reg, ExpNode *exp, int linkprev) {
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
static void clearslots2 (FuncState *fs, int reg, int n) {
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


static void setfirstfree (FuncState *fs, int newfirstfree) {
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


static void flushpendingexp2 (FuncState *fs) {
#ifdef LUA_DEBUG
  int i;
  for (i = 0; i < fs->expstack.used; i++) {
    ExpNode *exp = &fs->expstack.s[i];
    lua_assert(exp->pending == 0);
  }
#endif /* LUA_DEBUG */
  fs->expstack.used = 0;
  D(printf("resetting fs->firstfree from %d to %d\n", fs->firstfree,
            fs->nactvar));
  setfirstfree(fs, fs->nactvar);
  fs->lastcallexp = 0;
  fs->curr_constructor = 0;
  fs->D->nextlinenumber++;
  fs->seenstatinblock = 1;
}


static ExpNode *popexp2 (FuncState *fs, int reg) {
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
static void dumplocvar2 (DecompState *D, FuncState *fs, int reg) {
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
static void dumpRK2 (DecompState *D, FuncState *fs, int reg, ExpNode *op) {
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


static void pfn_stringifyobj (const char *s, size_t l, void *ud) {
  hksc_State *H = ud;
  Mbuffer *b = &G(H)->buff;
  luaZ_openspace(H, b, luaZ_sizebuffer(b) + l);
  memcpy(luaZ_buffer(b) + luaZ_bufflen(b), s, l);
  luaZ_bufflen(b) += l;
}


static void holdRK2 (DecompState *D, FuncState *fs, int reg,
                     struct HoldItem *hold, int addspace) {
  const char *str;
  size_t len;
  if (ISK(reg)) {
    TString *ts;
    Mbuffer *b = &G(D->H)->buff;
    luaZ_resetbuffer(b);
    luaO_printk(&fs->f->k[INDEXK(reg)], pfn_stringifyobj, D->H, '"');
    ts = luaS_newlstr(D->H, luaZ_buffer(b), luaZ_bufflen(b));
    str = getstr(ts);
    len = ts->tsv.len;
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


static void dumpexp2 (DecompState *D, FuncState *fs, ExpNode *exp,
                      unsigned int limit);


/*
** dumps an operand for a pending binary or unary operation
*/
static void dumpexpoperand2 (DecompState *D, FuncState *fs, ExpNode *operand,
                             ExpNode *op, unsigned int limit) {
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


static void dumpcloseparen2 (DecompState *D, FuncState *fs, ExpNode *exp) {
  int saveline = exp->line;
  lua_assert(exp->line <= exp->closeparenline);
  exp->line = exp->closeparenline;
  /* the line that the closing paren is on matters when matching */
  checklineneeded2(D,fs,exp);
  exp->line = saveline;
  DumpLiteral(")",D);
}


static void dumphashitem2 (DecompState *D, FuncState *fs, ExpNode *exp) {
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
  FuncState *fs;
  ExpNode *lastexp;  /* the last ExpNode that was dumped */
  ExpNode *nextexp;  /* the next ExpNode in the list */
  int firstreg;  /* the start register of the expression list */
};


/*
** initializes an expression list iterator, starting at FIRSTREG with FIRSTEXP
*/
static void initexplistiter2 (struct ExpListIterator *iter, FuncState *fs,
                              int firstreg, ExpNode *firstexp) {
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
static ExpNode *getnextexpinlist2 (struct ExpListIterator *iter, int i) {
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
** calculate the last line for a constructor argument in a function call
*/
static void calcconsargauxline (FuncState *fs, ExpNode *call, ExpNode *tab) {
  DecompState *D = fs->D;
  lua_assert(call->kind == ECALL);
  lua_assert(tab->kind == ECONSTRUCTOR);
  if (call->aux > call->line)
    tab->aux = call->aux;
  else if (tab->u.cons.nextpc != -1) {
    /* nextpc is this call, you want the one after it */
    int line = getstartline(fs, tab->u.cons.nextpc+1);
    if (line > D->linenumber)
      tab->aux = line-1;
  }
}


/*
** dumps an expression node to the output
*/
static void dumpexp2 (DecompState *D, FuncState *fs, ExpNode *exp,
                      unsigned int limit) {
  /* NEEDPARENFORLINEINFO is for using extra parens to preserve line info */
  int needparenforlineinfo = (exp->line != exp->closeparenline);
  struct HoldItem holdparen, holdleft;
  lua_assert(exp != NULL);
  lua_assert(exp->pending);
  exp->pending = 0;
  if (exp->forceparen)
    needparenforlineinfo = 1;
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
      /* b is set to (-1) to tell this function to use `bindex' instead to
         index the pending expression in the expression stack */
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
         with the close paren emitted on a later line than the second operand
         to preserve line mappings when recompiling the output. Here is an
         example of what I'm talking about:
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
            if (firstitem == nextarrayitem && firstitem->kind == ECONSTRUCTOR){
              if (firstitem->line > bracketline && D->lastline == bracketline)
                bracketline = firstitem->line;
            }
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
          if (D->matchlineinfo && !linestep) {
            ExpNode *nextitem = dumparray ? nextarrayitem : nexthashitem;
            lua_assert(nextitem);
            /* in the case of a table with only hash items, check for each item
               if a new line will happen, in which case the indent level should
               increase */
            if (nextitem->line > D->lastline) {
              linestep = 1;
              D->indentlevel++;
            }
          }
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
                   close brace, see if it can be written one line earlier,
                   which is usually the case in source code */
                if (exp->aux && nextarrayitem->line == exp->aux) {
                  /* check if this array item would have only generated code
                     after the parser advanced to the close brace line */
                  if (issimplelistfield(nextarrayitem) &&
                      nextarrayitem->line - 1 > D->linenumber) {
                    nextarrayitem->closeparenline = --nextarrayitem->line;
                  }
                }
              }
              /* check if an expression which normally has multiple returns
                 only has 1 slot here, which means it is wrapped in parens */
              if (hasmultretsinglereg(nextarrayitem))
                nextarrayitem->forceparen = 1;
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
        if (D->matchlineinfo && ishashtable(exp) && exp->aux == 0) {
          int nextpc = exp->u.cons.nextpc;
          if (nextpc == fs->f->sizecode-1 && fs->prev == NULL)
            /* table ends on the final return line in the main function */
            exp->aux = getline(fs->f, nextpc);
          else if (nextpc != -1) {
            int nextline = getstartline(fs, nextpc);
            /* see if ending it on the next line makes sense */
            if (linestep && D->linenumber+1 < nextline)
              exp->aux = D->linenumber+1;
          }
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
      int noparen = 0;  /* true if emitting a single constructor/string
                           argument and not wrapping it in parentheses */
      struct ExpListIterator iter;
      ExpNode *firstexp;
      int narg = exp->u.call.narg;
      int i;
      firstexp = index2exp(fs, exp->previndex);
      lua_assert(firstexp != NULL);  /* there must be an expression to call */
      /* if the called expression is also a call, calculate the line to emit
         the close paren for that call */
      if (D->matchlineinfo && firstexp->kind == ECALL) {
        if (firstexp->aux == 0)
          firstexp->aux = exp->line;
        else if (firstexp->aux != exp->line)
          firstexp->closeparenline = exp->line;
      }
      if (needparenforlineinfo)
        addliteralholditem2(D, &holdparen, "(", 0);
      dumpexp2(D, fs, firstexp, SUBEXPR_PRIORITY);  /* called expression */
      narg -= (firstexp->kind == ESELF);
      if (narg != 0) {
        /* initialize the expression list iterator for dumping arguments */
        initexplistiter2(&iter, fs, firstexp->info+1+(firstexp->kind == ESELF),
                         firstexp);
        if (firstexp->kind == ESELF) {
          D(printf("firstexp->auxlistnext = %d\n", firstexp->auxlistnext));
        }
        if (D->matchlineinfo && narg == 1) {
          ExpNode *firstarg = getnextexpinlist2(&iter, 0);
          /* if only 1 argument, check if it is a string */
          if (firstarg->kind == ELITERAL && ttisstring(firstarg->u.k)) {
            /* Here I'm checking if the called expression maps to a different
               line than the OP_CALL code, and that OP_CALL maps to the same
               line as the first argument, which is a string. In this case, the
               source code had a long string which was not wrapped in
               parentheses, which creates this unusual line-mapping */
            if (firstexp->line != exp->line && firstarg->line == exp->line) {
              noparen = 1;
              emitlongstring2(firstarg,D);
              narg = 0;
              firstarg->pending = 0;
            }
          }
          else if (firstarg->kind == ECONSTRUCTOR) {
            /* if the argument is a table constructor and the mapped line for
               CALL is not the same as the line for the called expression, do
               not wrap the table in parentheses and also move up the start
               line of the table to be that of the called expression, ensuring
               the line info will match on recompilation; see `funcargs' in
               lparser.c for more insight on how line mapping works in this
               case */
            if (firstexp->line != exp->line) {
              firstarg->line = firstarg->closeparenline = exp->line;
              noparen = 1;
            }
            else if (firstarg->aux <= 0) {
              calcconsargauxline(fs, exp, firstarg);
            }
          }
        }
        if (noparen == 0) {
          DumpLiteral("(",D);  /* start of arguments */
          D->needspace = 0;
        }
        for (i = 0; i < narg; i++) {
          ExpNode *arg = getnextexpinlist2(&iter, i);
          if (i != 0)
            DumpLiteral(",",D);
          if (i == narg-1) {
            if (D->matchlineinfo && arg->kind == ECONSTRUCTOR && arg->aux <= 0)
              calcconsargauxline(fs, exp, arg);
          }
          dumpexp2(D, fs, arg, 0);
        }
      }
      else if (noparen == 0) {
        DumpLiteral("(",D);  /* start of arguments */
        D->needspace = 0;
      }
      if (noparen == 0) {
        if (exp->aux > D->linenumber)
          updateline2(fs, exp->aux, D);
        DumpLiteral(")",D);  /* end of arguments */
      }
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
      D(printf("nexps = %d (firstindex = %d, lastindex = %d)\n", nexps,
                firstindex, exp->u.concat.lastindex));
      lua_assert(nexps >= 2);
      for (i = 1; i < nexps; i++) {
        CheckSpaceNeeded(D);
        DumpBinOpr(OPR_CONCAT,D);
        D->needspace = 1;  /* space between `..' and next expression */
        dumpexp2(D, fs, getnextexpinlist2(&iter, i),priority[OPR_CONCAT].left);
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
static void dischargefromreg2 (FuncState *fs, int reg, unsigned int priority) {
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
static void setreglocal2 (FuncState *fs, int reg, struct LocVar *var) {
  lua_assert(isregvalid(fs, reg));
  lua_assert(!test_reg_property(fs, reg, REG_LOCAL));
  unset_reg_property(fs, reg, REG_PENDING);
  set_reg_property(fs, reg, REG_LOCAL);
  getslotdesc(fs, reg)->u.locvar = var;
}


static void commitlocalvars2 (FuncState *fs, int base, int n) {
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
static void pushlocalvars2 (FuncState *fs, int base, int n) {
  lua_assert(n > 0);
  addlocalvars2(fs, n);
  commitlocalvars2(fs, base, n);
}


/*
** register N for-loop control variables
*/
static void commitcontrolvars2 (FuncState *fs, int base, int n) {
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
static void emitcallstat2 (FuncState *fs, ExpNode *call) {
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
static void emitbreakstat2 (FuncState *fs, int pc) {
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
static void emitretstat2 (FuncState *fs, int pc, int reg, int nret) {
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
      if (last != NULL) {
        if (line != expline)
          last->closeparenline = line;
        if (hasmultretsinglereg(last))
          last->forceparen = 1;
        /* if this is a call and it has multiple returns, ensure it is not
           wrapped in parens */
        else if (last->kind == ECALL) {
          /* ensure no parentheses while still matching line info */
          last->closeparenline = last->line;
          last->aux = line;
        }
      }
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
static void emitresidualexp2 (FuncState *fs, int reg, ExpNode *lastexp) {
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
     preserving line info: notice the OP_MOVE is mapped to line 3 because of
     the final close paren being on that line. Because the expression generated
     from OP_MOVE and the expression generated from OP_DIV are not related to
     each other, the line-mapping differences have to be accounted for here,
     and handled before dumping the final expression */
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
        if (firstexp->line > exp->line)
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

static void
addstr2buff (hksc_State *H, Mbuffer *b, const char *str, size_t len) {
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


/*
** `LHSStringBuilder' is an auxiliary structure used by `dischargestores2' to
** ensure all L-values are emitted in the correct order and with even spacing
** between items
*/
struct LHSStringBuilder {
  hksc_State *H;
  FuncState *fs;
  DecompState *D;
  Mbuffer *buff;
  int needspace;
  unsigned int currpriority;
  lu_byte empty;
};


static void pfn_addk2buff (const char *s, size_t l, void *ud) {
  struct LHSStringBuilder *sb = ud;
  addstr2buff(sb->H, sb->buff, s, l);
}


static void initstringbuilder2 (struct LHSStringBuilder *sb, DecompState *D) {
  sb->H = D->H;
  sb->fs = D->fs;
  sb->D = D;
  sb->buff = &D->buff;
  sb->needspace = 0;
  sb->currpriority = 0;
  sb->empty = 1;
  luaZ_resetbuffer(sb->buff);
}


static void addtolhsbuff2 (struct LHSStringBuilder *sb, const char *str,
                          size_t len) {
  lua_assert(len != 0);
  if (sb->needspace)
    addliteral2buff(sb->H, sb->buff, " ");
  addstr2buff(sb->H, sb->buff, str, len);
  sb->needspace = 1;
  sb->empty = 0;
}


static void flushlhsbuff2 (struct LHSStringBuilder *sb) {
  luaZ_resetbuffer(sb->buff);
  sb->needspace = 0;
  sb->empty = 1;
}


static void addkvalue2lhs (struct LHSStringBuilder *sb, TValue *o) {
  if (sb->empty == 0 && sb->needspace)
    addliteral2buff(sb->H, sb->buff, " ");
  luaO_printk(o, pfn_addk2buff, sb, '"');
  sb->needspace = 1;
  sb->empty = 0;
}


static void addchartolhs2 (struct LHSStringBuilder *sb, int c) {
  char x = (char)c;
  lua_assert(c != 0);
  addtolhsbuff2(sb, &x, 1);
}

#define addvarnametolhs2(sb,name) addtstolhs2(sb,name)
static void addtstolhs2 (struct LHSStringBuilder *sb, TString *ts) {
  lua_assert(ts != NULL);
  addtolhsbuff2(sb, getstr(ts), ts->tsv.len);
}


static void addcommatolhs2 (struct LHSStringBuilder *sb) {
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


static void completelhsbuilder2 (struct LHSStringBuilder *sb) {
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


static void addregtolhs2 (struct LHSStringBuilder *sb, int reg) {
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


static void addktolhs2 (struct LHSStringBuilder *sb, int k) {
  lua_assert(ISK(k));
  addkvalue2lhs(sb,&sb->fs->f->k[INDEXK(k)]);
}


static void addindextolhs2 (struct LHSStringBuilder *sb, int reg, int isfield){
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
                                   `[', than also omit space between `[' and
                                   the index */
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


static void addtab2funcname (struct LHSStringBuilder *sb, ExpNode *exp) {
  FuncState *fs = sb->fs;
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
      addchartolhs2(sb, '.');
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


static TString *buildfuncname (struct LHSStringBuilder *sb, ExpNode *exp,
                               int needself) {
  FuncState *fs = sb->fs;
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
        return
        luaS_newlstr(fs->H,luaZ_buffer(sb->buff),luaZ_bufflen(sb->buff));
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
      return luaS_newlstr(fs->H, luaZ_buffer(sb->buff),luaZ_bufflen(sb->buff));
    }
  }
  return NULL;
}


#define checkdischargestores2(sa,fs) if((sa)->laststore)dischargestores2(sa,fs)


/*
** dumps the pending assignment list (check if there are pending stores before
** calling)
*/
static void dischargestores2 (StackAnalyzer *sa, FuncState *fs) {
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
      }
      src->u.cl.name = funcname;
      lastsrcreg = (exp->kind == ESTORE) ? exp->u.store.srcreg : exp->info;
    }
  }
  if (funcname == NULL) {
    for (;;) {
      OpCode rootop;  /* opcode family for the current store */
      ExpNode *prev;
      rootop = exp->u.store.rootop;
      if (exp->kind != ESTORE || rootop == OP_MOVE) {
        /* assigning to local variable */
        D(printf("OP_MOVE from reg %d\n", exp->info));
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
  lastsrc->line = 0;
  lastsrc->closeparenline = 0;
  D(printf("lastsrcreg = %d\n", lastsrcreg));
  for (;;) { /* tarverse the chain to dump RHS values */
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
        if (hasmultretsinglereg(src))
          src->forceparen = 1;
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
        dummy.forceparen = 0;
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
static void emitlocalstat2 (FuncState *fs, int nvars, int pc) {
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
      /* use the actual local variable name as the function name because
         p->name is clamped to 512 characters */
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
       which has already been handled and for which no pending expression
       exists anymore  */
    if (firstexp == NULL) {
      firstexp = &dummy;
      firstexp->kind = ENIL;
      firstexp->line = D->linenumber;
      firstexp->closeparenline = firstexp->line;
      firstexp->info = firstreg;
      firstexp->aux = lastreg;
      firstexp->forceparen = 0;
      lastexp = firstexp;
    }
    /*lua_assert(firstexp == checkexpinreg2(fs, firstreg));*/
    lua_assert(firstexp->info == firstreg);
    if (D->matchlineinfo && firstexp->kind == ENIL && firstexp->aux == lastreg)
    {
      if (pc+1 < fs->f->sizecode-1 &&
          getstartline(fs, pc+1) == firstexp->line &&
          D->linenumber <= firstexp->line - 1) {
        haveRHS = 0;
        if (D->linenumber < firstexp->line - 1) {
          skipsemiforlineinfo = 1;
          firstexp->closeparenline = --firstexp->line;
        }
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
  D(printf("firstreg = %d, lastreg = %d\n", firstreg, lastreg));
  for (i = firstreg; i <= lastreg; i++) {
    ExpNode *exp = getexpinreg2(fs, i); /* the pending expression in REG */
    setreglocal2(fs, i, getlocvar2(fs, fs->nactvar++));
    if (haveRHS == 0) {
      if (exp) exp->pending = 0;
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
        seenfirstexp = 1;
      }
      else {
        DumpLiteral("UNHANDLED CASE in initlocvar2\n",D);
      }
    }
  }
  if (seenfirstexp == 0) {
    /* no expressions have been dumped, but the items in the hold need to be
       discharged and the line needs to be updated */
    D->needspace = 1;
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
** holds an active local variable or appending the new expression to the
** pending chain
*/
static ExpNode *addexptoreg2 (StackAnalyzer *sa, FuncState *fs, int reg,
                              ExpNode *exp, int *splitnil) {
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
         only temporary registers, ensuring the recursive call does not get
         here */
      int laststorereg = exp->aux < fs->nactvar ? exp->aux : fs->nactvar;
      if (fs->D->matchlineinfo)
        nextline = getstartline(fs, sa->pc+1);
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
static void dischargenildebt2 (StackAnalyzer *sa, FuncState *fs, int reg) {
  ExpNode *exp;
  lua_assert(sa->openexprnildebt > 0);
  exp = newexp(fs);
  exp->kind = ENIL;
  exp->previndex = exp->auxlistprev = exp->auxlistnext = 0;
  exp->info = reg;
  exp->aux = exp->info + sa->openexprnildebt-1;
  if (fs->D->matchlineinfo) {
    exp->line = getstartline(fs, sa->pc);
    exp->closeparenline = exp->line;
  }
  else if (GET_OPCODE(fs->f->code[sa->pc]) != OP_CLOSURE) {
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


static void initexp2 (FuncState *fs, ExpNode *exp, int reg, int pc) {
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
  exp->forceparen = 0;
}


static void linkexp2 (StackAnalyzer *sa, FuncState *fs, ExpNode *exp) {
  ExpNode *prevreg = index2exp(fs, exp->auxlistprev);
  sa->lastexpindex = exp2index(fs, exp);
  if (prevreg) {
    prevreg->auxlistnext = sa->lastexpindex;
    if (prevreg->kind == ECONSTRUCTOR && prevreg->aux == 0)
      prevreg->u.cons.firstarrayitem = sa->lastexpindex;
  }
}


static ExpNode *addboolexp2 (StackAnalyzer *sa, FuncState *fs, int reg, int pc,
                             int b) {
  ExpNode *exp = newexp(fs);
  initexp2(fs, exp, reg, pc);
  exp->kind = b ? ETRUE : EFALSE;
  linkexp2(sa, fs, exp);
  return exp;
}


static ExpNode *addnilexp2 (StackAnalyzer *sa, FuncState *fs, int reg, int pc){
  ExpNode *exp = newexp(fs);
  initexp2(fs, exp, reg, pc);
  exp->kind = ENIL;
  linkexp2(sa, fs, exp);
  return exp;
}


static ExpNode *addexp2 (StackAnalyzer *sa, FuncState *fs, int pc, OpCode o,
                         int a, int b, int c, int bx) {
  ExpNode node;
  ExpNode *exp = &node;
  const Proto *f = fs->f;
  if (o == OP_DATA)
    return NULL;
  lua_assert(ispcvalid(fs, pc));
  initexp2(fs, exp, a, pc);
  switch (o) {
    case OP_GETGLOBAL: case OP_GETGLOBAL_MEM:
      exp->kind = EGLOBAL;
      /* todo: if generating variable names, make sure this global name doesnt
         conflict with any of the variable names generated so far, create a
         test like the following and do a no-debug test:
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
#ifdef LUA_DEBUG
      exp->u.cons.arrsize = luaO_fb2int(b);
      exp->u.cons.hashsize = c > 0 ? luaO_fb2int(c-1)+1 : 0;
#endif /* LUA_DEBUG */
      exp->u.cons.nextpc = -1;
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
      if (fs->D->matchlineinfo)
        exp->line = exp->closeparenline = exp->u.cl.p->linedefined;
      break;
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
      else if (exp->u.call.narg > 0) {
        ExpNode *lastexp = index2exp(fs, sa->lastexpindex);
        /* check if the last argument has multiple returns but only the first
           slot is used, in this case, it must be wrapped in parens */
        if (lastexp != NULL && hasmultretsinglereg(lastexp))
          lastexp->forceparen = 1;
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
      /* if matching line info, see if the close-paren of the function call
         needs to be on a specific line */
      if (fs->D->matchlineinfo) {
        if (fs->prev == NULL && pc+1 == fs->f->sizecode-1) {
          /* call is before final return of main function */
          exp->aux = getline(fs->f, pc+1);
        }
        else if (IS_OP_TAILCALL(o)) {
          lua_assert(GET_OPCODE(fs->f->code[pc+1]) == OP_RETURN);
          exp->aux = getline(fs->f, pc+1);
        }
      }
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
reducecondition2 (StackAnalyzer *sa, FuncState *fs, int e1, int e2, int reg,
                  int pc) {
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
static ExpNode *addcompare2 (StackAnalyzer *sa, FuncState *fs, int pc, int reg,
                             Instruction i, lu_byte inverted) {
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
    int operands[2];
    if (istempreg(fs, b) && istempreg(fs, c)) {
      if (b > c) {
        int temp = b; b = c; c = temp;
        a = !a;
      }
    }
    else if (!test_ins_property(fs, pc, INS_SKIPPEDREF) &&
             getkoperands(o, b, c, b, operands)) {
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


static int isfailjump (FuncState *fs, int pc) {
  return (test_ins_property(fs, pc, INS_BRANCHFAIL) ||
          test_ins_property(fs, pc, INS_LOOPFAIL));
}

static int ispassjump (FuncState *fs, int pc) {
  return (test_ins_property(fs, pc, INS_BRANCHPASS) ||
          test_ins_property(fs, pc, INS_LOOPPASS));
}


static int iscondreducible (ExpNode *e1, ExpNode *e2, int golabel) {
  lua_assert(e2 != NULL);
  if (e1 == NULL)
    return 0;
  if (e1->endlabel == golabel)
    return 1;
  if (/*e1->goiftrue == e2->goiftrue && */e1->endlabel == e2->endlabel)
    return 1;
  return 0;
}

static void setpendingcondtarget (StackAnalyzer *sa, FuncState *fs, int label){
  /* if jumping past an augmented repeat-loop break, set the actual target to
     be the end of the loop, as this condition is the footer condition, and
     should not be finalized until then */
  if (!ispcvalid(fs, label-1) || !test_ins_property(fs, label-1, INS_AUGBREAK))
    sa->pendingcond.target = label;
  else
    sa->pendingcond.target = label+2;
}


/*
** update the pending conditional expression after a new jump; only call when
** encountering a jump inside a conditional expression
*/
static void addconditionalnode2 (StackAnalyzer *sa, FuncState *fs, int pc,
                                 int jumptarget) {
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
        /* see which bool label is targeted; if false, then this is a
           go-if-true node, e.g. `a == b and 1', because the boolean result of
           the first node will be used only if it is false */
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
           known until now since OP_NOT followed by OP_TEST will be optimized
           by inverting OP_TEST and not emitting OP_NOT; an extra unary `not'
           node will be added here if needed */
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
  setpendingcondtarget(sa, fs, exp->endlabel);
  {
    ExpNode *prevnode;
    int golabel = skipsboollabel ? jumptarget : pc+1;
    addnode:
    prevnode = index2exp(fs, sa->pendingcond.e);
    if (iscondreducible(prevnode, exp, golabel)) {
      if (exp->info == -1 && prevnode->info != -1)
        reg = exp->info = prevnode->info;
      /* combine the current pending node and this new node as the 2 operands
         of a logical operation */
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
    /* AUGMENT_DEST is true if this condition is part of an assignment where
       the last operand in the conditional expression is the destination
       variable, for example:
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
finalizeconditionalnode2 (StackAnalyzer *sa, FuncState *fs, int pc) {
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
  setpendingcondtarget(sa, fs, prevtarget);
  return exp1;
}


static int addstore2 (StackAnalyzer *sa, FuncState *fs, int pc, OpCode o,
                      int a, int b, int c, int bx) {
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


static ExpNode *addhashitem2 (FuncState *fs, int pc, OpCode o, int a, int b,
                              int c) {
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
      exp->line = srcexp->line;
    exp->aux = exp2index(fs, srcexp);
  }
  exp->closeparenline = exp->line;
  exp->previndex = exp->auxlistprev = exp->auxlistnext = 0;
  exp->leftside = 0;
  exp->pending = 0;
  exp->u.store.srcreg = srcreg;
  return exp;
}


static int openexpr2 (StackAnalyzer *sa, FuncState *fs) {
  ExpNode *result;
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
        D(printf("created new expression node\n"));
        D(printf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(printf("---------------------------\n"));
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
              /* use AUXLISTPREV to hold the index of the pending key
                 expression */
              exp->auxlistprev = exp2index(fs, getexpinreg2(fs, b));
            }
            if (istempreg(fs, reg)) {
              setfirstfree(fs, reg);
              /* if the last used reg holds the current table constructor, then
                 reset its firstarrayitem as it now points to a free register
                 */
              if (reg - 1 == a) {
                tab->u.cons.firstarrayitem = 0;
              }
            }
          }
          if (exp->aux) {
            ExpNode *value = index2exp(fs, exp->aux);
            int line = getexpline(value);
            if (line != exp->line && line != 0 && value->aux > 0)
              value->closeparenline = exp->line;
            else if (value->aux == 0)
              value->aux = exp->line;
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
  result = getexpinreg2(fs, e->firstreg);
  if (result && result->kind == ECONSTRUCTOR) {
    /* hashtable expressions don't have a mapped end line because there is no
       OP_SETLIST code emitted for them; in this case, the next pc is saved so
       its line number can be used as a limit on where the table can end */
    result->u.cons.nextpc = e->endpc+1;
  }
  return limit - e->startpc;
}


static int clamptoreg (int base, int n, int limit) {
  int res = base+n;
  if (res > limit)
    return limit-base;
  return n;
}


static int getnvarstartatpc2 (FuncState *fs, int pc, int reglimit) {
  if (ispcvalid(fs, pc)) {
    int nvars = varstartsatpc2(fs, pc);
    if (nvars == 0) return 0;
    return clamptoreg(fs->nactvar, nvars, reglimit);
  }
  return -1;
}


static ExpNode *getforloopbasenode2 (StackAnalyzer *sa, FuncState *fs,
                                     BlockNode *node, int base, int bias) {
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
dumpfornumheader2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
  DecompState *D = fs->D;
  struct ExpListIterator iter;
  struct HoldItem initvar, initeq;
  struct HoldItem *loopheader = sa->currheader;
  struct LocVar *var;
  int startline;
  /* get the base control register */
  const int base = GETARG_A(fs->f->code[node->endpc]);
  /* get the pending initial expression before creating the control variables
  */
  ExpNode *exp = getforloopbasenode2(sa, fs, node, base, 0);
  /* create the control variables */
  commitcontrolvars2(fs, base, 3);
  /* create the loop variable */
  pushlocalvars2(fs, base+3, 1);
  var = getlocvar2(fs, base+3);
  /* emit `for', either now or as hold item depending on if its start line is
     fixed */
  if (D->matchlineinfo && (startline = getforloopstartline(fs, node))) {
    updateline2(fs, startline, D);
    CheckSpaceNeeded(D);
    DumpLiteral("for",D);
    D->needspace = 1;
  }
  else {
    addliteralholditem2(D, loopheader, "for", 1);
  }
  /* add hold items for for-loop header */
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
dumpforlistheader2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
  hksc_State *H = fs->H;
  DecompState *D = fs->D;
  struct ExpListIterator iter;
  struct HoldItem *header = sa->currheader;
  int startline;
  Mbuffer *b = &D->buff;
  /* get the base control register */
  const int base = GETARG_A(fs->f->code[node->endpc-1]);
  /* get the number of expressions to list */
  const int nexp = (fs->firstfree - base) >= 3 ? (fs->firstfree - base) : 3;
  const int bias = nexp - 3;
  const int lastvar = base + 3 + sa->numforloopvars - 1;
  /* get the pending initial expression before creating the control variables
  */
  ExpNode *exp = getforloopbasenode2(sa, fs, node, base, bias);
  int i, lastreg = base + nexp - 1;
  /* create the control variables */
  commitcontrolvars2(fs, base, 3);
  /* create the loop variables */
  pushlocalvars2(fs, base+3, sa->numforloopvars);
  /* create the for-loop header string */
  luaZ_resetbuffer(b);
  /* emit `for', either now or as hold item depending on if its start line is
     fixed */
  if (D->matchlineinfo && (startline = getforloopstartline(fs, node))) {
    updateline2(fs, startline, D);
    CheckSpaceNeeded(D);
    DumpLiteral("for",D);
    D->needspace = 1;
  }
  else {
    addliteral2buff(H, b, "for ");
  }
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


static void
updateheaderline2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
  DecompState *D = fs->D;
  int line = D->linenumber;
  int nextline;
  if (D->matchlineinfo) {
    if (node->kind == BL_FUNCTION && issinglelinefunc(fs, node)) {
      updateline2(fs, getline(fs->f, 0), D);
      return;
    }
    else
      nextline = getstartline(fs, node->startpc);
  }
  else
    nextline = D->nextlinenumber + sa->numblockstartatpc;
  if (nextline - sa->numblockstartatpc > line)
    line = nextline - sa->numblockstartatpc;
  updateline2(fs, line, D);
}


static void
dumpbranchheader2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
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
    nodesetflag(node, ISELSEIF);
    if (haselsepart(node))
      nodesetflag(node->nextsibling, ISELSEIF);
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
  if (D->matchlineinfo && haselsepart(node) && node->startpc == node->endpc)
    /* if there are not instructions in the if-part, advance to the line of
       the true-exit jump before emitting `then' to match line info */
    updateline2(fs, getline(fs->f, node->endpc), D);
  DumpLiteral("then", D);
  D->needspace = 1;
  flushpendingexp2(fs);
}


static void
dumpwhilestatheader2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
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
        D(printf("created new expression node\n"));
        D(printf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(printf("---------------------------\n"));
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


static void dumpheaderstring2 (StackAnalyzer *sa, FuncState *fs,
                               BlockNode *node, const char *str) {
  DecompState *D = fs->D;
  updateheaderline2(sa, fs, node);
  CheckSpaceNeeded(D);
  DumpString(str, D);
  D->needspace = 1;
  D->nextlinenumber++;
}


static void
dumpfuncheader2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
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


static void calcnumblockstartatpc2 (StackAnalyzer *sa, BlockNode *node) {
  int pc = sa->pc;
  int n = 1;
  while((node = node->firstchild) != NULL && node->startpc == pc)
    n++;
  sa->numblockstartatpc = n;
}


static void enterblock2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node,
                         struct HoldItem *blockheader) {
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
           line as the statement after it); if not matching line info, room is
           made for a line feed */
        if (!D->matchlineinfo || getstartline(fs, 0) > D->linenumber)
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


static int getlastblockline (const FuncState *fs, const BlockNode *node) {
  lua_assert(fs->D->matchlineinfo);
  /* while-loops either have their final jump mapped to the start line, like
     for-loops, or mapped to the lastline at the time END was encountered, so
     they do not have a distinct final line that needs to be matched here;
     only functions for for-list blocks always have a final opcode which has a
     distinct line-mapping that needs to be matched */
  switch (node->kind) {
    case BL_REPEAT: if (nodegetflag(node, REPUNTILTRUE)) return 0;
    /* fallthrough */
    case BL_FUNCTION: case BL_FORLIST: return getline(fs->f, node->endpc);
    default: break;
  }
  return 0;
}


static int calclastline2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
  DecompState *D = fs->D;
  int line, nextline;
  lua_assert(ispcvalid(fs, node->endpc+1));
  /* I have it so if the entire block has been on the same line so far, also
     emit the footer on the same line. In case this alone will not match line
     info, I added a check after the entire file is dumped to add a semi-colon
     on the last mapped line to ensure matching line info upon recompilation */
  if (D->linenumber == sa->currheaderline)
    return D->linenumber;
  nextline = getline2(fs, node->endpc+1);
  line = D->linenumber;
  if (D->matchlineinfo && GET_OPCODE(fs->f->code[node->endpc+1]) ==OP_TFORLOOP)
    nextline = getline(fs->f, node->endpc+2);
  if (line + (sa->deferleaveblock + 1) <= nextline) {
    /*if (inmainfunc && node->endpc+1 == fs->f->sizecode-1 &&
        sa->deferleaveblock < 1)
      line = nextline;
    else*/
      line = line + 1;
  }
  return line;
}


static int
dumprepeatfooter2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
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


static void leaveblock2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
  const char *str;
  DecompState *D = fs->D;
  int inmainfunc = (fs->prev == NULL);
  if (inmainfunc && node->kind == BL_FUNCTION) {
    D->indentlevel = 0;
    if (D->delaysemi > 0) {
      updateline2(fs, D->delaysemi, D);
      DumpLiteral("; --[[ emitted to match line info ]]",D);
      D->delaysemi = 0;
    }
    /* no tokens needed to leave the main function, just add a line-feed */
    beginline2(fs, 1, D);
    return;
  }
  /* when leaving an elseif-block, do not dump anything unless this is an
     if-block which has an else part; in that case, dump `else'; otherwise,
     the parent block will dump the correct token when leaving */
  if (nodegetflag(node, ISELSEIF) &&
      ((node->kind == BL_IF && !haselsepart(node)) || node->kind == BL_ELSE)) {
    D->indentlevel+= 1+(node->kind != BL_ELSE && haselsepart(node));
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
        int lastmappedline = getlastblockline(fs, node);
        int lastline = 0;
        if (istailblock(sa->currparent, node)) {
          int lastmappedparentline = getlastblockline(fs, sa->currparent);
          if (lastmappedparentline &&
              /* nested functions have `end' on their last line, which
                 guarantees that its final return maps the correct line */
              (inmainfunc || sa->currparent->kind != BL_FUNCTION))
            /* the last line for this block will be the last mapped line for
               its parent block, so that when recompiling, the LexState
               lastline field will be LASTMAPPEDPARENTLINE when encountering
               the end of the parent block, ensuring line info matches with the
               original, example:
                for x in a do
                  for y in b do
                    return;
                  end -- LASTMAPPEDPARENTLINE
                end -- when encountered, ls->lastline is LASTMAPPEDPARENTLINE
                 */
            lastline = lastmappedparentline;
          else if (sa->currparent->kind != BL_FUNCTION) {
            sa->deferleaveblock++;
            return;
          }
        }
        if (sa->deferleaveblock) {
          D->indentlevel += sa->deferleaveblock;
          do {
            updateline2(fs, calclastline2(sa, fs, node), D);
            CheckSpaceNeeded(D);
            DumpLiteral("end", D);
            D->needspace = 1;
            D->indentlevel--;
          } while (--sa->deferleaveblock);
        }
        /* calculate a valid LASTLINE if needed; in this case, LASTLINE does
           not need to be a particular value, as long as it does not overstep
           for the next line-mapping in the program */
        if (!lastline) lastline = calclastline2(sa, fs, node);
        /* if there is no token on LASTMAPPEDLINE, put a token there so it will
           be mapped to the next opcode instead of D->LASTLINE when recompiling
          */
        if (D->lastline < lastmappedline) {
          updateline2(fs, lastmappedline, D);
          DumpLiteral(";", D);
          lua_assert(D->lastline == lastmappedline);
        }
        updateline2(fs, lastline, D);
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


static void addparams (FuncState *fs, const Proto *f) {
  fs->nlocvars = f->numparams;
  if (f->numparams)
    pushlocalvars2(fs, 0, f->numparams);
}


static void checkdelaysemi2 (FuncState *fs, int pc) {
  DecompState *D = fs->D;
  if (D->matchlineinfo == 0 || fs->prev != NULL || D->delaysemi < 0)
    return;
  if (pc < fs->f->sizecode-1)
    pc++;
  if (pc == fs->f->sizecode-1 && pc > 0) {
    int retline = getline2(fs, pc);
    int lastline = getline2(fs, pc-1);
    if (retline != lastline)
      D->delaysemi = retline;
  }
}


#endif /* HKSC_DECOMP_HAVE_PASS2 */


/*
** Second pass `block node' handler - the function's registers and stack are
** analyzed and local variables are detected. With information about the local
** variables, any undetected do-end blocks can be detected in this pass.
*/
static void blnode2 (StackAnalyzer *sa, FuncState *fs, BlockNode *node) {
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
  lua_assert(nodegetflag(node, VISITED) == 0);
  nodesetflag(node, VISITED);
  fs->seenstatinblock = 0;
  D->indentlevel++;
  if (nodegetflag(node, EMPTY)) /* block has no instructions */
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
        /* emit the local statement for the parent nil variables before
           entering the child block */
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
      waschildempty = nodegetflag(nextchild, EMPTY);
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
    (void)a; (void)b; (void)c; (void)sbx;
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
        if (sa->pc == nextchildstartpc)
          goto processnextchild;
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
    if (testAMode(o) || o == OP_DATA) { /* A is a register */
      ExpNode *exp;
      lua_assert(!test_ins_property(fs, pc, INS_BREAKSTAT));
      if (sa->pc == node->startpc && node->parentnilvars)
        a += node->parentnilvars;
      exp = addexp2(sa, fs, pc, o, a, b, c, bx);
      if (exp != NULL) {
        int splitnil;
        D(printf("created new expression node\n"));
        D(printf("---------------------------\n"));
        debugexp(fs, exp,0);
        D(printf("---------------------------\n"));
        lua_assert(isregvalid(fs, exp->info));
        if (exp->kind != ESTORE)
          exp = addexptoreg2(sa, fs, a, exp, &splitnil);
        if (exp->kind == ECALL && exp->u.call.nret == 0)
          emitcallstat2(fs, exp);
        /* if the final return the next code and it is mapped to a different
           line than this expression, wrap this expression in parens and put
           the closing paren on the line that the return is mapped to; this
           preserves line info when recompiling */
        else if (fs->prev == NULL && node->kind == BL_FUNCTION &&
                 D->matchlineinfo && pc+1 == fs->f->sizecode-1 &&
                 exp->kind != ECLOSURE) {
          int retline = getline2(fs, pc+1);
          int expline = getexpline(exp);
          if (retline != expline && expline != 0) {
            if (splitnil) {
              lua_assert(exp->kind == ENIL);
              exp->line = exp->closeparenline = retline;
            }
            else {
              if (exp->kind == ECALL && (exp->aux == 0 || exp->aux == retline))
                exp->aux = retline;
              else
                D->delaysemi = retline;
            }
          }
        }
        if (pc+1 == sa->pendingcond.target) {
           finalizeconditionalnode2(sa, fs, pc);
        }
        if (numvars > 0) {
          checkdischargestores2(sa, fs);
          D(printf("NEW LOCAL VARIABLE\n"));
          (void)getfirstexp;
          emitlocalstat2(fs, numvars, pc);
        }
      }
      else {  /* an A-mode instruction that doesn't clobber A */
        checkdelaysemi2(fs, pc);
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
        else if (pc == endpc && (node->kind != BL_REPEAT ||
                                 nodegetflag(node, UPVAL))) {
          /* do nothing */
        }
        else if (test_ins_property(fs, pc, INS_AUGBREAK)) {
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
      else {
        checkdelaysemi2(fs, pc);
        checkdischargestores2(sa, fs);
      }
    }
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
    if (pc == endpc)
      break;
  }
  /* check for an empty child block at the end of this block */
  if (nextchildstartpc != -1) {
    lua_assert(nextchild != NULL);
    lua_assert(nodegetflag(nextchild, EMPTY) != 0);
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

static void freeblocktree (DecompState *D, BlockNode *tree) {
  BlockNode *node = tree->firstchild;
  while (node) {
    BlockNode *nextsibling = node->nextsibling;
    freeblocktree(D, node);
    node = nextsibling;
  }
  luaO_delnode(&D->blocknodes, tree);
}

static void pass2 (const Proto *f, FuncState *fs) {
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
      lua_assert(test_ins_property(fs, pc, INS_VISITED));
    checktreevisited(functionblock);
  }
#endif /* LUA_DEBUG */
  freeblocktree(fs->D, fs->root);
}


static void DecompileFunction (DecompState *D, const Proto *f) {
  FuncState *fs = open_func(D, f);
  pass1(D, fs);
  debugpass1summary(fs);
  pass2(f,fs);
  close_func(D);
}


static void MarkGlobals (const Proto *f) {
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


/*
** I use the `reserved' field of TString to encode marked globals, to make the
** decompiler easier to port to regular Lua, but it will interfere the parser
** if the globals are not unmarked.
*/
static void UnmarkGlobals (const Proto *f) {
  int i;
  for (i = 0; i < f->sizek; i++) {
    const TValue *o = &f->k[i];
    if (ttype(o) == LUA_TSTRING) {
      TString *ts = rawtsvalue(o);
      /* max value of a reserved word is NUM_RESERVED */
      if (ts->tsv.reserved > NUM_RESERVED) {
        D(printf("unmarking global '%s'\n", getstr(ts)));
        ts->tsv.reserved = 0;
      }
    }
  }
}


static void prescan_func (DecompState *D, const Proto *f) {
  int i;
  if (++D->funcidx > D->maxtreedepth)
    D->maxtreedepth = D->funcidx;
  if (D->usedebuginfo == 0)
    MarkGlobals(f);
  for (i = 0; i < f->sizep; i++)
    prescan_func(D, f->p[i]);
  --D->funcidx;
}


static void postscan_func (DecompState *D, const Proto *f) {
  int i;
  if (D->usedebuginfo == 0)
    UnmarkGlobals(f);
  for (i = 0; i < f->sizep; i++)
    postscan_func(D, f->p[i]);
}

static void alloc_fs_stack (DecompState *D) {
  int i;
  D->fs_stack = luaM_newvector(D->H, D->maxtreedepth, struct FuncState);
  for (i = 0; i < D->maxtreedepth; i++)
    D->fs_stack[i].used = 0;
}

static void free_fs_stack (DecompState *D) {
  int i;
  for (i = 0; i < D->maxtreedepth; i++) {
    FuncState *fs = D->fs_stack + i;
    if (fs->used) free_funcstate(D, fs);
  }
  luaM_freearray(D->H, D->fs_stack, D->maxtreedepth, struct FuncState);
  D->fs_stack = NULL;
}

/*
** Execute a protected decompiler.
*/
static void f_decompiler (hksc_State *H, void *ud) {
  DecompState *D = ud;
  const Proto *f = D->mainfunc;
  prescan_func(D, f);
  lua_assert(D->funcidx == 0);
  luaO_bitmapalloc(H, &D->kmap, 256);
  luaO_bitmapalloc(H, &D->upvalmap, LUAI_MAXUPVALUES);
  alloc_fs_stack(D);
  DecompileFunction(D, f);
  postscan_func(D, f);
}


/*
** dump Lua function as decompiled chunk
*/
int luaU_decompile (hksc_State *H, const Proto *f, lua_Writer w, void *data) {
  DecompState D = {0};
  int status;
#ifdef LUA_DEBUG
  luaprintf_state = H;
#endif
  D.H=H;
  D.mainfunc=f;
  D.writer=w;
  D.data=data;
  D.name = H->currinputname;
  D.indentlevel=-1;  /* incremented to 0 when entering the main function */
  D.linenumber = D.lastline = D.nextlinenumber = 1;
  D.usedebuginfo = (!Settings(H).ignore_debug && f->sizelineinfo > 0);
  D.matchlineinfo = (Settings(H).match_line_info && D.usedebuginfo);
  luaZ_initbuffer(H, &D.buff);
  luaZ_resetbuffer(&D.buff);
  luaO_initlist(&D.blocknodes, BlockNode);
  status = luaD_pcall(H, f_decompiler, &D);
  luaZ_freebuffer(H, &D.buff);
  free_fs_stack(&D);
  luaO_freelist(H, &D.blocknodes);
#define DEF_VEC(t,n) VEC_FREE(H, D.n);
  VEC_LIST
#undef DEF_VEC
  luaO_bitmapfree(H, &D.kmap);
  luaO_bitmapfree(H, &D.upvalmap);
  if (status) D.status = status;
  return D.status;
}

#endif /* HKSC_DECOMPILER */


/* print a string as it would appear in Lua code */
void luaO_printstring (const TString *ts,
                       void (*pfn) (const char *s, size_t l, void *ud),
                       void *ud, int quote) {
  char buff[64];
  size_t bufflen = 0;
  size_t i, n = ts->tsv.len;
#define PUTCHAR(c) do { \
  if (bufflen == sizeof(buff)) { \
    (*pfn)(buff, sizeof(buff), ud); \
    bufflen = 0; \
  } \
  buff[bufflen++] = (c); \
  buff[bufflen] = 0; \
} while (0)
  PUTCHAR(quote);
  for (i = 0; i < n; i++) {
    int c = getstr(ts)[i];
    if (c != quote && c != '\\' && isprint(c))
      PUTCHAR(c);
    else {
      PUTCHAR('\\');
      if (c == quote)
        PUTCHAR(quote);
      else switch (c) {
        case '\\': PUTCHAR('\\'); break;
        case '\a': PUTCHAR('a'); break;
        case '\b': PUTCHAR('b'); break;
        case '\f': PUTCHAR('f'); break;
        case '\n': PUTCHAR('n'); break;
        case '\r': PUTCHAR('r'); break;
        case '\t': PUTCHAR('t'); break;
        case '\v': PUTCHAR('v'); break;
        default:
          if (bufflen + 4 > sizeof(buff)) {
            (*pfn)(buff, bufflen, ud);
            bufflen = 0;
          }
          bufflen += sprintf(buff + bufflen, "%03u", cast(unsigned char, c));
      }
    }
  }
  PUTCHAR(quote);
  if (bufflen)
    (*pfn)(buff, bufflen, ud);
#undef PUTCHAR
}

/* print a constant value as it would appear in Lua code */
void luaO_printk (const TValue *o,
                  void (*pfn) (const char *s, size_t l, void *ud),
                  void *ud, int quote) {
  switch (ttype(o)) {
    char buff[LUAI_MAXNUMBER2STR+LUAI_MAXUI642STR+1];
    int n;
    char lsuf;
    lu_int64 ui64;
    case LUA_TNIL: (*pfn)("nil", 3, ud); break;
    case LUA_TBOOLEAN:
      if (bvalue(o)) (*pfn)("true", 4, ud);
      else (*pfn)("false", 5, ud);
      break;
#ifdef HKSC_VERSION
    case LUA_TLIGHTUSERDATA: {
      size_t hl = hlvalue(o);
      lsuf = 'i';
#ifndef LUA_UI64_S
      ui64 = cast(lu_int64, hl);
#else
      ui64.lo = hl & 0xffffffff;
      if (sizeof(size_t) * CHAR_BIT > 32)
        ui64.hi = hl >> 32;
      else
        ui64.hi = 0;
#endif /* LUA_UI64_S */
      goto printui64;
    }
#endif /* HKSC_VERSION */
    case LUA_TNUMBER:
      n = lua_number2str(buff, nvalue(o));
      (*pfn)(buff, n, ud);
      break;
    case LUA_TSTRING:
      luaO_printstring(rawtsvalue(o), pfn, ud, quote);
      break;
#ifdef HKSC_VERSION
    case LUA_TUI64:
      lsuf = 'l';
      ui64 = ui64value(o);
      printui64: n = lua_ui642str(buff + 2, ui64) + 2;
      buff[0] = '0', buff[1] = 'x';
      buff[n++] = 'h', buff[n++] = lsuf;
      buff[n] = 0;
      (*pfn)(buff, n, ud);
      break;
#endif /* HKSC_VERSION */
    default: lua_assert(0);
  }
}
