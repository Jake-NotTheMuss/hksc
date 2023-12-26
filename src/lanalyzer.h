/*
** $Id: lanalyzer.h $
** Auxiliary functions to manipulate function analyzer structures
** See Copyright Notice in lua.h
*/

#ifndef lanalyzer_h
#define lanalyzer_h

#ifdef HKSC_DECOMPILER

#include "lcode.h"
#include "lobject.h"

#if defined(ldecomp_c) || defined(lanalyzer_c)

/*
** basic block types
*/
#define BLTYPE_TABLE \
  DEFBLTYPE(FUNCTION)   /* a Lua function */      \
  DEFBLTYPE(WHILE)      /* a while-loop */        \
  DEFBLTYPE(REPEAT)     /* a repeat-loop */       \
  DEFBLTYPE(FORNUM)     /* a for-numeric-loop */  \
  DEFBLTYPE(FORLIST)    /* a for-list-loop */     \
  DEFBLTYPE(DO)         /* a block */             \
  DEFBLTYPE(IF)         /* an if-block */         \
  DEFBLTYPE(ELSE)       /* an else-block */

#define DEFBLTYPE(e)  BL_##e,
enum BLTYPE {
  BLTYPE_TABLE
  MAX_BLTYPE
};
#undef DEFBLTYPE


/*
** instruction properties
*/
#define INSFLAG_TABLE \
  DEFINSFLAG(LEADER)  /* instruction is a leader */ \
  DEFINSFLAG(BBSUBEXPR) \
  DEFINSFLAG(KLOCVAR)  /* local initialization that pushes a constant */ \
  DEFINSFLAG(BBLOCVAR) \
  DEFINSFLAG(FAILJUMP)  /* a jump-on-false */ \
  DEFINSFLAG(PASSJUMP)  /* a jump past a jump-on-false */ \
  DEFINSFLAG(BRANCHFAIL) /* false-jump in an if-statement condition */ \
  DEFINSFLAG(BRANCHPASS) /* true-jump in an if-statement condition */ \
  DEFINSFLAG(LOOPFAIL)  /* false-jump in a loop condition */ \
  DEFINSFLAG(LOOPPASS)  /* true-jump in a loop condition */ \
  DEFINSFLAG(REPEATSTAT)  /* first pc in a repeat-loop */ \
  DEFINSFLAG(WHILESTAT)  /* first pc in a while-loop */ \
  DEFINSFLAG(FORLIST)  /* first pc in a list for-loop */ \
  DEFINSFLAG(FORNUM)  /* first pc in a numeric for-loop */ \
  DEFINSFLAG(BLOCKEND)  /* last pc in a non-loop block */ \
  DEFINSFLAG(AUGBREAK)  /* augmented break in a repeat-loop with upvalues */ \
  DEFINSFLAG(LOOPEND)  /* last pc in a loop */ \
  DEFINSFLAG(BREAKSTAT)  /* pc is a break instruction */ \
  DEFINSFLAG(DOSTAT)  /* pc begins a block */ \
  DEFINSFLAG(BOOLLABEL)  /* an OP_LOADBOOL label */ \
  DEFINSFLAG(SKIPBOOLLABEL)  /* a jump over 2 bool labels */ \
  DEFINSFLAG(NILLABEL)  /* an OP_LOADNIL label */ \
  DEFINSFLAG(TESTSETJUMP)  /* a jump within a testset expression */ \
  DEFINSFLAG(BLOCKFOLLOW)  /* is a valid pc for `return' or `break' */ \
  DEFINSFLAG(LOCVAREXPR)  /* start of a local varible initialization */ \
  DEFINSFLAG(ASSIGNSTART)  /* start of a local statement or store */ \
  DEFINSFLAG(ASSIGNEND)  /* end of a local statement or store */ \
  DEFINSFLAG(SELFUPVAL)  /* OP_CLOSURE uses its own register as an upvalue */ \
  DEFINSFLAG(SKIPPEDREF)  /* pc skips a constant/upvalue reference */ \
  DEFINSFLAG(VISITED)  /* this instruction has been processed in pass2 */

#define DEFINSFLAG(e)  INS_##e,
enum INSFLAG {
  INSFLAG_TABLE
  MAX_INSFLAG
};
#undef DEFINSFLAG


/*
** register properties
*/
#define REGFLAG_TABLE \
  DEFREGFLAG(PENDING)  /* a register being used in a temporary expression */ \
  DEFREGFLAG(CONTROL)   /* a register which holds a loop control variable */ \
  DEFREGFLAG(LOCAL)     /* a register which holds an active local variable */ \
  DEFREGFLAG(UPVAL)     /* a register used as an upvalue */

#define DEFREGFLAG(e)  REG_##e,
enum REGFLAG {
  REGFLAG_TABLE
  MAX_REGFLAG
};
#undef DEFREGFLAG


typedef struct BlockNode {
  struct BlockNode *next;  /* next block */
  struct BlockNode *nextsibling;  /* next sibling block */
  struct BlockNode *firstchild;  /* first child block */
  int startpc;  /* startpc of the block */
  int endpc;  /* endpc of the block */
  short parentnilvars;
#ifdef LUA_DEBUG
  /* use the enum type when debugging so it's easy to see what kind it is */
  enum BLTYPE kind;  /* the type of the block */
#else
  /* save space otherwise */
  unsigned kind : 3;
#endif
  unsigned isempty : 1;  /* true if the block has zero instructions */
  unsigned upval : 1;
  unsigned iselseif : 1;
  unsigned hardstatbeforechild : 1;  /* used by first pass */
  unsigned repuntiltrue : 1;  /* used by first pass */
#ifdef LUA_DEBUG
  unsigned visited : 1;  /* has this block been visited in pass2 */
#endif
} BlockNode;


/*
** BlockState tracks the state of a pending do-block terminated by OP_CLOSE
*/
typedef struct BlockState2 {
  BlockNode *nextsibling;  /* the next sibling of this block */
  BlockNode *firstchild;  /* the first child of this block */
  BlockNode *prevsibling;  /* the previous sibling of this block */
  int nested;  /* nesting level within the current loop or function */
  int startpc;
  int endpc;
  unsigned reg : 15;  /* closed register */
  unsigned loop : 1;  /* true if the OP_CLOSE code was at the end of the loop */
} BlockState2;



typedef enum {
  VOIDPREP,  /* use this kind when you need to traverse an instruction sequence
                but not record it as an open expression */
  CALLPREP,  /* function-call preparation code */
  CONCATPREP,  /* concat preparation code */
  FORNUMPREP,  /* numeric for-loop preparation code */
  FORLISTPREP,  /* list for-loop preparation code */
  SETLISTPREP,  /* code evaluating items in a table constructor */
  HASHTABLEPREP,  /* code constructing a table with only hash items */
  EMPTYTABLE,  /* an empty table constructor */
  RETPREP  /* return statement preparation code */
} openexptype;

typedef struct OpenExpr {
  int startpc, endpc;
#ifdef LUA_DEBUG
  /* use the enum type when debugging so it's easy to tell what kind it is */
  openexptype kind;
  unsigned sharednil : 1;
  unsigned firstreg : 15;
#else
  /* save space otherwise */
  unsigned kind : 4;
  unsigned sharednil : 1;
  unsigned firstreg : 11;  /* need at least 8 bits */
#endif
} OpenExpr;


/*
** Expression descriptor
*/

typedef enum {
  EVOID,  /* no value */
  ENIL,  /* `nil' */
  ETRUE,  /* `true' */
  EFALSE,  /* `false' */
  EVARARG,  /* `...' */
  ELITERAL,  /* a constant number or string */
  ECONSTRUCTOR,  /* a table constructor */
  ECLOSURE,  /* a Lua function */
  ELOCAL,  /* a local variable */
  EUPVAL,  /* an upvalue */
  EGLOBAL,  /* a global variable */
  EINDEXED,  /* a table index */
  ESELF,  /* a table index called as a method */
  EBINOP,  /* a binary operation */
  EUNOP,  /* a unary operation */
  ECALL,  /* a function call */
  ECONCAT,  /* a concatenation */
  ECONDITIONAL,  /* a conditional expression */
  ESTORE  /* encodes an L-value in an assignment list */
} expnodekind;


typedef struct ExpNode {
  expnodekind kind;
  union {
    TValue *k;  /* constant value */
    TString *name;  /* variable name */
    int token;  /* token ID, e.g. TK_TRUE for `true' */
    struct {
      int arrsize, hashsize;
      int firstarrayitem, firsthashitem, lasthashitem;
      int narray, nhash;
    } cons;  /* table constructor */
    struct {
      const Proto *p;
      TString *name;  /* non-NULL if the function needs to be named */
      int haveself;
    } cl;  /* Lua closure */
    struct {
      int e1, e2;  /* exp indices of operands */
      lu_byte goiftrue;  /* OPR_AND if 1, else OPR_OR */
    } cond;  /* condition expression */
    struct {
      int b, c;  /* B and C operands from the instruction */
      /* these 2 fields are needed if B and/or C reference a pending expression
         in a register, rather than an local variable or a constant */
      int bindex, cindex;  /* saved handles to the pending expressions that were
                              in these registers */
      BinOpr op; 
    } binop;
    struct {
      int b;  /* B operand from the instruction */
      int bindex;
      int needinnerparen;
      UnOpr op;
    } unop;
    struct {
      OpCode op;  /* which call opcode */
      int nret;  /* number of return values to use */
      int narg;  /* number of arguments passed */
    } call;
    struct {
      /* concatenations always push operands to the stack */
      int firstindex, lastindex;  /* index of first and last expression */
    } concat;
    struct {
      int b, c;  /* B is the table, C is the key */
      int bindex, cindex;
      int isfield;  /* true if the emitter should write it as a field */
    } indexed;
    struct {
      OpCode rootop;  /* the `root' opcode (without R1, BK, S or N suffixes)
                         ROOTOP serves as a subtype for ESTORE */
      int srcreg;  /* the source register to store (because the source
                      expression may be NULL in the case of OP_LOADNIL or 
                      OP_VARARG) */
      int aux1;  /* table register for OP_SETTABLE family
                    upvalue index for OP_SETUPVAL
                    K index for OP_SETGLOBAL */
      int aux2;  /* key RK operand for OP_SETTABLE family */
    } store;
  } u;
  int previndex;  /* if a store node, the previous store node in the chain,
                     otherwise, the previous ExpNode that clobbered the same
                     register */
  int auxlistprev;  /* auxiliary backward link for expression kinds to use for
                       whatever purpose */
  int auxlistnext;  /* auxiliary forward link for expression kinds to use for
                       whatever purpose */
  /*int type_checked;*/  /* if type-checked, which type */
  int info;
  int aux;
  int line;  /* which line is this on */
  int closeparenline;
  int endlabel;  /* next conditional jump target or -1 */
  lu_byte leftside; /* is this node the left operand in a binary operation */
  lu_byte pending;  /* true if this expression has not yet been emitted */
  lu_byte goiftrue;
  lu_byte forceparen;  /* force parentheses around the expression */
} ExpNode;


typedef struct SlotDesc {
  lu_byte flags;
  union {
    struct LocVar *locvar;  /* the local variable that is in this register */
    int expindex;  /* if pending, the ExpNode that is in this register */
  } u;
} SlotDesc;

#endif /* ldecomp_c */

LUAI_FUNC Analyzer *luaA_newanalyzer (hksc_State *H);
LUAI_FUNC void luaA_freeanalyzer (hksc_State *H, Analyzer *a);

/*
** call this to allocate a bitmap big enough for NK constants; the caller can
** then reference a->kmap
*/
LUAI_FUNC void luaA_allockmap (hksc_State *H, Analyzer *a, int nk);
LUAI_FUNC void luaA_freekmap (hksc_State *H, Analyzer *a);

#endif /* HKSC_DECOMPILER */
#endif
