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
  DEFBLTYPE(ELSE)       /* an else-block */       \
  DEFBLTYPE(ELSEIF)     /* an elseif-block */

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
  DEFINSFLAG(PRERETURN1) /* the pc before a single-value-return */ \
  DEFINSFLAG(BRANCHFAIL) /* false-jump in an if-statement condition */ \
  DEFINSFLAG(BRANCHPASS) /* true-jump in an if-statement condition */ \
  DEFINSFLAG(LOOPFAIL)  /* false-jump in a loop condition */ \
  DEFINSFLAG(LOOPPASS)  /* true-jump in a loop condition */ \
  DEFINSFLAG(OPTLOOPFAILTARGET)  /* optimized jump target of a loop fail */ \
  DEFINSFLAG(REPEATSTAT)  /* first pc in a repeat-loop */ \
  DEFINSFLAG(WHILESTAT)  /* first pc in a while-loop */ \
  DEFINSFLAG(FORLIST)  /* first pc in a list for-loop */ \
  DEFINSFLAG(FORNUM)  /* first pc in a numeric for-loop */ \
  DEFINSFLAG(BLOCKEND)  /* last pc in a non-loop block */ \
  DEFINSFLAG(BRANCHBEGIN)  /* start of branch block */ \
  DEFINSFLAG(LOOPEND)  /* last pc in a loop */ \
  DEFINSFLAG(TESTSETEND) /* last pc in a OP_TESTSET expression */ \
  DEFINSFLAG(BREAKSTAT)  /* pc is a break instruction */ \
  DEFINSFLAG(DOSTAT)  /* pc begins a block */ \
  DEFINSFLAG(EMPTYBLOCK)  /* an empty block exists before this instruction */ \
  DEFINSFLAG(BOOLLABEL)  /* an OP_LOADBOOL label */ \
  DEFINSFLAG(LOCVAREND)  /* endpc of a local variable */ \
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
  DEFREGFLAG(HASNOTE)  /* used by first pass to avoid creating redunadnt \
                          RegNote entries */ \
  DEFREGFLAG(PENDING)  /* a register being used in a temporary expression */ \
  DEFREGFLAG(LOCAL)     /* a register which holds an active local variable */ \
  DEFREGFLAG(CONTROL)   /* a register which holds a loop control variable */ \
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
  int type;  /* the type of the block */
  lu_byte isempty;  /* true if the block has zero instructions */
#ifdef LUA_DEBUG
  lu_byte visited;  /* has this block been visited in pass2 */
#endif
} BlockNode;


typedef enum {
  VOIDPREP,  /* use this kind when you need to traverse an instruction sequence
                but not record it as an open expression */
  CALLPREP,  /* function-call preparation code */
  CONCATPREP,  /* concat preparation code */
  FORNUMPREP,  /* numeric for-loop preparation code */
  FORLISTPREP,  /* list for-loop preparation code */
  SETLISTPREP,  /* code evaluating items in a table constructor */
  EMPTYTABLE,  /* empty table constructors are their own category because they
                  may or may not clobber an open register */
  RETPREP  /* return statement preparation code */
} openexptype;

typedef struct OpenExpr {
  openexptype kind;
  int startpc;
  int endpc;
  int firstreg;
} OpenExpr;


/* when debug info is not used, the decompiler makes note of how registers are
   used in the program */
enum NOTEWORTHY_TYPE {
  REG_NOTE_UPVALUE, /* is used as an upvalue for a child function */
  REG_NOTE_NONRELOC, /* is the source in an OP_MOVE inside an open expression */
  MAX_REG_NOTE
};


typedef struct RegNote {
  int note;  /* what is noteworhty about this register and pc */
  int pc;
  int reg;
} RegNote;


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
  ECON,  /* a table constructor */
  ECLOSURE,  /* a Lua function */
  ELOCAL,  /* a local variable */
  EUPVAL,  /* an upvalue */
  EGLOBAL,  /* a global variable */
  EINDEXED,  /* a table index */
  ESELF,  /* a table index called as a method */
  EBINOP,  /* a binary operation */
  EUNOP,  /* a unary operation */
  ECALL,  /* a function call */
  ETAILCALL,  /* a tail function call */
  ECONCAT,  /* a concatenation */
  ESTORE  /* encodes an L-value in an assignment list */
} expnodekind;


typedef struct ExpNode {
  expnodekind kind;
  union {
    TValue *k;  /* constant value */
    TString *name;  /* variable name */
    int token;  /* token ID, e.g. TK_TRUE for `true' */
    struct { int arrsize, hashsize; int est; } con;
    const Proto *p;  /* Lua closure */
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
  int prevregindex;  /* stack index of ExpNode in the previous register */
  int nextregindex;  /* stack index of ExpNode in the next register */
  /*int type_checked;*/  /* if type-checked, which type */
  int info;
  int aux;
  int line;  /* which line is this on */
  int closeparenline;
  lu_byte dependondest; /* does this node use its destination as a source */
  lu_byte leftside; /* is this node the left operand in a binary operation */
  lu_byte pending;  /* true if this expression has not yet been emitted */
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

#endif /* HKSC_DECOMPILER */
#endif
