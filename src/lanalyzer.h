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
** lexical block types
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
  DEFINSFLAG(SEQPT)  /* a statement boundary */ \
  DEFINSFLAG(ENDSCOPE)  /* end of a lexical scope */ \
  DEFINSFLAG(KLOCVAR)  /* local initialization that pushes a constant */ \
  DEFINSFLAG(MULTILOAD)  /* loads values into multiple registers */ \
  DEFINSFLAG(FIXEDSTARTLINE)  \
                             /* pc corresponds to an earlier source line than \
                             it is mapped to (referred to as `fixed' line) */ \
  DEFINSFLAG(CONSTRUCTOR)  /* inside a constructor */ \
  DEFINSFLAG(CONSTRUCTOR_SETTABLE)  /* a settable op in a constructor */ \
  DEFINSFLAG(FAILJUMP)  /* a jump-on-false */ \
  DEFINSFLAG(PASSJUMP)  /* a jump past a jump-on-false */ \
  DEFINSFLAG(BRANCHFAIL) /* false-jump in an if-statement condition */ \
  DEFINSFLAG(BRANCHPASS) /* true-jump in an if-statement condition */ \
  DEFINSFLAG(LOOPFAIL)  /* false-jump in a loop condition */ \
  DEFINSFLAG(LOOPPASS)  /* true-jump in a loop condition */ \
  DEFINSFLAG(BLOCKEND)  /* last pc in a non-loop block */ \
  DEFINSFLAG(AUGBREAK)  /* augmented break in a repeat-loop with upvalues */ \
  DEFINSFLAG(BREAKSTAT)  /* pc is a break instruction */ \
  DEFINSFLAG(BOOLLABEL)  /* an OP_LOADBOOL label */ \
  DEFINSFLAG(SKIPBOOLLABEL)  /* a jump over 2 bool labels */ \
  DEFINSFLAG(NILLABEL)  /* an OP_LOADNIL label */ \
  DEFINSFLAG(BLOCKFOLLOW)  /* is a valid pc for `return' or `break' */ \
  DEFINSFLAG(LOCVAREXPR)  /* start of a local varible initialization */ \
  DEFINSFLAG(DISCHARGEDLOCVAREXPR)  /* same as above for a saved local */ \
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

/* ensure there are enough bits for all instruction properties */
lua_static_assert(MAX_INSFLAG <= sizeof(InstructionFlags) * CHAR_BIT);


/*
** register properties
*/
#define REGFLAG_TABLE \
  DEFREGFLAG(PENDING)  /* a register being used in a temporary expression */ \
  DEFREGFLAG(CONTROL)   /* a register which holds a loop control variable */ \
  DEFREGFLAG(LOCAL)    /* a register which holds an active local variable */ \
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
  unsigned fixedstartline : 1;  /* true if start-line is fixed; in this case,
                                   the actual start line is mapped to a
                                   specific opcode at the end of the block */
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
  unsigned loop : 1; /* true if the OP_CLOSE code was at the end of the loop */
} BlockState2;



typedef enum {
  CALLPREP,  /* function-call preparation code */
  CONCATPREP,  /* concat preparation code */
  FORNUMPREP,  /* numeric for-loop preparation code */
  FORLISTPREP,  /* list for-loop preparation code */
  SETLISTPREP,  /* code evaluating a table constructor with array items */
  HASHTABLEPREP, /* code evaluating a table constructor with only hash items */
  EMPTYTABLE,  /* an empty table constructor */
  RETPREP,  /* return statement preparation code */
  VOIDPREP  /* use this kind when you need to traverse an instruction sequence
               but not record it as an open expression */
} openexptype;

typedef struct OpenExpr {
  int startpc, endpc;
  /* sharednil is a single bit, but I use an int to encode other information
     when `sharednil' is not applicable */
  int sharednil;
#ifdef LUA_DEBUG
  /* use the enum type when debugging so it's easy to tell what kind it is */
  openexptype kind;
#else
  /* save space otherwise (3 bits needed for the expression kind) */
  lu_byte kind;
#endif
  lu_byte firstreg;  /* 8-bit register index */
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
  ECALL,  /* a function call, aux = line to emit close paren of call or 0 */
  ECONCAT,  /* a concatenation */
  ECONDITIONAL,  /* a conditional expression */
  ESTORE  /* encodes an L-value in an assignment list */
} expnodekind;


/*
** note: An int or larger is needed for ExpNode indices there can be as many
** ExpNodes at a time as there can be instructions in a program
*/
typedef struct ExpNode {
  expnodekind kind;
  union {
    TValue *k;  /* constant value */
    TString *name;  /* variable name */
    int token;  /* token ID, e.g. TK_TRUE for `true' */
    struct {
#ifdef LUA_DEBUG
      int arrsize, hashsize;
#endif
      int nextpc;  /* the pc after the end of the constructor expression */
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
      int bindex, cindex;  /* saved handles to the pending expressions that
                              were in these registers */
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
#if HKSC_STRUCTURE_EXTENSION_ON
  lu_byte type;  /* type of value in this slot */
  const struct StructProto *proto;
#endif /* HKSC_STRUCTURE_EXTENSION_ON */
  union {
    /* fields used by source code generator */
    struct LocVar *locvar;  /* the local variable that is in this register */
    int expindex;  /* if pending, the ExpNode that is in this register */
    /* fields used by ExpressionParser */
    struct {
      int firstactive; /* the pc after the evaluation of a value in this slot*/
      int aux;  /* extra value based on flags */
    } s;
  } u;
} SlotDesc;

#endif /* ldecomp_c */

LUAI_FUNC Analyzer *luaA_newanalyzer (hksc_State *H);
LUAI_FUNC void luaA_freeanalyzer (hksc_State *H, Analyzer *a);

#endif /* HKSC_DECOMPILER */

#ifndef HKSC_VERSION
#define CASE_OP_CALL_LABEL OP_CALL: case CASE_OP_TAILCALL_LABEL
#define CASE_OP_TAILCALL_LABEL OP_TAILCALL
#define CASE_OP_GETTABLE_LABEL OP_GETTABLE
#define CASE_OP_SETTABLE_LABEL OP_SETTABLE
#define IS_OP_CALL(o)  ((o) == OP_CALL || IS_OP_TAILCALL(o))
#define IS_OP_TAILCALL(o)  ((o) == OP_TAILCALL)
#define IS_OP_GETTABLE(o)  ((o) == OP_GETTABLE)
#define IS_OP_SETTABLE(o)  ((o) == OP_SETTABLE)
#define IS_OP_SETSLOT(o)  0
#endif /* HKSC_VERSION */

#endif
