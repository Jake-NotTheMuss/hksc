/*
** $Id: lanalyzer.h $
** Auxiliary functions to manipulate function analyzer structures
** See Copyright Notice in lua.h
*/

#ifndef lanalyzer_h
#define lanalyzer_h

#ifdef HKSC_DECOMPILER

#include "lobject.h"

#if defined(ldecomp_c) || defined(lanalyzer_c)

/*
** basic block types
*/
#define BBLTYPE_TABLE \
  DEFBBLTYPE(FUNCTION)   /* a Lua function */      \
  DEFBBLTYPE(WHILE)      /* a while-loop */        \
  DEFBBLTYPE(REPEAT)     /* a repeat-loop */       \
  DEFBBLTYPE(FORNUM)     /* a for-numeric-loop */  \
  DEFBBLTYPE(FORLIST)    /* a for-list-loop */     \
  DEFBBLTYPE(DO)         /* a block */             \
  DEFBBLTYPE(IF)         /* an if-block */         \
  DEFBBLTYPE(ELSE)       /* an else-block */       \
  DEFBBLTYPE(ELSEIF)     /* an elseif-block */

#define DEFBBLTYPE(e)  BBL_##e,
enum BBLTYPE {
  BBLTYPE_TABLE
  MAX_BBLTYPE
};
#undef DEFBBLTYPE


/*
** instruction properties
*/
#define INSFLAG_TABLE \
  DEFINSFLAG(PRECONCAT)  /* first pc that sets up a concat operation */ \
  DEFINSFLAG(PRECALL)  /* first pc that sets up a function call */ \
  DEFINSFLAG(PRERETURN)  /* first pc that evaluates a returned expression */ \
  /* possible first pc that evaluates a single return value */ \
  DEFINSFLAG(PRERETURN1)  \
  DEFINSFLAG(PREBRANCHTEST)  /* first pc that evaluates a branch condition */ \
  /* possible first pc that evaluates a branch condition */ \
  DEFINSFLAG(PREBRANCHTEST1) \
  /* false-jump out of an if-statement condition evaluation */ \
  DEFINSFLAG(BRANCHFAIL) \
   /* true-jump out of an if-statement condition evaluation */ \
  DEFINSFLAG(BRANCHPASS) \
  DEFINSFLAG(PRELOOPTEST)  /* first pc that evaluates a loop condition */ \
  /* possible first pc that evaluates a loop condition */ \
  DEFINSFLAG(PRELOOPTEST1) \
  DEFINSFLAG(LOOPFAIL)  /* false-jump out of a loop condition evaluation */ \
  DEFINSFLAG(LOOPPASS)  /* true-jump out of a loop condition evaluation */ \
  DEFINSFLAG(OPTLOOPFAILTARGET)  /* optimized jump target of a loop fail */ \
  DEFINSFLAG(REPEATSTAT)  /* first pc in a repeat-loop */ \
  DEFINSFLAG(WHILESTAT)  /* first pc in a while-loop */ \
  DEFINSFLAG(FORLIST)  /* first pc in a list for-loop */ \
  /* first pc to evaluate for-list control variables */ \
  DEFINSFLAG(PREFORLIST) \
  DEFINSFLAG(FORNUM)  /* first pc in a numeric for-loop */ \
  DEFINSFLAG(PREFORNUM)  /* first pc to evluate for-num control variables */ \
  DEFINSFLAG(BLOCKEND)  /* last pc in a block */ \
  DEFINSFLAG(BRANCHBEGIN)  /* start of branch block */ \
  DEFINSFLAG(LOOPEND)  /* last pc in a loop */ \
  DEFINSFLAG(TESTSETEND) /* last pc in a OP_TESTSET expression */ \
  DEFINSFLAG(BREAKSTAT)  /* pc is a break instruction */ \
  DEFINSFLAG(DOSTAT)  /* pc begins a block */ \
  DEFINSFLAG(EMPTYBLOCK)  /* an empty block exists before this instruction */ \
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
  DEFREGFLAG(FREE)      /* a free register */ \
  DEFREGFLAG(RESERVED)  /* a register being used in a temporary expression */ \
  DEFREGFLAG(LOCAL)     /* a register which holds an active local variable */ \
  DEFREGFLAG(CONTROL)   /* a register which holds a loop control variable */ \
  DEFREGFLAG(UPVAL)     /* a register used as an upvalue */

#define DEFREGFLAG(e)  REG_##e,
enum REGFLAG {
  REGFLAG_TABLE
  MAX_REGFLAG
};
#undef DEFREGFLAG


typedef struct BasicBlock {
  struct BasicBlock *next;  /* next block */
  struct BasicBlock *nextsibling;  /* next sibling block */
  struct BasicBlock *firstchild;  /* first child block */
  int startpc;  /* startpc of the block */
  int endpc;  /* endpc of the block */
  int type;  /* the type of the block */
  lu_byte isempty;  /* true if the block has zero instructions */
#ifdef LUA_DEBUG
  lu_byte visited;  /* has this block been visited in pass2 */
#endif
} BasicBlock;


typedef enum {
  CALLPREP,  /* function-call preparation code */
  CONCATPREP,  /* concat preparation code */
  FORNUMPREP,  /* numeric for-loop preparation code */
  FORLISTPREP,  /* list for-loop preparation code */
  RETPREP  /* return statement preparation code */
} openexptype;

typedef struct OpenExpr {
  openexptype kind;
  int startpc;
} OpenExpr;


#endif /* ldecomp_c */

LUAI_FUNC Analyzer *luaA_newanalyzer (hksc_State *H);
LUAI_FUNC void luaA_freeanalyzer (hksc_State *H, Analyzer *a);

#endif /* HKSC_DECOMPILER */
#endif
