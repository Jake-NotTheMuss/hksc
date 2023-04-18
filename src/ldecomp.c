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
#include "ldebug.h"
#include "ldo.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "lundump.h"

#ifdef HKSC_DECOMPILER

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

#if 0
#define DEFREGFLAG(e)  "REG_" #e,
static const char *const regflagnames [] = {
  REGFLAG_TABLE
  "MAX_REGFLAG"
};
#undef DEFREGFLAG
#endif

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

typedef int (*CheckVarStartsCB)(struct DFuncState *fs, int pc);

typedef struct {
  hksc_State *H;
  struct DFuncState *fs;  /* current function state */
  CheckVarStartsCB varstarts;
  lua_Writer writer;
  void *data;
  const char *name;  /* input name */
  int status;
  int usedebuginfo;  /* true if using debug info */
  int matchlineinfo;  /* true if matching statements to line info */
  /* data for the current function's decompilation */
  int funcidx;  /* n for the nth function that is being decompiled */
  int indentlevel;
} DecompState;


/* a decompiled function */
typedef struct DFuncState {
  struct DFuncState *prev;  /* enclosing function */
  DecompState *D;  /* decompiler state */
  Analyzer *a;  /* function analyzer data */
  const Proto *f;  /* current function header */
  hksc_State *H;  /* copy of the Lua state */
  int idx;  /* the nth function */
  struct LocVar *locvars;  /* information about local variables */
  int locvaridx;
  int nlocvars;  /* number of local variables declared so far */
  int sizelocvars;
  int pc;  /* current pc */
  int firstclob;  /* first pc that clobbers register A */
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
      case 'I': /* instruction flag */
        fputs(insflagname(va_arg(argp, int)), stdout);
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

#else /* !LUA_DEBUG */
#define D(x) ((void)0)
#endif /* LUA_DEBUG */


#define CHECK(fs,c,msg) if (!(c)) badcode(fs,msg)

static void badcode(DFuncState *fs, const char *msg)
{
  const char *name = fs->D->name;
  luaD_setferror(fs->H, "%s: bad code in precompiled chunk: %s", name, msg);
  luaD_throw(fs->H, LUA_ERRSYNTAX);
}


/*
** test_ins_property - check if flag VAL is set on the instruction at PC
*/
#define test_ins_property(fs,pc,val) test_ins_property_(fs,pc,1<<(val))

static int test_ins_property_(DFuncState *fs, int pc, int mask)
{
  lua_assert(pc >= 0 && pc < fs->f->sizecode);
  return ((fs->a->insproperties[pc] & mask) != 0);
}

#ifdef LUA_DEBUG
#define check_ins_property(fs,pc,val) \
  lua_assert(test_ins_property(fs, pc, val))
#else /* !LUA_DEBUG */
#define check_ins_property(fs,pc,val) ((void)0)
#endif /* LUA_DEBUG */


/*
** set_ins_property - set flag VAL for the instruction at PC
*/
static void set_ins_property_(DFuncState *fs, int pc, int mask)
{
  fs->a->insproperties[pc] |= mask;
}

#ifdef LUA_DEBUG

#define set_ins_property(fs,pc,val) \
  set_ins_property_debug((fs),(pc),1<<(val),(val))

static void set_ins_property_debug(DFuncState *fs, int pc, int mask, int idx)
{
  int i;
  lprintf("  marking pc (%i) as %I\n", pc, idx);
  lua_assert(pc >= 0 && pc < fs->f->sizecode);
  lprintf("  previous flags for pc (%i):", pc);
  for (i = 0; i < MAX_INSFLAG; i++) {
    if (fs->a->insproperties[pc] & (1 << i))
      lprintf("  %I", i);
  }
  printf("\n");
  set_ins_property_(fs, pc, mask);
}
#else /* !LUA_DEBUG */
#define set_ins_property(fs,pc,val) set_ins_property_(fs,pc,1<<(val))
#endif /* LUA_DEBUG */


/*
** unset_ins_property - clear flag VAL for the instruction at PC
*/
static void unset_ins_property_(DFuncState *fs, int pc, int mask)
{
  fs->a->insproperties[pc] &= ~mask;
}

#ifdef LUA_DEBUG

#define unset_ins_property(fs,pc,val) \
  unset_ins_property_debug((fs),(pc),1<<(val),(val))

static void unset_ins_property_debug(DFuncState *fs, int pc, int mask, int idx)
{
  lprintf("  clearing flag %I for pc (%i)\n", idx, pc);
  lua_assert(pc >= 0 && pc < fs->f->sizecode);
  unset_ins_property_(fs, pc, mask);
}
#else /* !LUA_DEBUG */
#define unset_ins_property(fs,pc,val) unset_ins_property_(fs,pc,1<<(val))
#endif /* LUA_DEBUG */

/*
** init_ins_property - set flag VAL on the instruction at PC, assert the flag
** was not previously set
*/
#ifdef LUA_DEBUG
#define init_ins_property(fs,pc,val) \
  (lua_assert(!test_ins_property(fs,pc,val)), set_ins_property(fs,pc,val))
#else /* !LUA_DEBUG */
#define init_ins_property(fs,pc,val) set_ins_property(fs,pc,val)
#endif /* LUA_DEBUG */


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
  if (D->usedebuginfo) { /* have debug info */
    D(printf("using debug info for function '%s'\n", f->name ? getstr(f->name) :
             "(anonymous)"));
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
  fs->firstclob = -1;
  /* allocate vectors for instruction and register properties */
  a->sizeinsproperties = f->sizecode; /* flags for each instruction */
  a->insproperties = luaM_newvector(H, a->sizeinsproperties, InstructionFlags);
  memset(a->insproperties, 0, f->sizecode * sizeof(InstructionFlags));
  a->sizeregproperties = f->maxstacksize; /* flags for each register */
  a->regproperties = luaM_newvector(H, f->maxstacksize, RegisterFlags);
  memset(a->regproperties, 0, a->sizeregproperties * sizeof(RegisterFlags));
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


static void DumpStringf(DecompState *D, const char *fmt, ...)
{
  va_list argp;
  const char *s;
  va_start(argp, fmt);
  s = luaO_pushvfstring(D->H, fmt, argp);
  DumpString(s,D);
  va_end(argp);
}


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
    printf("  ");
  if (indent) printf("- ");
  lprintf("(%i-%i) %b ", bbl->startpc, bbl->endpc, bbl);
  if (nextsibling != NULL)
    lprintf("(sibling (%i-%i) %b)\n", nextsibling->startpc, nextsibling->endpc,
            nextsibling);
  else
    printf("(NO sibling)\n");
  while (child != NULL) {
    debugbbl(fs, child, indent+1);
    child = child->nextsibling;
  }
}


static void debugbblsummary(DFuncState *fs)
{
  BasicBlock *first = fs->a->bbllist.first;
  printf("BASIC BLOCK SUMMARY\n"
         "-------------------\n");
  lua_assert(first != NULL);
  lua_assert(first->type == BBL_FUNCTION);
  lua_assert(first->nextsibling == NULL);
  debugbbl(fs, first, 0);
  printf("-------------------\n");
}


static void checktreevisited(BasicBlock *bbl)
{
  BasicBlock *child, *nextsibling;
  lua_assert(bbl != NULL);
  child = bbl->firstchild;
  nextsibling = bbl->nextsibling;
  lua_assert(bbl->visited);
  while (child != NULL) {
    checktreevisited(child);
    child = child->nextsibling;
  }
}


#else /* !LUA_DEBUG */

#define debugbblsummary(fs)  ((void)(fs))
#define checktreevisited(bbl)  ((void)(bbl))

#endif /* LUA_DEBUG */


static void dumploopinfo(const Proto *f, DFuncState *fs, DecompState *D)
{
  int pc;
  const Instruction *code = f->code;
  int sizecode = f->sizecode;
  for (pc = 0; pc < sizecode; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    if (test_ins_property(fs, pc, INS_REPEATSTAT))
      DumpLiteral("BEGIN REPEAT\n", D);
    if (test_ins_property(fs, pc, INS_WHILESTAT))
      DumpLiteral("BEGIN WHILE\n", D);
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
    if (test_ins_property(fs, pc, INS_EMPTYBLOCK))
      DumpLiteral("EMPTY BLOCK\n", D);
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
              int prevA = GETARG_A(prev);
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

#ifdef LUA_DEBUG

#define printins1(pc,i,func) lprintf("pc %i: %O (" func ")\n",pc,i)

#define encounteredstat1(what) \
  lprintf("  encountered " what " ending at (%i)\n", endpc)

#define leavingstat1(what) \
  lprintf("  leaving " what " ending at (%i), (%i-%i)\n", endpc,ca->pc,endpc)

#else /* !LUA_DEBUG */
#define printins1(pc,i,func) ((void)0)
#define encounteredstat1(what) ((void)0)
#define leavingstat1(what) ((void)0)
#endif /* LUA_DEBUG */

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
    printins1(pc,i,"concat1");
    switch (o) {
      CASE_OP_CALL:
        callstat1(ca, fs);
        pc = ca->pc;
        o = GET_OPCODE(code[pc]);
        if (firstreg == a && !isOpCall(o))
          goto markconcat;
        break;
      case OP_CONCAT:
        /* todo: what is the actual restruction on the nested firstreg? */
        if (GETARG_B(code[pc]) <= firstreg) lua_assert(0);
        concat1(ca, fs);
        break;
      case OP_RETURN:
        lua_assert(0);
      default:
        if (beginstempexpr(code, i, pc, firstreg, endpc)) {
          markconcat:
          set_ins_property(fs, pc, INS_PRECONCAT);
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
    printins1(pc,i,"callstat1");
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
          set_ins_property(fs, pc, INS_PRECALL);
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
    printins1(pc,i,"retstat1");
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
    printins1(pc,i,"fornumprep1");
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
    /* todo: is this an assert ot code-check */
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
    printins1(pc,i,"forlistprep1");
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

#ifdef LUA_DEBUG
#define encountered1(what,pc) printf("  encountered a " what " loop starting" \
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
  D(printf("--\n"));
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
  D(printf("--\n"));
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
    D(printf("found no children for this block\n"));
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
    D(printf("entering new basic block of type (%s)\n", bbltypename(type)));
  }
  else {
    struct stat1 *outersaved;
    if (branch != NULL) { /* inside a branch construct */
      lua_assert(block == NULL);
      lua_assert(branch->elseblock != NULL);
      nextsibling = branch->elseblock; /* the else-block */
      outersaved = branch->outer;
      D(printf("entering new branch\n"));
    }
    else { /* inside a block */
      lua_assert(block != NULL);
      nextsibling = NULL;
      outersaved = block->outer;
      D(printf("entering new do-block\n"));
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
    int sbx = GETARG_sBx(i);
    printins1(pc,i,"bbl1");
    switch (o) {
      case OP_JMP: {
        /* todo: check that target is a valid pc */
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
        CHECK(fs, target >= 0 && target < fs->f->sizecode, "jump target pc is "
              "invalid");
        if (test_ins_property(fs, pc+1, INS_FORLIST)) {
          forlistprep1(ca, fs, target);
          break;
        }
        if (pc == 0) {
          ca->prevTMode = 0; /* unconditional jump */
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
              /* todo: should this be a code check instead? */
              lua_assert(nextsibling == NULL ||
                         nextsibling == fs->a->bbllist.first);
              D(lprintf("nextbranch = %B\n", nextbranch));
              D(lprintf("cleaning up testset expression from (%i-%i)\n",
                     pc, target-1));
              D(printf("--\n"));
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
              D(printf("--\n"));
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
                  /* todo: should this be an assert or a code check? */
                  lua_assert(test_ins_property(fs, target, INS_REPEATSTAT));
                  goto markrepeatstat;
                }
                /* todo: what should be done in this case? */
              }
              else { /* a repeat-loop fail-jump */
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
            lua_assert(type != BBL_FUNCTION);
            /* the current loop must have a sibling for it to be an optimized
               exit */
            if (futuresibling != NULL) {
              int siblingstartpc = futuresibling->startpc -
                                   (futuresibling->type == BBL_ELSE);
              siblingjump = code[siblingstartpc];
              if (GET_OPCODE(siblingjump) != OP_JMP) {
                /* todo: bad code? */
                lua_assert(0);
              }
              siblingtarget = siblingstartpc+1+GETARG_sBx(siblingjump);
              if (target == siblingtarget) { /* optimized jump? */
                if (ca->prevTMode || pc == startpc) /* loop-exit */
                  init_ins_property(fs, pc, INS_LOOPFAIL);
                else /* break */
                  init_ins_property(fs, pc, INS_BREAKSTAT);
                break;
              }
              else { /* bad code? */
                lua_assert(0);
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
              /* todo: should this be a code check */
              lua_assert(branch == NULL && block == NULL);
              if (target-1 == outer.end && outer.end-1 == endpc &&
                  futuresibling == NULL) {
                BasicBlock *lastsibling = nextsibling;
                BasicBlock *newblock;
                D(lprintf("fixing erroneous while-true-loop detection\n"));
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
                }
                D(lprintf("created new block %B\n", newblock));
                D(lprintf("first block in chain is %B\n",
                          fs->a->bbllist.first));
                ca->testset.endpc = ca->testset.reg = -1;
                ca->curr = outer;
                return;
              }
              else { /* todo: when is this the case? */
                lua_assert(0);
              }
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
                    D(printf("this branch is a child block of the top-level "
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
#if 0
              if (pc > 3 && GET_OPCODE(code[pc-2]) == OP_JMP) {
                if (testTMode(GET_OPCODE(code[pc-3])))
                  goto markrepeatfail; /* fail-jump out of repeat-loop */
                else {
                  /* in a repeat-loop, OP_JMP at PC1 should jump to (PC+1):
                       JMP     2 <-- PC1  (jump to PC+1)
                       CLOSE   n <-- PC1+1 (PREVOP)
                       JMP     2 <-- PC
                       CLOSE   n <-- PC+1
                       JMP     (to start of repeat-loop)
                   */
                  if ((pc-2 + 1 + GETARG_sBx(code[pc1])) == (pc+1)) {
                  /*if (pc1 >= 0 && testTMode(GET_OPCODE(code[pc1])))*/
                    /* mark this now so it doesn't get detected as something
                       else */
                    init_ins_property(fs, pc-2, INS_LOOPPASS);
                    goto markrepeatfail; /* fail-jump out of repeat-loop */
                  }
                }
              }
#endif
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
                D(printf("encountered an else-block which closes variables, "
                       "correcting erroneous do-block detection\n"));
                /* the OP_CLOSE at the end of this branch is not for a block */
                noblock = 1;
                /* need to close all the child blocks that are contained inside
                   the else-part as well */
                D(lprintf("childblock initialized to %B\n", childblock));
                for (; bl != NULL; bl = bl->prev) {
                  D(printf("i = (%d), bl = (%p)\n", i, cast(void *, bl)));
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
                  /* correct the else-block that has already been created */
                  lua_assert(elseblock->startpc == pc+1);
                  D(lprintf("branchendpc = (%i)\n"
                            "branch->midpc = (%i)\n",
                            branchendpc, branch ? branch->midpc: -1));
                  if (branch != NULL && branchendpc+1 == branch->midpc &&
                      target == new_branch.optimalexit &&
                      new_branch.optimalexit > elseblock->endpc) {
                    D(lprintf("using new_branch.optimalexit-1 (%i) as the endpc"
                              " instead of (%i)\n", new_branch.optimalexit-1,
                              elseblock->endpc));
                    elseblock->endpc = new_branch.optimalexit-1;
                  }
                  else if (branch != NULL && target == branch->midpc) {
                    elseblock->type = BBL_IF;
                    branch->ifblock = elseblock;
                    branch->firstblock = new_branch.firstblock;
                    branch->startpc = elseblock->startpc;
                    branch->target1 = target;
                    return;
                  }
                  elseblock->type = BBL_IF;
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
                    /*lua_assert(elseprevsibling->endpc+1 < elseblock->startpc);*/
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
            /* this block is adjacent to the previous, switch out the context
               instead of pushing a new one */
            BasicBlock *new_block;
            D(printf("encountered adjacent do-block\n"));
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
              nextsibling = nextbranch;
              /* check if the parent was also inside the else-branch */
              if (new_block.pass) { /* also pass this data to the parent */
                lua_assert(block != NULL);
                block->nextbranch = nextbranch;
                block->nextbranchtarget = nextbranchtarget;
                return; /* return for each block that was in the else-branch */
              }
            }
            else if (new_block.ifblock != NULL) {
              BasicBlock *ifblock = new_block.ifblock;
              ifblock->nextsibling = nextsibling;
              nextsibling = ifblock;
              if (block != NULL && block->state.prevsibling == NULL)
                block->state.prevsibling = nextsibling;
              if (branch != NULL && branch->elseprevsibling == NULL)
                branch->elseprevsibling = nextsibling;
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
        if (beginseval(o, a, b, c, 0)) {
          int stateunsure;
          struct block1 *bl;
          struct blockstate1 *blstate;
          fs->firstclob = pc; /* update first unstruction that clobbers A */
          if (branch != NULL && branch->parentblock != NULL) {
            bl = branch->parentblock;
            blstate = &bl->possiblestate;
            stateunsure = 1;
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
                          "removed\n", stateunsure ? "(possibly)" : "", bl->reg,
                          pc, i, i == 1 ? "" : "s"));

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
  /* make sure branch-blocks are corrected if their relationship doesn't make
     sense; NEXTBRANCH should have an exit-jump that jumps to  */
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
  if (branch == NULL && block == NULL) {
    ca->curr = outer; /* pop old values */
  }
  else {
    if (branch != NULL) {
      D(printf("forced to return from branch without finding an if-part\n"));
      branch->firstblock = nextsibling;
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
static void pass1(const Proto *f, DFuncState *fs, DecompState *D)
{
  CodeAnalyzer ca;
  ca.curr.start = ca.curr.end = ca.curr.type = -1;
  ca.pc = f->sizecode - 1; /* will be decremented again to skip final return */
  ca.prevTMode = 0;
  ca.testset.endpc = ca.testset.reg = -1;
  ca.code = f->code;
  UNUSED(D);
  bbl1(&ca, fs, 0, BBL_FUNCTION, NULL, NULL, NULL);
  /*init_ins_property(fs, f->sizecode - 1, INS_BLOCKEND);*/ /* mark end of function */
  {
    BasicBlock *first = fs->a->bbllist.first;
    lua_assert(first != NULL);
    lua_assert(first->nextsibling == NULL);
    lua_assert(first->type == BBL_FUNCTION);
  }
  D(printf("ca.pc == (%d)\n", ca.pc));
  lua_assert(ca.pc <= 0);
  lua_assert(ca.testset.endpc == -1 && ca.testset.reg == -1);
}


#define varstartshere2(fs,pc) (*((fs)->D->varstarts))((fs),(pc))

static int varstarts_nodebug(DFuncState *fs, int pc) {
  UNUSED(fs); UNUSED(pc);
  return 0; /* no debug info, let the analyzer guess */
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
  const Instruction *code;
  int firstfree;
  int pc;
  int sizecode;
  int maxstacksize;
  lu_byte intailemptyblock;
} StackAnalyzer;


#ifdef HKSC_DECOMP_DEBUG_PASS1
static void enterblock2(DFuncState *fs, BasicBlock *bbl)
{
  if (bbl->type == BBL_FUNCTION && fs->prev == NULL) { /* top-level function */
    lua_assert(fs->D->indentlevel == -1);
    return;
  }
  lua_assert(fs->D->indentlevel >= 0);
  DumpIndentation(fs->D);
  DumpString(bbltypename(bbl->type),fs->D);
  DumpLiteral("\n",fs->D);
}


static void leaveblock2(DFuncState *fs, BasicBlock *bbl)
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
}
#else /* !HKSC_DECOMP_DEBUG_PASS1 */

#define enterblock2(fs,bbl) (void)0
#define leaveblock2(fs,bbl) (void)0

#endif /* HKSC_DECOMP_DEBUG_PASS1 */

#ifdef LUA_DEBUG
static void assertbblvalid(DFuncState *fs, BasicBlock *bbl)
{
  int startpc = bbl->startpc;
  int endpc = bbl->endpc;
  int type = bbl->type;
  BasicBlock *nextsibling = bbl->nextsibling;
  lua_assert(startpc <= endpc || bbl->isempty);
  lua_assert(type >= 0 && type < MAX_BBLTYPE);
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

static void printins2(DFuncState *fs, BasicBlock *bbl, int pc, OpCode op)
{
  const char *opname;
  int type = bbl->type;
  char spaces[] = "                  ";
  int numspaces;
  opname = getOpName(op);
  numspaces = cast_int(sizeof(spaces)-1-strlen(opname));
  if (numspaces <= 0) numspaces = 1;
  lua_assert(numspaces < cast_int(sizeof(spaces)));
  spaces[numspaces] = '\0';
  printf("pc = (%d), OP_%s%s%s\n", pc+1, opname, spaces, bbltypename(type));
  UNUSED(fs);
}
#else /* !LUA_DEBUG */
#define assertbblvalid(fs,bbl) ((void)(fs),(void)(bbl))
#define printins2(fs,bbl,pc,op) ((void)(fs),(void)(bbl),(void)(pc),(void)(op))
#endif /* LUA_DEBUG */


static void DecompileFunction(DecompState *D, const Proto *f);


/*
** Second pass `basic block' handler - the function's registers and stack are
** analyzed and local variables are detected. With information about the local
** variables, any undetected do-end blocks can be detected in this pass.
*/
static void bbl2(StackAnalyzer *sa, DFuncState *fs, BasicBlock *bbl)
{
  const Instruction *code = sa->code;
  BasicBlock *nextchild = bbl->firstchild;
  int nextchildstartpc = nextchild ? nextchild->startpc : -1;
  int startpc = bbl->startpc;
  int endpc = bbl->endpc;
  int type = bbl->type;
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
  assertbblvalid(fs,bbl);
  D(lprintf("pass2: entered %sblock %b (%i-%i) at pc (%i)\n", 
         bbl->isempty ? (sa->intailemptyblock ? "tail-empty " : "empty") : "",
         bbl, startpc, endpc, sa->pc));
  enterblock2(fs, bbl);
  lua_assert(bbl->visited == 0);
  D(bbl->visited = 1);
  if (bbl->isempty) goto block2finished;
  fs->D->indentlevel++;
  for (; sa->pc < sa->sizecode; sa->pc++) {
    int pc, a, b, c, bx;
    Instruction i;
    OpCode o;
    if (sa->pc == nextchildstartpc) {
      int ischildempty;
      processnextchild:
      lua_assert(nextchild != NULL);
      ischildempty = nextchild->isempty;
      bbl2(sa, fs, nextchild);
      nextchild = nextchild->nextsibling;
      nextchildstartpc = nextchild ? nextchild->startpc : -1;
      if (sa->intailemptyblock) {
        sa->intailemptyblock = 0;
        goto loopfinished;
      }
      else if (!ischildempty) {
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
    printins2(fs, bbl, pc, o);
#ifdef LUA_DEBUG
    /* make sure this instruction hasn't already been visited */
    lua_assert(!test_ins_property(fs, pc, INS_VISITED));
    /* set this directly to avoid printing debug message every time */
    fs->a->insproperties[pc] |= (1 << INS_VISITED);
#ifdef HKSC_DECOMP_DEBUG_PASS1
    if (o == OP_CLOSURE) { /* nested closure? */
      const Proto *f = fs->f->p[bx];
      DecompileFunction(fs->D,f);
    }
#endif /* HKSC_DECOMP_DEBUG_PASS1 */
#endif /* LUA_DEBUG */
    if (pc == endpc)
      break;
    (void)a; (void)b; (void)c; (void)bx;
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
  fs->D->indentlevel--;
  block2finished:
  leaveblock2(fs, bbl);
  D(lprintf("pass2: leaving %sblock %b (%i-%i) at pc (%i)\n", bbl->isempty ?
            (sa->intailemptyblock ? "tail-empty " : "empty") : "", bbl,
            startpc, endpc, sa->pc));
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
  sa.intailemptyblock = 0;
  lua_assert(func != NULL);
  UNUSED(D);
  bbl2(&sa, fs, func);
#ifdef LUA_DEBUG
  { /* debug: make sure all instructions were visited */
    int pc;
    BasicBlock *functionblock = fs->a->bbllist.first;
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
  if (f->name && getstr(f->name))
    D(printf("-- Decompiling function (%d) named '%s'\n", D->funcidx,
             getstr(f->name)));
  else
    D(printf("-- Decompiling anonymous function (%d)\n", D->funcidx));
  pass1(f,&new_fs,D);
  (void)dumploopinfo;
  D(printf("new_fs.firstclob = (%d)", new_fs.firstclob+1));
  if (new_fs.firstclob != -1)
    D(printf(", a = (%d)",GETARG_A(f->code[new_fs.firstclob])));
  D(printf("\n"));
  debugbblsummary(&new_fs);
  /*dumploopinfo(f, &new_fs, D);*/ (void)dumploopinfo;
  /*DumpLiteral("\n\n", D);*/
  pass2(f,&new_fs,D);
  /*DecompileCode(f,&new_fs,D);*/
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
  return D.status;
  (void)DumpIndentation; /* todo: remove when this is used by pass2 */
}

#endif /* HKSC_DECOMPILER */
