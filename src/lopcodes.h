/*
** $Id: lopcodes.h $
** Opcodes for Lua virtual machine
** See Copyright Notice in lua.h
*/

#ifndef lopcodes_h
#define lopcodes_h

#include "llimits.h"


/*===========================================================================
  We assume that instructions are unsigned numbers.
  All instructions have an opcode in the first 6 bits.
  Instructions can have the following fields:
  `A' : 8 bits
  `B' : 9 bits
  `C' : 9 bits
  `Bx' : 18 bits (`B' and `C' together)
  `sBx' : signed Bx

  A signed argument is represented in excess K; that is, the number
  value is the unsigned value minus K. K is exactly the maximum value
  for that argument (so that -max is represented by 0, and +max is
  represented by 2*max), which is half the maximum for the corresponding
  unsigned argument.
===========================================================================*/


enum OpMode {iABC, iABx, iAsBx};  /* basic instruction format */


/*
** size and position of opcode arguments.
*/
#define SIZE_C    9
#define SIZE_B    8
#define SIZE_Bx    (SIZE_C + SIZE_B)
#define SIZE_A    8

#define SIZE_OP   7

#define POS_A     0
#define POS_C    (POS_A + SIZE_A)
#define POS_B    (POS_C + SIZE_C)
#define POS_Bx    POS_C
#define POS_OP   (POS_B + SIZE_B)

/*
** limits for opcode arguments.
** we use (signed) int to manipulate most arguments,
** so they must fit in LUAI_BITSINT-1 bits (-1 for sign)
*/
#if SIZE_Bx < LUAI_BITSINT-1
#define MAXARG_Bx        ((1<<SIZE_Bx)-1)
#define MAXARG_sBx        (MAXARG_Bx>>1)         /* `sBx' is signed */
#else
#define MAXARG_Bx        MAX_INT
#define MAXARG_sBx        MAX_INT
#endif


#define MAXARG_A        ((1<<SIZE_A)-1)
#define MAXARG_B        ((1<<SIZE_B)-1)
#define MAXARG_C        ((1<<SIZE_C)-1)


/* creates a mask with `n' 1 bits at position `p' */
#define MASK1(n,p)  ((~((~(Instruction)0)<<(n)))<<(p))

/* creates a mask with `n' 0 bits at position `p' */
#define MASK0(n,p)  (~MASK1(n,p))


#define ISBK(o) \
  (getOpMode(o) == iABC && (getBMode(o) == OpArgUK || getBMode(o) == OpArgRK))

/*
** the following macros help to manipulate instructions
*/

#define GET_OPCODE(i)  (cast(OpCode, ((i)>>POS_OP) & MASK1(SIZE_OP,0)))
#define SET_OPCODE(i,o) do { \
  OpCode newop = (o), oldop = GET_OPCODE(i); \
  if (ISBK(oldop)) \
    newop |= oldop & 1; \
  (i) = (((i)&MASK0(SIZE_OP,POS_OP)) | \
  ((cast(Instruction, newop)<<POS_OP)&MASK1(SIZE_OP,POS_OP))); \
} while (0)

#define GETARG_A(i)  (cast(int, ((i)>>POS_A) & MASK1(SIZE_A,0)))
#define SETARG_A(i,u)  ((i) = (((i)&MASK0(SIZE_A,POS_A)) | \
    ((cast(Instruction, u)<<POS_A)&MASK1(SIZE_A,POS_A))))

#define GETARG_B(i) \
  (cast(int, ((i)>>POS_B) & MASK1(SIZE_B+ISBK(GET_OPCODE(i)),0)))
#define SETARG_B(i,b) do { \
  OpCode oldop = GET_OPCODE(i); \
  int masksize = SIZE_B + ISBK(oldop); \
  (i) = (((i)&MASK0(masksize,POS_B)) | \
  ((cast(Instruction, b)<<POS_B)&MASK1(masksize,POS_B))); \
} while (0)


#define GETARG_C(i)  (cast(int, ((i)>>POS_C) & MASK1(SIZE_C,0)))
#define SETARG_C(i,b)  ((i) = (((i)&MASK0(SIZE_C,POS_C)) | \
    ((cast(Instruction, b)<<POS_C)&MASK1(SIZE_C,POS_C))))

#define GETARG_Bx(i)  (cast(int, ((i)>>POS_Bx) & MASK1(SIZE_Bx,0)))
#define SETARG_Bx(i,b)  ((i) = (((i)&MASK0(SIZE_Bx,POS_Bx)) | \
    ((cast(Instruction, b)<<POS_Bx)&MASK1(SIZE_Bx,POS_Bx))))

#define GETARG_sBx(i)  (GETARG_Bx(i)-MAXARG_sBx)
#define SETARG_sBx(i,b)  SETARG_Bx((i),cast(unsigned int, (b)+MAXARG_sBx))


#define CREATE_ABC(o,a,b,c)  ((cast(Instruction, o)<<POS_OP) \
      | (cast(Instruction, a)<<POS_A) \
      | (cast(Instruction, b)<<POS_B) \
      | (cast(Instruction, c)<<POS_C))

#define CREATE_ABx(o,a,bc)  ((cast(Instruction, o)<<POS_OP) \
      | (cast(Instruction, a)<<POS_A) \
      | (cast(Instruction, bc)<<POS_Bx))


/*
** Macros to operate RK indices
*/

/* this bit 1 means constant (0 means register) */
#define BITRK    (1 << (SIZE_C - 1))

/* test whether value is a constant */
#define ISK(x)    ((x) & BITRK)

/* gets the index of the constant */
#define INDEXK(r)  ((int)(r) & ~BITRK)

#if !defined HKSC_NO_RK || defined ldecomp_c
#define MAXINDEXRK  (BITRK - 1)
#else
#define MAXINDEXRK  (-1)
#endif /* HKSC_NO_RK */

/* code a constant index as a RK value */
#define RKASK(x)  ((x) | BITRK)


/*
** invalid register that fits in 8 bits
*/
#define NO_REG    MAXARG_A


#define SLOTMT_POS_TYPE (SIZE_B - 4)  /* need 4 bits for Lua types */
#define SLOTMT_TAG_CHAIN_MASK ((1 << SLOTMT_POS_TYPE) - 1)
#define GET_SLOTMT_TYPE(i) ((GETARG_B(i) >> SLOTMT_POS_TYPE) & 15)
#define GET_SLOTMT_TAGCHAIN(i) (GETARG_B(i) & SLOTMT_TAG_CHAIN_MASK)

#define DEFCODE(name,m,t,a,b,c,mr1,ur1,vr1) OP_##name,
typedef enum {
#include "lopcodes.def"
  OP_MAX
} OpCode;
#undef DEFCODE


#define NUM_OPCODES (cast(int, OP_MAX))


enum OpArgMask {
  OpArgN,  /* argument is not used */
  OpArgU,  /* argument is used */
  OpArgUK, /* argument is a used constant or a jump offset: */
  OpArgR,  /* argument is a register */
  OpArgRK, /* argument is a register/constant */
  OpArgK,  /* argument is a constant */
  OpArgR1UseRegister
};

/* R1 modes */
enum OpR1Mode {
  NR1, /* not an R1 instruction */
  R1A,
  R1B
};


/*
** instruction properties
** currently using 20 bits for each opcode
** enum OpMode opmode : 2;
** enum OpArgMask bmode : 3;
** enum OpArgMask cmode : 3;
** int useRA : 1;
** int tmode : 1;
** int makeR1 : 1;
** enum OpR1Mode r1Mode : 2;
** OpCode r1Version : SIZE_OP;
*/
typedef lu_byte OpCodeDesc[3];


LUAI_DATA const OpCodeDesc luaP_opmodes[NUM_OPCODES];

#define getOpMode(m)  (cast(enum OpMode, luaP_opmodes[m][0] & 3))
#define getCMode(m)  (cast(enum OpArgMask, (luaP_opmodes[m][0] >> 5) & 7))
#define getBMode(m)  (cast(enum OpArgMask, (luaP_opmodes[m][0] >> 2) & 7))
#define testAMode(m)  (cast(int, luaP_opmodes[m][1] & (1 << 0)))
#define testTMode(m)  (cast(int, luaP_opmodes[m][1] & (1 << 1)))
#define testMakeR1(m)  (cast(int, luaP_opmodes[m][1] & (1 << 2)))
#define getR1Mode(m)  (cast(enum OpR1Mode, (luaP_opmodes[m][1] >> 3) & 3))
#define getR1Version(m)  (cast(OpCode, luaP_opmodes[m][2]))


LUAI_DATA const char *const luaP_opnames[NUM_OPCODES+1];  /* opcode names */
#define getOpName(m)  (luaP_opnames[m])

/* number of list items to accumulate before a SETLIST instruction */
#define LFIELDS_PER_FLUSH  50

#define CASE_OP_CALL \
  case OP_CALL: case OP_CALL_I: case OP_CALL_I_R1: case OP_CALL_C: \
  case OP_CALL_M: CASE_OP_TAILCALL

#define CASE_OP_TAILCALL \
  case OP_TAILCALL: case OP_TAILCALL_I: case OP_TAILCALL_I_R1: \
  case OP_TAILCALL_C: case OP_TAILCALL_M

#define IS_OP_CALL(o) \
  ((o) == OP_CALL || (o) == OP_CALL_I || (o) == OP_CALL_I_R1 || \
   (o) == OP_CALL_C || (o) == OP_CALL_M || IS_OP_TAILCALL(o))

#define IS_OP_TAILCALL(o) \
  ((o) == OP_TAILCALL || (o) == OP_TAILCALL_I || (o) == OP_TAILCALL_I_R1 || \
   (o) == OP_TAILCALL_C || (o) == OP_TAILCALL_M)


#define IS_OP_SETTABLE(o) \
  ((o) == OP_SETFIELD || (o) == OP_SETFIELD_R1 || (o) == OP_SETTABLE || \
   (o) == OP_SETTABLE_BK || (o) == OP_SETTABLE_N || (o) == OP_SETTABLE_N_BK || \
   (o) == OP_SETTABLE_S || (o) == OP_SETTABLE_S_BK)

#define IS_OP_SETSLOT(o) \
  ((o) == OP_SETSLOTN || (o) == OP_SETSLOTI || (o) == OP_SETSLOT || \
   (o) == OP_SETSLOTS || (o) == OP_SETSLOTMT)

#define CASE_OP_SETTABLE \
  case OP_SETFIELD: case OP_SETFIELD_R1: case OP_SETTABLE: \
  case OP_SETTABLE_BK: case OP_SETTABLE_N: case OP_SETTABLE_N_BK: \
  case OP_SETTABLE_S: case OP_SETTABLE_S_BK

#define CASE_OP_GETTABLE \
  case OP_GETFIELD: case OP_GETFIELD_R1: case OP_GETTABLE: \
  case OP_GETTABLE_S: case OP_GETTABLE_N

#endif
