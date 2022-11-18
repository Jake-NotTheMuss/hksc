/*
** $Id: lopcodes.h,v 1.123 2005/10/23 17:37:55 roberto Exp roberto $
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
#define MASK1(n,p)  ((~((~(Instruction)0)<<n))<<p)

/* creates a mask with `n' 0 bits at position `p' */
#define MASK0(n,p)  (~MASK1(n,p))

/*
** the following macros help to manipulate instructions
*/

#define GET_OPCODE(i)  (cast(OpCode, ((i)>>POS_OP) & MASK1(SIZE_OP,0)))
#define SET_OPCODE(i,o)  ((i) = (((i)&MASK0(SIZE_OP,POS_OP)) | \
    ((cast(Instruction, o)<<POS_OP)&MASK1(SIZE_OP,POS_OP))))

#define GETARG_A(i)  (cast(int, ((i)>>POS_A) & MASK1(SIZE_A,0)))
#define SETARG_A(i,u)  ((i) = (((i)&MASK0(SIZE_A,POS_A)) | \
    ((cast(Instruction, u)<<POS_A)&MASK1(SIZE_A,POS_A))))

#define GETARG_B(i)  (cast(int, ((i)>>POS_B) & MASK1(SIZE_B,0)))
#define SETARG_B(i,b)  ((i) = (((i)&MASK0(SIZE_B,POS_B)) | \
    ((cast(Instruction, b)<<POS_B)&MASK1(SIZE_B,POS_B))))

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

#define MAXINDEXRK  (BITRK - 1)

/* code a constant index as a RK value */
#define RKASK(x)  ((x) | BITRK)


/*
** invalid register that fits in 8 bits
*/
#define NO_REG    MAXARG_A


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
*/
struct OpCodeDesc {
  enum OpMode mode;
  enum OpArgMask opc;
  enum OpArgMask opb;
  lu_byte useRA;
  lu_byte test;
  lu_byte makeR1;
  enum OpR1Mode r1Mode;
  OpCode r1Version;
};


LUAI_DATA const struct OpCodeDesc luaP_opmodes[NUM_OPCODES];

#define getOpMode(m)  (luaP_opmodes[m].mode)
#define getCMode(m)   (luaP_opmodes[m].opc)
#define getBMode(m)   (luaP_opmodes[m].opb)
#define testAMode(m)  (luaP_opmodes[m].useRA)
#define testTMode(m)  (luaP_opmodes[m].test)
#define testMakeR1(m) (luaP_opmodes[m].makeR1)
#define getR1Mode(m)  (luaP_opmodes[m].r1Mode)
#define getR1Version(m) (luaP_opmodes[m].r1Version)


LUAI_DATA const char *const luaP_opnames[NUM_OPCODES+1];  /* opcode names */
#define getOpName(m)  (luaP_opnames[m])

/* number of list items to accumulate before a SETLIST instruction */
#define LFIELDS_PER_FLUSH  50


#endif
