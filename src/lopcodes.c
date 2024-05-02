/*
** $Id: lopcodes.c $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE


#include "lopcodes.h"

/*
** opcode names
*/
#define DEFCODE(name,m,t,a,b,c,mr1,ur1,vr1) #name,
const char *const luaP_opnames[NUM_OPCODES+1] = {
#include "lopcodes.def"
  NULL
};
#undef DEFCODE


/*
** opcode modes
*/
#define DEFCODE(name,mode,test,useRA,bmode,cmode,makeR1,useR1,r1Version) \
  { cast_byte(i##mode | (OpArg##bmode<<2) | (OpArg##cmode<<5)), \
    cast_byte((useRA) | ((test)<<1) | ((makeR1)<<2) | ((useR1)<<3)), \
    cast_byte(r1Version) },
const OpCodeDesc luaP_opmodes[NUM_OPCODES] = {
#include "lopcodes.def"
};
#undef DEFCODE


#define ISBK(o) \
  (getOpMode(o) == iABC && (getBMode(o) == OpArgUK || getBMode(o) == OpArgRK))


Instruction luaP_set_opcode(Instruction *i, OpCode o) {
  Instruction newOp = *i;
  OpCode oldOpCode = GET_OPCODE(newOp);
  if (ISBK(oldOpCode))
    o = o | (oldOpCode & 1);
  (newOp) = (((newOp)&MASK0(SIZE_OP,POS_OP)) |
    ((cast(Instruction, o)<<POS_OP)&MASK1(SIZE_OP,POS_OP)));
  *i = newOp;
  return newOp;
}

int luaP_getarg_b(Instruction i) {
  OpCode oldOpCode = GET_OPCODE(i);
  int size = SIZE_B + ISBK(oldOpCode);
  return (cast(int, ((i)>>POS_B) & MASK1(size,0)));
}

Instruction luaP_setarg_b(Instruction *i, int b) {
  Instruction newOp = *i;
  OpCode oldOpCode = GET_OPCODE(newOp);
  int size = SIZE_B + ISBK(oldOpCode);
  (newOp) = (((newOp)&MASK0(size,POS_B)) |
    ((cast(Instruction, b)<<POS_B)&MASK1(size,POS_B)));
  *i = newOp;
  return newOp;
}

