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

