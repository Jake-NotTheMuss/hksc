/*
** $Id: lopcodes.c,v 1.36 2005/10/13 12:22:05 roberto Exp roberto $
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
  {i##mode,OpArg##cmode,OpArg##bmode,useRA,test,makeR1,useR1,r1Version},
const struct OpCodeDesc luaP_opmodes[NUM_OPCODES] = {
#include "lopcodes.def"
};
#undef DEFCODE

