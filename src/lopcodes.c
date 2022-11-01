/*
** $Id: lopcodes.c,v 1.36 2005/10/13 12:22:05 roberto Exp roberto $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE

#include "hksc_begin_code.h"


#include "lopcodes.h"

/*
** opcode names
*/
#define DEFCODE(name, m, t, a, b, c) #name,
const char *const luaP_opnames[NUM_OPCODES+1] = {
#include "lopcodes.def"
  NULL
};
#undef DEFCODE


#define opmode(t,a,b,c,m) (((t)<<7) | ((a)<<6) | ((b)<<4) | ((c)<<2) | (m))

/*
** opcode modes
*/
#define DEFCODE(n, mode, test, seta, bmode, cmode) \
  opmode(test, seta, OpArg##bmode, OpArg##cmode, i##mode),
const lu_byte luaP_opmodes[NUM_OPCODES] = {
#include "lopcodes.def"
};
#undef DEFCODE

