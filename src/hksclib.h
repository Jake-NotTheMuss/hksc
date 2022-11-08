/*
** hksclib.h
** A wrapper for the modded Lua library
** See Copyright Notice in lua.h
*/


#ifndef HKSC_LIB_H
#define HKSC_LIB_H

#include "lstate.h"


LUAI_FUNC hksc_State *hksI_newstate(void);
LUAI_FUNC int hksc_parsefile(hksc_State *H, const char *filename);

/* extra error code for `luaL_load' */
#define LUA_ERRFILE     (LUA_ERRERR+1)

#endif /* HKSC_LIB_H */
