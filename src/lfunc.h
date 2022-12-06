/*
** $Id: lfunc.h $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/

#ifndef lfunc_h
#define lfunc_h

#include "lua.h"

#include "lobject.h"


LUAI_FUNC Proto *luaF_newproto (hksc_State *H);
LUAI_FUNC void luaF_freeproto (hksc_State *H, Proto *f);
LUAI_FUNC const char *luaF_getlocalname (const Proto *func, int local_number,
                                         int pc);
#ifdef LUA_COD
LUAI_FUNC void luaF_hash (hksc_State *H, Proto *f);
#endif /* LUA_COD */


#endif
