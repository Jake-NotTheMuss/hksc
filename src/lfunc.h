/*
** $Id: lfunc.h,v 2.3 2005/02/18 12:40:02 roberto Exp roberto $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/

#ifndef lfunc_h
#define lfunc_h

#include "lobject.h"


LUAI_FUNC Proto *luaF_newproto (hksc_State *H);
LUAI_FUNC void luaF_freeproto (hksc_State *H, Proto *f);
LUAI_FUNC const char *luaF_getlocalname (const Proto *func, int local_number,
                                         int pc);
LUAI_FUNC void luaF_makehash (hksc_State *H, Proto *f);


#endif
