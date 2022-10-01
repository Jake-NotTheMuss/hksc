/*
** $Id: lfunc.h,v 2.3 2005/02/18 12:40:02 roberto Exp roberto $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/

#ifndef lfunc_h
#define lfunc_h

#include "hksc_begin_code.h"

#include "lobject.h"


#define sizeCclosure(n)	(cast(int, sizeof(CClosure)) + \
                         cast(int, sizeof(TValue)*((n)-1)))

#define sizeLclosure(n)	(cast(int, sizeof(LClosure)) + \
                         cast(int, sizeof(TValue *)*((n)-1)))


LUAI_FUNC Proto *luaF_newproto (void);
LUAI_FUNC Closure *luaF_newCclosure (int nelems, Table *e);
LUAI_FUNC Closure *luaF_newLclosure (int nelems, Table *e);
LUAI_FUNC UpVal *luaF_newupval (void);
LUAI_FUNC UpVal *luaF_findupval (StkId level);
LUAI_FUNC void luaF_close (StkId level);
LUAI_FUNC void luaF_freeproto (Proto *f);
LUAI_FUNC void luaF_freeclosure (Closure *c);
LUAI_FUNC void luaF_freeupval (UpVal *uv);
LUAI_FUNC const char *luaF_getlocalname (const Proto *func, int local_number,
                                         int pc);


#endif
