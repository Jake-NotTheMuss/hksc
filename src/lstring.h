/*
** $Id: lstring.h,v 1.42 2005/02/23 17:30:22 roberto Exp roberto $
** String table (keep all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#ifndef lstring_h
#define lstring_h

#include <string.h>

#include "hksc_begin_code.h"

#include "lgc.h"
#include "lobject.h"
#include "lstate.h"

#define sizestring(s) (sizeof(union TString)+((s)->len+1)*sizeof(char))

#define sizeudata(u)  (sizeof(union Udata)+(u)->len)

#define luaS_new(H, s)  (luaS_newlstr(H, s, strlen(s)))
#define luaS_newliteral(H, s) (luaS_newlstr(H, "" s, \
                                 (sizeof(s)/sizeof(char))-1))

#define luaS_fix(s) l_setbit((s)->tsv.marked, FIXEDBIT)

LUAI_FUNC void luaS_resize (hksc_State *H, int newsize);
LUAI_FUNC TString *luaS_newlstr (hksc_State *H, const char *str, size_t l);


#endif
