/*
** $Id: lstring.h,v 1.42 2005/02/23 17:30:22 roberto Exp roberto $
** String table (keep all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#ifndef lstring_h
#define lstring_h

#include <string.h>

#include "lgc.h"
#include "lobject.h"
#include "lstate.h"

#define sizestring(s) (sizeof(union TString)+((s)->len+1)*sizeof(char))

#define sizeudata(u)  (sizeof(union Udata)+(u)->len)

#define luaS_new(H, s)  (luaS_newlstr(H, s, strlen(s)))
#define luaS_newliteral(H, s) (luaS_newlstr(H, "" s, \
                                 (sizeof(s)/sizeof(char))-1))

#define luaS_fix(s) l_setbit((s)->tsv.marked, FIXEDBIT)

#define luaS_dbhash(H, s)  (luaS_dbhashlstr(H, s, strlen(s)))
#define luaS_dbhashliteral(H, s)  (luaS_dbhashlstr(H, "" s, \
                                    (sizeof(s)/sizeof(char))-1))

#define MAINCHUNKNAME "(main chunk)"

LUAI_DATA TString *luaS_mainchunk;

LUAI_FUNC void luaS_resize (hksc_State *H, int newsize);
LUAI_FUNC TString *luaS_newlstr (hksc_State *H, const char *str, size_t l);
LUAI_FUNC lu_int32 luaS_dbhashlstr (hksc_State *H, const char *str, size_t l);

#endif
