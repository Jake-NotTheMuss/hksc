/*
** $Id: lstring.h $
** String table (keep all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#ifndef lstring_h
#define lstring_h


#include "lgc.h"
#include "lobject.h"
#include "lstate.h"


#define sizestring(s) (sizeof(union TString)+((s)->len+1)*sizeof(char))

#define sizeudata(u)  (sizeof(union Udata)+(u)->len)

#define luaS_new(H, s)  (luaS_newlstr(H, s, strlen(s)))
#define luaS_newliteral(H, s) (luaS_newlstr(H, "" s, \
                                 (sizeof(s)/sizeof(char))-1))

#define luaS_fix(s) l_setbit((s)->tsv.marked, GC_FIXED)

#define MAINCHUNKNAME "(main chunk)"

LUAI_FUNC void luaS_resize (hksc_State *H, int newsize);
#define luaS_newproto(H,n) luaS_newudata(H,sizestruct(n),NULL)
LUAI_FUNC Udata *luaS_newudata (hksc_State *H, size_t s, Table *e);
LUAI_FUNC TString *luaS_newlstr (hksc_State *H, const char *str, size_t l);

#ifdef LUA_CODT6
#define luaS_cod_hash(H, s)  (luaS_cod_hashstring(H, s, strlen(s)))
#define luaS_cod_hashliteral(H, s)  (luaS_cod_hashstring(H, "" s, \
                                      (sizeof(s)/sizeof(char))-1))
LUAI_FUNC lu_int32 luaS_cod_hashstring (hksc_State *H, const char *s,
                                        size_t l);
#endif /* LUA_CODT6 */

#endif
