/*
** $Id: ltable.h,v 2.9 2006/01/10 12:51:53 roberto Exp roberto $
** Lua tables (hash)
** See Copyright Notice in lua.h
*/

#ifndef ltable_h
#define ltable_h

#include "hksc_begin_code.h"

#include "lobject.h"


#define gnode(t,i)	(&(t)->node[i])
#define gkey(n)		(&(n)->i_key.nk)
#define gval(n)		(&(n)->i_val)
#define gnext(n)	((n)->i_key.nk.next)

#define key2tval(n)	(&(n)->i_key.tvk)


LUAI_FUNC const TValue *luaH_getnum (Table *t, int key);
LUAI_FUNC TValue *luaH_setnum (hksc_State *H, Table *t, int key);
LUAI_FUNC const TValue *luaH_getstr (Table *t, TString *key);
LUAI_FUNC TValue *luaH_setstr (hksc_State *H, Table *t, TString *key);
LUAI_FUNC const TValue *luaH_get (Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_set (hksc_State *H, Table *t, const TValue *key);
LUAI_FUNC Table *luaH_new (hksc_State *H, int narray, int lnhash);
LUAI_FUNC void luaH_resizearray (hksc_State *H, Table *t, int nasize);
LUAI_FUNC void luaH_free (hksc_State *H, Table *t);
LUAI_FUNC int luaH_next (hksc_State *H, Table *t, StkId key);
LUAI_FUNC int luaH_getn (Table *t);


#if defined(LUA_DEBUG)
LUAI_FUNC Node *luaH_mainposition (const Table *t, const TValue *key);
LUAI_FUNC int luaH_isdummy (Node *n);
#endif


#endif
