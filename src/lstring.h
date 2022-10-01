/*
** $Id: lstring.h,v 1.42 2005/02/23 17:30:22 roberto Exp roberto $
** String table (keep all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#ifndef lstring_h
#define lstring_h

#include <string.h>

#include "hksc_begin_code.h"

#include "lobject.h"

/* macros to convert a GCObject into a specific value */
#define rawgco2ts(o)  check_exp((o)->gch.tt == LUA_TSTRING, &((o)->ts))
#define gco2ts(o) (&rawgco2ts(o)->tsv)
#define rawgco2u(o) check_exp((o)->gch.tt == LUA_TUSERDATA, &((o)->u))
#define gco2u(o)  (&rawgco2u(o)->uv)
#define gco2cl(o) check_exp((o)->gch.tt == LUA_TFUNCTION, &((o)->cl))
#define gco2h(o)  check_exp((o)->gch.tt == LUA_TTABLE, &((o)->h))
#define gco2p(o)  check_exp((o)->gch.tt == LUA_TPROTO, &((o)->p))
#define gco2uv(o) check_exp((o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define ngcotouv(o) \
  check_exp((o) == NULL || (o)->gch.tt == LUA_TUPVAL, &((o)->uv))
#define gco2th(o) check_exp((o)->gch.tt == LUA_TTHREAD, &((o)->th))

/* macro to convert any Lua object into a GCObject */
#define obj2gco(v)  (cast(GCObject *, (v)))

typedef struct stringtable {
  GCObject **hash;
  lu_int32 nuse;  /* number of elements */
  int size;
} stringtable;


/*
** Union of all collectable objects
*/
union GCObject {
  GCheader gch;
  union TString ts;
  union Udata u;
  union Closure cl;
  struct Table h;
  struct Proto p;
  struct UpVal uv;
};

extern stringtable hksc_strt;


#define sizestring(s)  (sizeof(union TString)+((s)->len+1)*sizeof(char))

#define sizeudata(u)  (sizeof(union Udata)+(u)->len)

#define luaS_new(s)  (luaS_newlstr(s, strlen(s)))
#define luaS_newliteral(s)  (luaS_newlstr("" s, \
                                 (sizeof(s)/sizeof(char))-1))

#define luaS_fix(s)  (void)0

LUAI_FUNC void luaS_resize (int newsize);
LUAI_FUNC TString *luaS_newlstr (const char *str, size_t l);


#endif
