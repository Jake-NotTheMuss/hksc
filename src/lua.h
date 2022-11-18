/*
** $Id: lua.h,v 1.215 2005/12/27 17:09:50 roberto Exp roberto $
** Lua - An Extensible Extension Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*/


#ifndef lua_h
#define lua_h

#include <stdarg.h>
#include <stddef.h>


#include "luaconf.h"


#define LUA_VERSION	"Lua 5.1"
#define LUA_VERSION_NUM	501
#define LUA_COPYRIGHT	"Copyright (C) 1994-2006 Lua.org, PUC-Rio"
#define LUA_AUTHORS 	"R. Ierusalimschy, H. H. de Figueiredo & W. Celes"

/* default extension of pre-compiled Lua files */
#define LUAC_EXT ".luac"
/* default extension of source Lua files */
#define LUA_EXT  ".lua"
/* default extension of decompiled Lua files */
#define LUADEC_EXT  ".dec.lua"

/* mark for precompiled code (`<esc>Lua') */
#define	LUA_SIGNATURE	"\033Lua"

/* option for multiple returns in `lua_pcall' and `lua_call' */
#define LUA_MULTRET	(-1)


/*
** pseudo-indices
*/
#define LUA_REGISTRYINDEX	(-10000)
#define LUA_ENVIRONINDEX	(-10001)
#define LUA_GLOBALSINDEX	(-10002)
#define lua_upvalueindex(i)	(LUA_GLOBALSINDEX-(i))


/* thread status; 0 is OK */
#define LUA_YIELD	1
#define LUA_ERRRUN	2
#define LUA_ERRSYNTAX	3
#define LUA_ERRMEM	4
#define LUA_ERRERR	5


typedef struct hksc_State hksc_State;

typedef int (*lua_CFunction) (hksc_State *H);


/*
** functions that read/write blocks when loading/dumping Lua chunks
*/
typedef const char * (*lua_Reader) (hksc_State *H, void *ud, size_t *sz);

typedef int (*lua_Writer) (hksc_State *H, const void* p, size_t sz, void* ud);


/*
** prototype for memory-allocation functions
*/
typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);


/*
** basic types
*/
#define LUA_TANY    (-2)
#define LUA_TNONE		(-1)

#define LUA_TNIL		0
#define LUA_TBOOLEAN		1
#define LUA_TLIGHTUSERDATA	2
#define LUA_TNUMBER		3
#define LUA_TSTRING		4
#define LUA_TTABLE		5
#define LUA_TFUNCTION		6
#define LUA_TUSERDATA		7
#define LUA_TTHREAD		8
#define LUA_TIFUNCTION  9
#define LUA_TCFUNCTION  10
#define LUA_TUI64       11
#define LUA_TSTRUCT     12

#define LUA_NUM_TYPE_OBJECTS 14


/* minimum Lua stack available to a C function */
#define LUA_MINSTACK	20


/*
** generic extra include file
*/
#if defined(LUA_USER_H)
#include LUA_USER_H
#endif


/* type of numbers in Lua */
typedef LUA_NUMBER lua_Number;


/* type for integer functions */
typedef LUA_INTEGER lua_Integer;


/*
** state manipulation
*/
LUA_API hksc_State *(lua_newstate) (lua_Alloc f, void *ud);
LUA_API void       (lua_close) (hksc_State *H);
LUA_API hksc_State *(lua_newthread) (hksc_State *H);

LUA_API lua_CFunction (lua_atpanic) (hksc_State *H, lua_CFunction panicf);


/*
** basic stack manipulation
*/
LUA_API int   (lua_gettop) (hksc_State *H);
LUA_API void  (lua_settop) (hksc_State *H, int idx);
LUA_API void  (lua_pushvalue) (hksc_State *H, int idx);
LUA_API void  (lua_remove) (hksc_State *H, int idx);
LUA_API void  (lua_insert) (hksc_State *H, int idx);
LUA_API void  (lua_replace) (hksc_State *H, int idx);
LUA_API int   (lua_checkstack) (hksc_State *H, int sz);

LUA_API void  (lua_xmove) (hksc_State *from, hksc_State *to, int n);


/*
** access functions (stack -> C)
*/

LUA_API int             (lua_isnumber) (hksc_State *H, int idx);
LUA_API int             (lua_isstring) (hksc_State *H, int idx);
LUA_API int             (lua_iscfunction) (hksc_State *H, int idx);
LUA_API int             (lua_isuserdata) (hksc_State *H, int idx);
LUA_API int             (lua_type) (hksc_State *H, int idx);
LUA_API const char     *(lua_typename) (hksc_State *H, int tp);

LUA_API int            (lua_equal) (hksc_State *H, int idx1, int idx2);
LUA_API int            (lua_rawequal) (hksc_State *H, int idx1, int idx2);
LUA_API int            (lua_lessthan) (hksc_State *H, int idx1, int idx2);

LUA_API lua_Number      (lua_tonumber) (hksc_State *H, int idx);
LUA_API lua_Integer     (lua_tointeger) (hksc_State *H, int idx);
LUA_API int             (lua_toboolean) (hksc_State *H, int idx);
LUA_API const char     *(lua_tolstring) (hksc_State *H, int idx, size_t *len);
LUA_API size_t          (lua_objlen) (hksc_State *H, int idx);
LUA_API lua_CFunction   (lua_tocfunction) (hksc_State *H, int idx);
LUA_API void	       *(lua_touserdata) (hksc_State *H, int idx);
LUA_API hksc_State      *(lua_tothread) (hksc_State *H, int idx);
LUA_API const void     *(lua_topointer) (hksc_State *H, int idx);


/*
** push functions (C -> stack)
*/
LUA_API void  (lua_pushnil) (hksc_State *H);
LUA_API void  (lua_pushnumber) (hksc_State *H, lua_Number n);
LUA_API void  (lua_pushinteger) (hksc_State *H, lua_Integer n);
LUA_API void  (lua_pushlstring) (hksc_State *H, const char *s, size_t l);
LUA_API void  (lua_pushstring) (hksc_State *H, const char *s);
LUA_API const char *(lua_pushvfstring) (hksc_State *H, const char *fmt,
                                                      va_list argp);
LUA_API const char *(lua_pushfstring) (hksc_State *H, const char *fmt, ...);
LUA_API void  (lua_pushcclosure) (hksc_State *H, lua_CFunction fn, int n);
LUA_API void  (lua_pushboolean) (hksc_State *H, int b);
LUA_API void  (lua_pushlightuserdata) (hksc_State *H, void *p);
LUA_API int   (lua_pushthread) (hksc_State *H);


/*
** get functions (Lua -> stack)
*/
LUA_API void  (lua_gettable) (hksc_State *H, int idx);
LUA_API void  (lua_getfield) (hksc_State *H, int idx, const char *k);
LUA_API void  (lua_rawget) (hksc_State *H, int idx);
LUA_API void  (lua_rawgeti) (hksc_State *H, int idx, int n);
LUA_API void  (lua_createtable) (hksc_State *H, int narr, int nrec);
LUA_API void *(lua_newuserdata) (hksc_State *H, size_t sz);
LUA_API int   (lua_getmetatable) (hksc_State *H, int objindex);
LUA_API void  (lua_getfenv) (hksc_State *H, int idx);


/*
** set functions (stack -> Lua)
*/
LUA_API void  (lua_settable) (hksc_State *H, int idx);
LUA_API void  (lua_setfield) (hksc_State *H, int idx, const char *k);
LUA_API void  (lua_rawset) (hksc_State *H, int idx);
LUA_API void  (lua_rawseti) (hksc_State *H, int idx, int n);
LUA_API int   (lua_setmetatable) (hksc_State *H, int objindex);
LUA_API int   (lua_setfenv) (hksc_State *H, int idx);


/*
** `load' and `call' functions (load and run Lua code)
*/
LUA_API void  (lua_call) (hksc_State *H, int nargs, int nresults);
LUA_API int   (lua_pcall) (hksc_State *H, int nargs, int nresults, int errfunc);
LUA_API int   (lua_cpcall) (hksc_State *H, lua_CFunction func, void *ud);
LUA_API int   (lua_load) (hksc_State *H, lua_Reader reader, void *dt,
                                        const char *chunkname);

LUA_API int (lua_dump) (hksc_State *H, lua_Writer writer, void *data);


/*
** coroutine functions
*/
LUA_API int  (lua_yield) (hksc_State *H, int nresults);
LUA_API int  (lua_resume) (hksc_State *H, int narg);
LUA_API int  (lua_status) (hksc_State *H);

/*
** garbage-collection function and options
*/

#define LUA_GCSTOP		0
#define LUA_GCRESTART		1
#define LUA_GCCOLLECT		2
#define LUA_GCCOUNT		3
#define LUA_GCCOUNTB		4
#define LUA_GCSTEP		5
#define LUA_GCSETPAUSE		6
#define LUA_GCSETSTEPMUL	7

LUA_API int (lua_gc) (hksc_State *H, int what, int data);


/*
** miscellaneous functions
*/

LUA_API int   (lua_error) (hksc_State *H);

LUA_API int   (lua_next) (hksc_State *H, int idx);

LUA_API void  (lua_concat) (hksc_State *H, int n);

LUA_API lua_Alloc (lua_getallocf) (hksc_State *H, void **ud);
LUA_API void lua_setallocf (hksc_State *H, lua_Alloc f, void *ud);



/* 
** ===============================================================
** some useful macros
** ===============================================================
*/

#define lua_pop(H,n)		lua_settop(H, -(n)-1)

#define lua_newtable(H)		lua_createtable(H, 0, 0)

#define lua_register(H,n,f) (lua_pushcfunction(H, (f)), lua_setglobal(H, (n)))

#define lua_pushcfunction(H,f)	lua_pushcclosure(H, (f), 0)

#define lua_strlen(H,i)		lua_objlen(H, (i))

#define lua_isfunction(H,n)	(lua_type(H, (n)) == LUA_TFUNCTION)
#define lua_istable(H,n)	(lua_type(H, (n)) == LUA_TTABLE)
#define lua_islightuserdata(H,n)	(lua_type(H, (n)) == LUA_TLIGHTUSERDATA)
#define lua_isnil(H,n)		(lua_type(H, (n)) == LUA_TNIL)
#define lua_isboolean(H,n)	(lua_type(H, (n)) == LUA_TBOOLEAN)
#define lua_isthread(H,n)	(lua_type(H, (n)) == LUA_TTHREAD)
#define lua_isnone(H,n)		(lua_type(H, (n)) == LUA_TNONE)
#define lua_isnoneornil(H, n)	(lua_type(H, (n)) <= 0)

#define lua_pushliteral(H, s)	\
	lua_pushlstring(H, "" s, (sizeof(s)/sizeof(char))-1)

#define lua_setglobal(H,s)	lua_setfield(H, LUA_GLOBALSINDEX, (s))
#define lua_getglobal(H,s)	lua_getfield(H, LUA_GLOBALSINDEX, (s))

#define lua_tostring(H,i)	lua_tolstring(H, (i), NULL)



/*
** compatibility macros and functions
*/

#define lua_open()	luaL_newstate()

#define lua_getregistry(H)	lua_pushvalue(H, LUA_REGISTRYINDEX)

#define lua_getgccount(H)	lua_gc(H, LUA_GCCOUNT, 0)

#define lua_Chunkreader		lua_Reader
#define lua_Chunkwriter		lua_Writer



/*
** {======================================================================
** Debug API
** =======================================================================
*/


/*
** Event codes
*/
#define LUA_HOOKCALL	0
#define LUA_HOOKRET	1
#define LUA_HOOKLINE	2
#define LUA_HOOKCOUNT	3
#define LUA_HOOKTAILRET 4


/*
** Event masks
*/
#define LUA_MASKCALL	(1 << LUA_HOOKCALL)
#define LUA_MASKRET	(1 << LUA_HOOKRET)
#define LUA_MASKLINE	(1 << LUA_HOOKLINE)
#define LUA_MASKCOUNT	(1 << LUA_HOOKCOUNT)

typedef struct lua_Debug lua_Debug;  /* activation record */


/* Functions to be called by the debuger in specific events */
typedef void (*lua_Hook) (hksc_State *H, lua_Debug *ar);


LUA_API int lua_getstack (hksc_State *H, int level, lua_Debug *ar);
LUA_API int lua_getinfo (hksc_State *H, const char *what, lua_Debug *ar);
LUA_API const char *lua_getlocal (hksc_State *H, const lua_Debug *ar, int n);
LUA_API const char *lua_setlocal (hksc_State *H, const lua_Debug *ar, int n);
LUA_API const char *lua_getupvalue (hksc_State *H, int funcindex, int n);
LUA_API const char *lua_setupvalue (hksc_State *H, int funcindex, int n);

LUA_API int lua_sethook (hksc_State *H, lua_Hook func, int mask, int count);
LUA_API lua_Hook lua_gethook (hksc_State *H);
LUA_API int lua_gethookmask (hksc_State *H);
LUA_API int lua_gethookcount (hksc_State *H);


struct lua_Debug {
  int event;
  const char *name;	/* (n) */
  const char *namewhat;	/* (n) `global', `local', `field', `method' */
  const char *what;	/* (S) `Lua', `C', `main', `tail' */
  const char *source;	/* (S) */
  int currentline;	/* (l) */
  int nups;		/* (u) number of upvalues */
  int linedefined;	/* (S) */
  int lastlinedefined;	/* (S) */
  char short_src[LUA_IDSIZE]; /* (S) */
  /* private part */
  int i_ci;  /* active function */
};

/* }====================================================================== */


/******************************************************************************
* Copyright (C) 1994-2006 Lua.org, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/


#endif
