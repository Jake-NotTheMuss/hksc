/*
** $Id: ldo.h $
** Stack and Call structure of Lua
** See Copyright Notice in lua.h
*/

#ifndef ldo_h
#define ldo_h

#include <stdarg.h>

#include "lobject.h"
#include "lstate.h"
#include "lzio.h"

#define hksc_setliteralmsg(H,s) \
  (luaE_seterrormsg(H, getstr(luaS_newliteral(H, s))))

#define hksc_setmsg(H,s) (luaE_seterrormsg(H, getstr(luaS_new(H, s))))

/* hksc additions */
#define hksc_setvfmsg luaD_setvfmsg
#define hksc_setfmsg luaD_setfmsg

/* type of protected functions, to be ran by `runprotected' */
typedef void (*Pfunc) (hksc_State *H, void *ud);

LUAI_FUNC int luaD_protectedparser (hksc_State *H, ZIO *z, const char *name);
LUAI_FUNC int luaD_pcall (hksc_State *H, Pfunc func, void *u);
LUAI_FUNC void luaD_throw (hksc_State *H, int errcode);
LUAI_FUNC int luaD_rawrunprotected (hksc_State *H, Pfunc f, void *ud);

LUAI_FUNC void luaD_seterrorobj (hksc_State *H, int errcode);

LUAI_FUNC void luaD_setvfmsg (hksc_State *H, const char *fmt, va_list argp);
LUAI_FUNC void luaD_setfmsg (hksc_State *H, const char *fmt, ...);

#endif
