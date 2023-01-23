/*
** $Id: ldo.h $
** Stack and Call structure of Lua
** See Copyright Notice in lua.h
*/

#include <stdarg.h>

#ifndef ldo_h
#define ldo_h

#include "lobject.h"
#include "lstate.h"
#include "lzio.h"

/* type of protected functions, to be ran by `runprotected' */
typedef void (*Pfunc) (hksc_State *H, void *ud);

LUAI_FUNC int luaD_protectedparser (hksc_State *H, ZIO *z, const char *name);
LUAI_FUNC int luaD_pcall (hksc_State *H, Pfunc func, void *u);
LUAI_FUNC void luaD_throw (hksc_State *H, int errcode);
LUAI_FUNC int luaD_rawrunprotected (hksc_State *H, Pfunc f, void *ud);

LUAI_FUNC void luaD_seterrorobj (hksc_State *H, int errcode);

LUAI_FUNC void luaD_setvferror (hksc_State *H, const char *fmt, va_list argp);
LUAI_FUNC void luaD_setferror (hksc_State *H, const char *fmt, ...);

#endif
