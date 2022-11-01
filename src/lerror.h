/*
** $Id: ldo.h,v 2.6 2005/08/22 19:58:29 roberto Exp roberto $
** Stack and Call structure of Lua
** See Copyright Notice in lua.h
*/

#ifndef ldo_h
#define ldo_h

#include <stdarg.h>

#include "lobject.h"
#include "lstate.h"
#include "lzio.h"

#define hksc_luaD_setliteralmsg(H,s) \
  (luaE_seterrormsg(H, getstr(luaS_newliteral(H, s))))

#define hksc_luaD_setmsg(H,s) (luaE_seterrormsg(H, getstr(luaS_new(H, s))))

/* I give the option to omit `hksc_' prefix on existing APIs */
#define luaD_protectedparser hksc_luaD_protectedparser
#define luaD_pcall hksc_luaD_pcall
#define luaD_throw hksc_luaD_throw
#define luaD_rawrunprotected hksc_luaD_rawrunprotected
#define luaD_seterrorobj hksc_luaD_seterrorobj
/* whereas I give the option to omit the `'luaD_' midfix on hksc extensions */
#define hksc_setliteralmsg hksc_luaD_setliteralmsg
#define hksc_setmsg hksc_luaD_setmsg
#define hksc_setvfmsg hksc_luaD_setvfmsg
#define hksc_setfmsg hksc_luaD_setfmsg

/* type of protected functions, to be ran by `runprotected' */
typedef void (*Pfunc) (hksc_State *H, void *ud);

LUAI_FUNC int hksc_luaD_protectedparser (hksc_State *H, ZIO *z,
                                    const char *name, Proto **pf);
LUAI_FUNC int hksc_luaD_pcall (hksc_State *H, Pfunc func, void *u);
LUAI_FUNC void hksc_luaD_throw (hksc_State *H, int errcode);
LUAI_FUNC int hksc_luaD_rawrunprotected (hksc_State *H, Pfunc f, void *ud);

LUAI_FUNC void hksc_luaD_seterrorobj (hksc_State *H, int errcode);

LUAI_FUNC void hksc_luaD_setvfmsg (hksc_State *H, const char *fmt,
                                         va_list argp);
LUAI_FUNC void hksc_luaD_setfmsg (hksc_State *H, const char *fmt, ...);

#endif
