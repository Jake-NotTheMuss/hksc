/*
** $Id: lapi.c $
** Lua API
** See Copyright Notice in lua.h
*/


#include <stdarg.h>
#include <string.h>

#define lapi_c
#define LUA_CORE

#include "lua.h"

/*#include "lapi.h"*/
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lundump.h"



const char lua_ident[] =
  "$Lua: " LUA_VERSION " " LUA_COPYRIGHT " $\n"
  "$Authors: " LUA_AUTHORS " $\n"
  "$URL: www.lua.org $\n";


LUA_API const char *lua_newstring (hksc_State *H, const char *str) {
  return getstr(luaS_new(H, str));
}


LUA_API const char *lua_newlstring (hksc_State *H, const char *str, size_t l) {
  return getstr(luaS_newlstr(H, str, l));
}


LUA_API const char *lua_newfstring (hksc_State *H, const char *fmt, ...) {
  const char *str;
  va_list argp;
  va_start(argp, fmt);
  str = luaO_pushvfstring(H, fmt, argp);
  va_end(argp);
  return str;
}


LUA_API const char *lua_newvfstring (hksc_State *H, const char *fmt,
                                     va_list argp) {
  return luaO_pushvfstring(H, fmt, argp);
}


LUA_API const char *lua_geterror (hksc_State *H) {
  return H->errormsg;
}


LUA_API void lua_seterror (hksc_State *H, const char *s) {
  hksc_seterror(H, getstr(luaS_new(H, s)));
}


LUA_API void lua_setferror (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  luaD_setvferror(H, fmt, argp);
  va_end(argp);
}


LUA_API void lua_setvferror (hksc_State *H, const char *fmt, va_list argp) {
  luaD_setvferror(H, fmt, argp);
}

LUA_API void lua_clearerror (hksc_State *H) {
  H->status = 0;
  H->errormsg = NULL;
}


LUA_API int lua_getmode (hksc_State *H) {
  int mode;
  mode = hksc_mode(H);
  return mode;
}


LUA_API void lua_setmode (hksc_State *H, int mode) {
  hksc_mode(H) = mode;
}


LUA_API lua_Alloc lua_getallocf (hksc_State *H, void **ud) {
  lua_Alloc f;
  lua_lock(H);
  if (ud) *ud = G(H)->ud;
  f = G(H)->frealloc;
  lua_unlock(H);
  return f;
}


LUA_API void lua_setallocf (hksc_State *H, lua_Alloc f, void *ud) {
  lua_lock(H);
  G(H)->ud = ud;
  G(H)->frealloc = f;
  lua_unlock(H);
}


#if defined(LUA_COD) && defined(HKSC_DECOMPILER)

LUA_API const char *lua_getDebugFile (hksc_State *H) {
  return H->currdebugfile;
}

LUA_API void lua_setDebugFile (hksc_State *H, const char *name) {
  H->currdebugfile = name;
}

#endif /* defined(LUA_COD) && defined(HKSC_DECOMPILER) */


LUA_API int lua_getEmitStruct (hksc_State *H) {
  return hksc_getEmitStruct(H);
}


LUA_API void lua_setEmitStruct (hksc_State *H, int emit_struct) {
  hksc_setEmitStruct(H, emit_struct);
}


LUA_API int lua_getIntLiteralsEnabled (hksc_State *H) {
  return hksc_getIntLiteralsEnabled(H);
}


LUA_API void lua_setIntLiteralsEnabled (hksc_State *H, int enable_int_literals)
{
  hksc_setIntLiteralsEnabled(H, enable_int_literals);
}


LUA_API int lua_getBytecodeStrippingLevel (hksc_State *H) {
  return hksc_getBytecodeStrippingLevel(H);
}


LUA_API void lua_setBytecodeStrippingLevel (hksc_State *H, int strip) {
  hksc_setBytecodeStrippingLevel(H, strip);
}


LUA_API int lua_getIgnoreDebug (hksc_State *H) {
  return hksc_getIgnoreDebug(H);
}


LUA_API void lua_setIgnoreDebug (hksc_State *H, int ignore_debug) {
  hksc_setIgnoreDebug(H, ignore_debug);
}


LUA_API int lua_getMatchLineInfo (hksc_State *H) {
  return hksc_getMatchLineInfo(H);
}


LUA_API void lua_setMatchLineInfo (hksc_State *H, int match_line_info) {
  hksc_setMatchLineInfo(H, match_line_info);
}

