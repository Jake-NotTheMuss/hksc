/*
** $Id: lapi.c $
** Lua API
** See Copyright Notice in lua.h
*/


#include <stdarg.h>
#include <string.h>

#define lapi_c
#define LUA_CORE

#include "hksclua.h"

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


LUA_API lua_CFunction lua_atpanic (hksc_State *H, lua_CFunction panicf) {
  lua_CFunction old;
  lua_lock(H);
  old = G(H)->panic;
  G(H)->panic = panicf;
  lua_unlock(H);
  return old;
}


LUA_API hksc_CycleCallback lua_onstartcycle (hksc_State *H,
                                             hksc_CycleCallback startcycle) {
  hksc_CycleCallback old;
  lua_lock(H);
  old = G(H)->startcycle;
  G(H)->startcycle = startcycle;
  lua_unlock(H);
  return old;
}


LUA_API hksc_CycleCallback lua_onendcycle (hksc_State *H,
                                           hksc_CycleCallback endcycle) {
  hksc_CycleCallback old;
  lua_lock(H);
  old = G(H)->endcycle;
  G(H)->endcycle = endcycle;
  lua_unlock(H);
  return old;
}


LUA_API const char *lua_newfixedlstring (hksc_State *H, const char *str,
                                         size_t l) {
  TString *ts;
  const char *retstr;
  lua_lock(H);
  luaC_checkGC(H);
  ts = luaS_newlstr(H, str, l);
  luaS_fix(ts);
  retstr = getstr(ts);
  lua_unlock(H);
  return retstr;
}


LUA_API const char *lua_newfixedstring (hksc_State *H, const char *str) {
  return lua_newfixedlstring(H, str, strlen(str));
}


LUA_API const char *lua_newlstring (hksc_State *H, const char *str, size_t l) {
  const char *retstr;
  lua_lock(H);
  luaC_checkGC(H);
  retstr = getstr(luaS_newlstr(H, str, l));
  lua_unlock(H);
  return retstr;
}


LUA_API const char *lua_newstring (hksc_State *H, const char *str) {
  return lua_newlstring(H, str, strlen(str));
}


LUA_API const char *lua_newfstring (hksc_State *H, const char *fmt, ...) {
  const char *str;
  va_list argp;
  lua_lock(H);
  va_start(argp, fmt);
  str = luaO_pushvfstring(H, fmt, argp);
  va_end(argp);
  lua_unlock(H);
  return str;
}


LUA_API const char *lua_newvfstring (hksc_State *H, const char *fmt,
                                     va_list argp) {
  const char *str;
  lua_lock(H);
  luaC_checkGC(H);
  str = luaO_pushvfstring(H, fmt, argp);
  lua_unlock(H);
  return str;
}


LUA_API const char *lua_geterror (hksc_State *H) {
  const char *errormsg;
  lua_lock(H);
  errormsg = H->errormsg;
  lua_unlock(H);
  return errormsg;
}


LUA_API void lua_seterror (hksc_State *H, const char *s) {
  lua_lock(H);
  luaC_checkGC(H);
  hksc_seterror(H, getstr(luaS_new(H, s)));
  lua_unlock(H);
}


LUA_API void lua_setferror (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  lua_lock(H);
  va_start(argp, fmt);
  luaD_setvferror(H, fmt, argp);
  va_end(argp);
  lua_unlock(H);
}


LUA_API void lua_setvferror (hksc_State *H, const char *fmt, va_list argp) {
  lua_lock(H);
  luaD_setvferror(H, fmt, argp);
  lua_unlock(H);
}


LUA_API void lua_clearerror (hksc_State *H) {
  lua_lock(H);
  H->status = 0;
  H->errormsg = NULL;
  lua_unlock(H);
}


LUA_API int lua_getmode (hksc_State *H) {
  int mode;
  lua_lock(H);
  mode = hksc_mode(H);
  lua_unlock(H);
  return mode;
}


LUA_API void lua_setmode (hksc_State *H, int mode) {
  lua_lock(H);
  hksc_mode(H) = mode;
  lua_unlock(H);
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


LUA_API void lua_setprefixmap (hksc_State *H, const char *from, const char *to)
{
  lua_lock(H);
  G(H)->prefix_map_from = from;
  G(H)->prefix_map_to = to;
  lua_unlock(H);
}


#if defined(LUA_CODT6)

LUA_API const char *lua_getdebugfile (hksc_State *H) {
  const char *currdebugfile;
  lua_lock(H);
  currdebugfile = H->currdebugfile;
  lua_unlock(H);
  return currdebugfile;
}

LUA_API void lua_setdebugfile (hksc_State *H, const char *name) {
  lua_lock(H);
  H->currdebugfile = name;
  lua_unlock(H);
}

#endif /* LUA_CODT6 */

/*
** compiler/decompiler settings (C -> stack)
*/

LUA_API int lua_getemitstruct (hksc_State *H) {
  int emit_struct;
  lua_lock(H);
  emit_struct = hksc_getemitstruct(H);
  lua_unlock(H);
  return emit_struct;
}


LUA_API void lua_setemitstruct (hksc_State *H, int emit_struct) {
  lua_lock(H);
  hksc_setemitstruct(H, emit_struct);
  lua_unlock(H);
}


LUA_API int lua_getintliteralsenabled (hksc_State *H) {
  int enable_int_literals;
  lua_lock(H);
  enable_int_literals = hksc_getintliteralsenabled(H);
  lua_unlock(H);
  return enable_int_literals;
}


LUA_API void lua_setintliteralsenabled (hksc_State *H, int enable_int_literals)
{
  lua_lock(H);
  hksc_setintliteralsenabled(H, enable_int_literals);
  lua_unlock(H);
}


LUA_API int lua_getbytecodestrippinglevel (hksc_State *H) {
  int strip;
  lua_lock(H);
  strip = hksc_getbytecodestrippinglevel(H);
  lua_unlock(H);
  return strip;
}


LUA_API void lua_setbytecodestrippinglevel (hksc_State *H, int strip) {
  lua_lock(H);
  hksc_setbytecodestrippinglevel(H, strip);
  lua_unlock(H);
}


LUA_API int lua_getignoredebug (hksc_State *H) {
  int ignore_debug;
  lua_lock(H);
  ignore_debug = hksc_getignoredebug(H);
  lua_unlock(H);
  return ignore_debug;
}


LUA_API void lua_setignoredebug (hksc_State *H, int ignore_debug) {
  lua_lock(H);
  hksc_setignoredebug(H, ignore_debug);
  lua_unlock(H);
}


#ifdef HKSC_DECOMPILER

LUA_API int lua_getmatchlineinfo (hksc_State *H) {
  int match_line_info;
  lua_lock(H);
  match_line_info = hksc_getmatchlineinfo(H);
  lua_unlock(H);
  return match_line_info;
}


LUA_API void lua_setmatchlineinfo (hksc_State *H, int match_line_info) {
  lua_lock(H);
  hksc_setmatchlineinfo(H, match_line_info);
  lua_unlock(H);
}

#endif /* HKSC_DECOMPILER */


LUA_API int lua_dump (hksc_State *H, lua_Writer w, void *data) {
  int status;
  const Proto *f;
  lua_lock(H);
  api_check(H, H->last_result != NULL);
  api_check(H, w != NULL);
  luaC_checkGC(H);
  f = H->last_result;
  status = luaU_dump(H, f, w, data);
  lua_unlock(H);
  return status;
}

#ifdef HKSC_DECOMPILER
/* decompile one chunk; from ldecomp.c */
LUA_API int lua_decompile (hksc_State *H, lua_Writer w, void *data) {
  int status;
  const Proto *f;
  lua_lock(H);
  api_check(H, H->last_result != NULL);
  api_check(H, w != NULL);
  luaC_checkGC(H);
  f = H->last_result;
  status = luaU_decompile(H, f, w, data);
  lua_unlock(H);
  return status;
}
#endif /* HKSC_DECOMPILER */

/* print a function */
LUA_API void lua_print (hksc_State *H, int full) {
  const Proto *f;
  lua_lock(H);
  api_check(H, H->last_result != NULL);
  luaC_checkGC(H);
  f = H->last_result;
  luaU_print(f, full);
  lua_unlock(H);
}

