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
#include "lvec.h"



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


LUA_API const char *lua_geterror (hksc_State *H) {
  const char *msg;
  lua_lock(H);
  msg = H->errormsg;
  lua_unlock(H);
  return msg;
}


LUA_API void lua_seterror (hksc_State *H, const char *msg) {
  lua_lock(H);
  if (msg)
    msg = getstr(luaS_new(H, msg));
  H->errormsg = msg;
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


#ifdef LUA_DEBUG
LUA_API void lua_setlogfile (hksc_State *H, void *logfile) {
  lua_lock(H);
  G(H)->logfile = logfile;
  lua_unlock(H);
}
#endif /* LUA_DEBUG */


LUA_API void lua_addprefixmap (hksc_State *H, const char *arg) {
  const char *eq;  /* equals sign in ARG */
  lua_lock(H);
  eq = strchr(arg, '=');
  if (eq != NULL) {
    int i;
    TString *from = luaS_newlstr(H, arg, cast(size_t, eq - arg));
    TString *to = luaS_new(H, eq+1);
    for (i = 0; i < G(H)->prefixmaps.used; i++)
      if (G(H)->prefixmaps.s[i].from == from && G(H)->prefixmaps.s[i].to == to)
        break;
    if (i >= G(H)->prefixmaps.used) {
      FilePrefixMap *map = VEC_NEWELT(H, G(H)->prefixmaps);
      map->from = from;
      map->to = to;
    }
  }
  lua_unlock(H);
}


LUA_API void lua_removeprefixmap (hksc_State *H, const char *arg) {
  const char *eq;  /* equals sign in ARG */
  lua_lock(H);
  eq = strchr(arg, '=');
  if (eq != NULL) {
    int i;
    size_t from_len = cast(size_t, eq - arg);
    const char *from = arg, *to = eq + 1;
    for (i = 0; i < G(H)->prefixmaps.used; i++) {
      FilePrefixMap *map = &G(H)->prefixmaps.s[i];
      if (map->from->tsv.len == from_len &&
          strncmp(getstr(map->from), from, from_len) == 0 &&
          strcmp(getstr(map->to), to) == 0) {
        int j;
        /* remove this entry */
        for (j = i+1; j < G(H)->prefixmaps.used; j++)
          G(H)->prefixmaps.s[j-1] = G(H)->prefixmaps.s[j];
        G(H)->prefixmaps.used--;
        /* continue in case there are duplicates in the array */
      }
    }
  }
  lua_unlock(H);
}


/*
** compiler/decompiler settings (C -> stack)
*/

#define returnfield(s) \
  int s; lua_lock(H); s = Settings(H).s; lua_unlock(H); return s
#define setfield(s) lua_lock(H); Settings(H).s = s; lua_unlock(H)

LUA_API int lua_getemitstruct (hksc_State *H) {
  returnfield(emit_struct);
}

LUA_API void lua_setemitstruct (hksc_State *H, int emit_struct) {
  setfield(emit_struct);
}

LUA_API int lua_getliteralsenabled (hksc_State *H) {
  returnfield(literals);
}

LUA_API void lua_setliteralsenabled (hksc_State *H, int literals) {
  setfield(literals);
}

LUA_API int lua_getstrip (hksc_State *H) {
  returnfield(strip);
}

LUA_API void lua_setstrip (hksc_State *H, int strip) {
  setfield(strip);
}

LUA_API int lua_getignoredebug (hksc_State *H) {
  returnfield(ignore_debug);
}

LUA_API void lua_setignoredebug (hksc_State *H, int ignore_debug) {
  setfield(ignore_debug);
}

#ifdef HKSC_DECOMPILER

LUA_API int lua_getmatchlineinfo (hksc_State *H) {
  returnfield(match_line_info);
}

LUA_API void lua_setmatchlineinfo (hksc_State *H, int match_line_info) {
  setfield(match_line_info);
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
LUA_API int lua_print (hksc_State *H, lua_Writer w, void *data, int full) {
  int status;
  const Proto *f;
  lua_lock(H);
  api_check(H, H->last_result != NULL);
  luaC_checkGC(H);
  f = H->last_result;
  status = luaU_print(H, f, w, data, full);
  lua_unlock(H);
  return status;
}

