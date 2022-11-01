/*
** $Id: ldebug.c,v 2.28 2005/11/01 16:08:52 roberto Exp roberto $
** Debug Interface
** See Copyright Notice in lua.h
*/


#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include "hksc_begin_code.h"

#define ldebug_c
#define LUA_CORE

#include "lua.h"


#include "lcode.h"
#include "ldebug.h"
#include "lerror.h"
#include "lfunc.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"


#if 0
void luaG_errormsg (hksc_State *H) {
  if (H->errfunc != 0) {  /* is there an error handling function? */
    StkId errfunc = restorestack(H, H->errfunc);
    if (!ttisfunction(errfunc)) luaD_throw(H, LUA_ERRERR);
    setobjs2s(H, H->top, H->top - 1);  /* move argument */
    setobjs2s(H, H->top - 1, errfunc);  /* push function */
    incr_top(H);
    luaD_call(H, H->top - 2, 1);  /* call it */
  }
  luaD_throw(H, LUA_ERRRUN);
}
#endif

void luaG_runerror (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  hksc_setvfmsg(H, fmt, argp);
  va_end(argp);
  luaD_throw(H, LUA_ERRRUN);
}

