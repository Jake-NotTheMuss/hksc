/*
** $Id: ldebug.c $
** Debug Interface
** See Copyright Notice in lua.h
*/


#include <stdarg.h>
#include <stddef.h>
#include <string.h>

#define ldebug_c
#define LUA_CORE

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"



void luaG_runerror (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  luaD_setvfmsg(H, fmt, argp);
  va_end(argp);
  luaD_throw(H, LUA_ERRRUN);
}

