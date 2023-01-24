/*
** $Id: llog.c $
** Logging information and warnings
** See Copyright Notice in lua.h
*/

#include <stdarg.h>

#define llog_c
#define LUA_CORE

#include "hksclua.h"

#include "llog.h"
#include "lobject.h"
#include "lstate.h"

#ifdef HKSC_LOGGING

#define DEFLOGCATEGORY(e,n) n,
static const char *const luaI_categories [] = {
  LOG_CATEGORY_TABLE
  NULL
};
#undef DEFLOGCATEGORY


void luaI_log(hksc_State *H, int category, int priority, const char *msg) {
  hksc_LogContext *ctx;
  lua_assert(msg != NULL);
  lua_assert(category < LOG_CATEGORY_MAX && category >= 0);
  ctx = &G(H)->logctx;
  if (ctx->f != NULL && priority >= ctx->priority) {
    lua_unlock(H);
    (*ctx->f) (H, luaI_categories[category], priority, msg, ctx->ud);
    lua_lock(H);
  }
}

const char *luaI_formatmsg(hksc_State *H, const char *fmt, ...) {
  const char *str;
  va_list argp;
  va_start(argp, fmt);
  str = luaO_pushvfstring(H, fmt, argp);
  va_end(argp);
  return str;
}

#endif /* HKSC_LOGGING */
