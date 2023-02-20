/*
** $Id: ldo.c $
** Stack and Call structure of Lua
** See Copyright Notice in lua.h
*/


#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#define ldo_c
#define LUA_CORE

#include "hksclua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"

#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"

#include "lundump.h"

#include "lzio.h"




/*
** {======================================================
** Error-recovery functions
** =======================================================
*/


/* chain list of long jump buffers */
struct lua_longjmp {
  struct lua_longjmp *previous;
  luai_jmpbuf b;
  volatile int status;  /* error code */
};

void luaD_setvferror (hksc_State *H, const char *fmt, va_list argp)
{
  hksc_seterror(H, luaO_pushvfstring(H, fmt, argp));
}


void luaD_setferror (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  luaD_setvferror(H, fmt, argp);
  va_end(argp);
}


void luaD_seterrorobj (hksc_State *H, int errcode) {
  switch (errcode) {
    case LUA_ERRMEM: {
      hksc_seterror(H, getstr(luaS_newliteral(H, MEMERRMSG)));
      break;
    }
    case LUA_ERRERR: {
      hksc_seterror(H,
        getstr(luaS_newliteral(H, "error in error handling")));
      break;
    }
    case LUA_ERRSYNTAX:
    case LUA_ERRRUN: {
      break; /* error message was already set, do nothing */
    }
  }
}

void luaD_throw (hksc_State *H, int errcode) {
  if (H->errorJmp) {
    H->errorJmp->status = errcode;
    LUAI_THROW(H, H->errorJmp);
  }
  else {
    H->status = cast_byte(errcode);
    if (G(H)->panic) {
      lua_unlock(H);
      G(H)->panic(H);
    }
    exit(EXIT_FAILURE);
  }
}

int luaD_rawrunprotected (hksc_State *H, Pfunc f, void *ud) {
  struct lua_longjmp lj;
  lj.status = 0;
  lj.previous = H->errorJmp;  /* chain new error handler */
  H->errorJmp = &lj;
  LUAI_TRY(H, &lj,
    ((*f)(H, ud));
  );
  H->errorJmp = lj.previous;  /* restore old error handler */
  return lj.status;
}


int luaD_pcall (hksc_State *H, Pfunc func, void *u) {
  int status;
  unsigned short oldnCcalls = H->nCcalls;
  /*ptrdiff_t old_errfunc = H->errfunc;*/
  status = luaD_rawrunprotected(H, func, u);
  if (status != 0) {
    H->nCcalls = oldnCcalls;
    luaD_seterrorobj(H, status);
  }
  return status;
}

/*
** Execute a protected parser.
*/
struct SParser {  /* data to `f_parser' */
  ZIO *z;
  Mbuffer buff;  /* buffer to be used by the scanner */
  const char *name;
};

static void f_parser (hksc_State *H, void *ud) {
  Proto *tf;
  struct SParser *p = cast(struct SParser *, ud);
  int c = luaZ_lookahead(p->z);
  if (c == LUA_SIGNATURE[0]) { /* binary file */
    lua_assert(hksc_mode(H) == HKSC_MODE_BINARY);
    tf = luaU_undump(H, p->z, &p->buff, p->name);
  } else {
    lua_assert(hksc_mode(H) == HKSC_MODE_SOURCE);
    tf = luaY_parser(H, p->z, &p->buff, p->name);
  }
  /*tf = luaY_parser(H, p->z, &p->buff, p->name);*/
  lua_assert(H->last_result == NULL);
  H->last_result = tf;
}

int luaD_protectedparser (hksc_State *H, ZIO *z, const char *name) {
  struct SParser p;
  int status;
  p.z = z; p.name = name;
  luaZ_initbuffer(H, &p.buff);
  status = luaD_pcall(H, f_parser, &p);
  luaZ_freebuffer(H, &p.buff);
  return (status);
}

