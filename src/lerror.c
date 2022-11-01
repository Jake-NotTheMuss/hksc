/*
** $Id: ldo.c,v 2.36 2005/10/23 17:52:42 roberto Exp roberto $
** Stack and Call structure of Lua
** See Copyright Notice in lua.h
*/


#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#define lerror_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "lerror.h"
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

void hksc_luaD_setvfmsg (hksc_State *H, const char *fmt, va_list argp)
{
  char buf[512];
  vsnprintf(buf, sizeof(buf), fmt, argp);
  va_end(argp);
  buf[sizeof(buf) - 1] = '\0';
  luaE_seterrormsg(H, getstr(luaS_new(H, buf)));
}

void hksc_luaD_setfmsg (hksc_State *H, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  hksc_luaD_setvfmsg(H, fmt, argp);
  va_end(argp);
}

void hksc_luaD_seterrormsg (hksc_State *H, const char *msg) {
  luaE_seterrormsg(H, getstr(luaS_new(H, msg)));
}


void hksc_luaD_seterrorobj (hksc_State *H, int errcode) {
  switch (errcode) {
    case LUA_ERRMEM: {
      luaE_seterrormsg(H, getstr(luaS_newliteral(H, MEMERRMSG)));
      break;
    }
    case LUA_ERRERR: {
      luaE_seterrormsg(H,
        getstr(luaS_newliteral(H, "error in error handling")));
      break;
    }
    case LUA_ERRSYNTAX:
    case LUA_ERRRUN: {
      /* error message was already set, do nothing */
      break;
    }
  }
}

void hksc_luaD_throw (hksc_State *H, int errcode) {
  if (H->errorJmp) {
    H->errorJmp->status = errcode;
    LUAI_THROW(H, H->errorJmp);
  }
  else {
    H->status = cast_byte(errcode);
    if (G(H)->panic) {
      /*lua_unlock(H);*/
      G(H)->panic(H);
    }
    exit(EXIT_FAILURE);
  }
}

int hksc_luaD_rawrunprotected (hksc_State *H, Pfunc f, void *ud) {
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


int hksc_luaD_pcall (hksc_State *H, Pfunc func, void *u) {
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
  Proto *f;
};
#if 1
static void f_parser (hksc_State *H, void *ud) {
  int i;
  Proto *tf;
  Closure *cl;
  struct SParser *p = cast(struct SParser *, ud);
  tf = hksc_parser(H, p->z, &p->buff, p->name);
  p->f = tf;
#if 0
  int c = luaZ_lookahead(p->z);
  tf = ((c == LUA_SIGNATURE[0]) ? luaU_undump : luaY_parser)(H, p->z,
                                                             &p->buff, p->name);
  cl = luaF_newLclosure(H, tf->nups, hvalue(gt(H)));
  cl->l.p = tf;
  for (i = 0; i < tf->nups; i++)  /* initialize eventual upvalues */
    cl->l.upvals[i] = luaF_newupval(H);
  setclvalue(H, H->top, cl);
  incr_top(H);
#endif
}

int hksc_luaD_protectedparser (hksc_State *H, ZIO *z, const char *name,
                               Proto **pf) {
  struct SParser p;
  int status;
  p.z = z; p.name = name;
  luaZ_initbuffer(H, &p.buff);
  status = luaD_pcall(H, f_parser, &p);
  luaZ_freebuffer(H, &p.buff);
  *pf = p.f;
  return (status);
}
#endif
