/*
** $Id: lfunc.c,v 2.11 2005/05/05 20:47:02 roberto Exp roberto $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#include "hksc_begin_code.h"

#define lfunc_c
#define LUA_CORE

#include "lua.h"

#include "lfunc.h"
/*#include "lgc.h"*/
#include "lmem.h"
#include "lobject.h"
/*#include "lstate.h"*/



Closure *luaF_newCclosure (int nelems, Table *e) {
  Closure *c = cast(Closure *, luaM_malloc(sizeCclosure(nelems)));
  /*luaC_link(L, obj2gco(c), LUA_TFUNCTION);*/
  c->c.isC = 1;
  c->c.env = e;
  c->c.nupvalues = cast_byte(nelems);
  return c;
}


Closure *luaF_newLclosure (int nelems, Table *e) {
  Closure *c = cast(Closure *, luaM_malloc(sizeLclosure(nelems)));
  /*luaC_link(L, obj2gco(c), LUA_TFUNCTION);*/
  c->l.isC = 0;
  c->l.env = e;
  c->l.nupvalues = cast_byte(nelems);
  while (nelems--) c->l.upvals[nelems] = NULL;
  return c;
}


UpVal *luaF_newupval () {
  UpVal *uv = luaM_new(UpVal);
  /*luaC_link(L, obj2gco(uv), LUA_TUPVAL);*/
  uv->v = &uv->u.value;
  setnilvalue(uv->v);
  return uv;
}


UpVal *luaF_findupval (StkId level) {
  return NULL;
}


static void unlinkupval (UpVal *uv) {
  lua_assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  uv->u.l.next->u.l.prev = uv->u.l.prev;  /* remove from `uvhead' list */
  uv->u.l.prev->u.l.next = uv->u.l.next;
}


void luaF_freeupval (UpVal *uv) {

}


void luaF_close (StkId level) {

}


Proto *luaF_newproto () {
  Proto *f = luaM_new(Proto);
  /*luaC_link(L, obj2gco(f), LUA_TPROTO);*/
  f->k = NULL;
  f->sizek = 0;
  f->p = NULL;
  f->sizep = 0;
  f->code = NULL;
  f->sizecode = 0;
  f->sizelineinfo = 0;
  f->sizeupvalues = 0;
  f->nups = 0;
  f->upvalues = NULL;
  f->numparams = 0;
  f->is_vararg = 0;
  f->maxstacksize = 0;
  f->lineinfo = NULL;
  f->sizelocvars = 0;
  f->locvars = NULL;
  f->linedefined = 0;
  f->lastlinedefined = 0;
  f->source = NULL;
  return f;
}


void luaF_freeproto (Proto *f) {
  luaM_freearray(f->code, f->sizecode, Instruction);
  luaM_freearray(f->p, f->sizep, Proto *);
  luaM_freearray(f->k, f->sizek, TValue);
  luaM_freearray(f->lineinfo, f->sizelineinfo, int);
  luaM_freearray(f->locvars, f->sizelocvars, struct LocVar);
  luaM_freearray(f->upvalues, f->sizeupvalues, TString *);
  luaM_free(f);
}


void luaF_freeclosure (Closure *c) {
  int size = (c->c.isC) ? sizeCclosure(c->c.nupvalues) :
                          sizeLclosure(c->l.nupvalues);
  luaM_freemem(c, size);
}


/*
** Look for n-th local variable at line `line' in function `func'.
** Returns NULL if not found.
*/
const char *luaF_getlocalname (const Proto *f, int local_number, int pc) {
  int i;
  for (i = 0; i<f->sizelocvars && f->locvars[i].startpc <= pc; i++) {
    if (pc < f->locvars[i].endpc) {  /* is variable active? */
      local_number--;
      if (local_number == 0)
        return getstr(f->locvars[i].varname);
    }
  }
  return NULL;  /* not found */
}

