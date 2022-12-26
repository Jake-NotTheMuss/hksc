/*
** $Id: lfunc.c $
** Auxiliary functions to manipulate prototypes and closures
** See Copyright Notice in lua.h
*/


#include <stddef.h>
#include <string.h> /* memcpy */

#define lfunc_c
#define LUA_CORE

#include "lua.h"

#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"



Proto *luaF_newproto (hksc_State *H) {
  Proto *f = luaM_new(H, Proto);
  luaC_link(H, obj2gco(f), LUA_TPROTO);
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
  f->name = NULL;
#ifdef LUA_COD
  f->hash = 0;
#endif /* LUA_COD */
  return f;
}


void luaF_freeproto (hksc_State *H, Proto *f) {
  luaM_freearray(H, f->code, f->sizecode, Instruction);
  luaM_freearray(H, f->p, f->sizep, Proto *);
  luaM_freearray(H, f->k, f->sizek, TValue);
  luaM_freearray(H, f->lineinfo, f->sizelineinfo, int);
  luaM_freearray(H, f->locvars, f->sizelocvars, struct LocVar);
  luaM_freearray(H, f->upvalues, f->sizeupvalues, TString *);
  luaM_free(H, f);
}


#ifdef LUA_COD
void luaF_hash (hksc_State *H, Proto *f) {
  char *str;
  size_t len = f->source->tsv.len;
  lua_assert(len != 0);
  if (f->name)
    len += f->name->tsv.len;
  str = luaM_newvector(H, len, char);
  memcpy(str, getstr(f->source), f->source->tsv.len);
  if (f->name)
    memcpy(str+f->source->tsv.len, getstr(f->name), f->name->tsv.len);
  f->hash = luaS_cod_hashstring(H,str,len);
  luaM_freearray(H, str, len, char);
}
#endif /* LUA_COD */


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

