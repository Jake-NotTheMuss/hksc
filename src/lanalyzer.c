/*
** $Id: lanalyzer.c $
** Auxiliary functions to manipulate prototype analyzer structures
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#define lanalyzer_c
#define LUA_CORE

#include "lua.h"

#include "lanalyzer.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"

#ifdef HKSC_DECOMPILER

Analyzer *luaA_newanalyzer (hksc_State *H) {
  Analyzer *a = luaM_new(H, Analyzer);
  luaC_link(H, obj2gco(a), LUA_TANALYZER);
  a->insproperties = NULL;
  a->sizeinsproperties = 0;
  a->regproperties = NULL;
  a->sizeregproperties = 0;
  a->locvars = NULL;
  a->sizelocvars = 0;
  a->bbldata = NULL;
  a->sizebbldata = 0;
  return a;
}


void luaA_freeanalyzer (hksc_State *H, Analyzer *a) {
  int i;
  luaM_freearray(H, a->insproperties, a->sizeinsproperties, InstructionFlags);
  luaM_freearray(H, a->regproperties, a->sizeregproperties, RegisterFlags);
  luaM_freearray(H, a->locvars, a->sizelocvars, struct LocVar);
  luaM_freearray(H, a->bbldata, a->sizebbldata, struct BBLStart);
  luaM_free(H, a);
}

#endif /* HKSC_DECOMPILER */
