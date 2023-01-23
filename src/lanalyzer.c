/*
** $Id: lanalyzer.c $
** Auxiliary functions to manipulate prototype analyzer structures
** See Copyright Notice in lua.h
*/


#include <stddef.h>

#define lanalyzer_c
#define LUA_CORE

#include "hksclua.h"

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
  a->lineinfo = NULL;
  a->sizelineinfo = 0;
  a->locvars = NULL;
  a->sizelocvars = 0;
  a->upvalues = NULL;
  a->sizeupvalues = 0;
  a->bbllist.first = a->bbllist.last = NULL;
  return a;
}


void luaA_freeanalyzer (hksc_State *H, Analyzer *a) {
  struct BasicBlock *bbl;
  luaM_freearray(H, a->insproperties, a->sizeinsproperties, InstructionFlags);
  luaM_freearray(H, a->regproperties, a->sizeregproperties, RegisterFlags);
  luaM_freearray(H, a->lineinfo, a->sizelineinfo, int);
  luaM_freearray(H, a->locvars, a->sizelocvars, struct LocVar);
  luaM_freearray(H, a->upvalues, a->sizeupvalues, TString *);
  bbl = a->bbllist.first;
  while (bbl != NULL) {
    struct BasicBlock *next = bbl->next;
    luaM_free(H, bbl);
    bbl = next;
  }
  luaM_free(H, a);
}

#endif /* HKSC_DECOMPILER */
