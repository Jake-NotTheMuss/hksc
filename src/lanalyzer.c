/*
** $Id: lanalyzer.c $
** Auxiliary functions to manipulate prototype analyzer structures
** See Copyright Notice in lua.h
*/


#include <stddef.h>
#include <string.h> /* memset */

#define lanalyzer_c
#define LUA_CORE

#include "hksclua.h"

#include "lanalyzer.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "lzio.h"

#ifdef HKSC_DECOMPILER

Analyzer *luaA_newanalyzer (hksc_State *H) {
  Analyzer *a = luaM_new(H, Analyzer);
  luaC_link(H, obj2gco(a), LUA_TANALYZER);
  a->insproperties = NULL;
  a->sizeinsproperties = 0;
  a->opencalls = NULL;
  a->sizeopencalls = 0;
  a->regproperties = NULL;
  a->sizeregproperties = 0;
  a->locvars = NULL;
  a->sizelocvars = 0;
  a->upvalues = NULL;
  a->sizeupvalues = 0;
  a->actvar = NULL;
  a->sizeactvar = 0;
  a->kmap = NULL;
  a->sizekmap = 0;
  a->kmap_1 = 0;
  a->decomppass = 1;  /* starts on the first pass */
  a->bllist.first = a->bllist.last = NULL;
  a->pendingstk.u.s1 = NULL;
  a->pendingstk.u.s2 = NULL;
  a->pendingstk.total = a->pendingstk.used = 0;
  return a;
}


void luaA_allockmap (hksc_State *H, Analyzer *a, int nk) {
  int numblocks = (nk + 31) >> 5;
  if (numblocks > 1) {
    a->kmap = luaM_newvector(H, numblocks, lu_int32);
    memset(a->kmap, 0, sizeof(lu_int32) * a->sizekmap);
  }
  else if (numblocks == 1) {
    a->kmap = &a->kmap_1;
    a->kmap_1 = 0;
  }
  a->sizekmap = numblocks;
}


void luaA_freekmap (hksc_State *H, Analyzer *a) {
  if (a->sizekmap > 1) {
    lua_assert(a->kmap != &a->kmap_1);
    luaM_freearray(H, a->kmap, a->sizekmap, lu_int32);
  }
  a->kmap = NULL;
  a->sizekmap = 0;
}


void luaA_freeanalyzer (hksc_State *H, Analyzer *a) {
  struct BlockNode *bn;
  luaM_freearray(H, a->insproperties, a->sizeinsproperties, InstructionFlags);
  luaM_freearray(H, a->opencalls, a->sizeopencalls, struct OpenExpr);
  luaM_freearray(H, a->regproperties, a->sizeregproperties, SlotDesc);
  luaM_freearray(H, a->locvars, a->sizelocvars, struct LocVar);
  luaM_freearray(H, a->upvalues, a->sizeupvalues, TString *);
  luaM_freearray(H, a->actvar, a->sizeactvar, unsigned short);
  luaA_freekmap(H, a);
  bn = a->bllist.first;
  while (bn != NULL) {
    struct BlockNode *next = bn->next;
    luaM_free(H, bn);
    bn = next;
  }
  if (a->decomppass == 1)
    luaM_freearray(H, a->pendingstk.u.s1, a->pendingstk.total, BlockState2);
  else
    luaM_freearray(H, a->pendingstk.u.s2, a->pendingstk.total, ExpNode);
  luaM_free(H, a);
}


#endif /* HKSC_DECOMPILER */
